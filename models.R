# Structure
# 1. Functions to estimate population level effect
# 2. Functions to estimate models


# Functions to estimate population level effect --------------------

birth_mean <- function(data) {
  data <- as.data.table(data)
  data <- data[, .(birth = max(birth)), by = id]
  return(mean(data$birth, na.rm = T))
}

cum_exp <- function(data) {
  data <- as.data.table(data)
  data <- data[order(id, age), ]
  data <- data[, .(birth = survival_cum(birth)), by = id]
  return(mean(data$birth, na.rm = T) * 100)
}

survival_cum <- function(x) {
  lx <- 100000
  for (prob in x) {
    lx <- (1-prob) * lx
  }
  return(round(lx/100000, 4))
}

counter_dts <- function(data, replacement, indicator) {
  data[data[[indicator]] < replacement, indicator] <- replacement
  return(data)
}

counter <- function(data, replacement, indicator) {
  data <- as.data.frame(data)
  data$availability <- as.numeric(data[, indicator])
  data$availability[data$availability < replacement] <- replacement
  return(data)
}

# Function to estimate population effects for logistic models
pop_effect_log <- function(model, replacement, indicator) {
  # Estimate the predicted level of childlessness
  ch_pred <- mean(model$fitted.values, na.rm = T) * 100
  # Estimate the counterfactual
  dta <- counter(model$data, replacement = 0, indicator)
  ch_count <- mean(predict.glm(model, dta, type = "response"), na.rm = T) * 100
  # Estimate the population level effect
  pop_effect <- (ch_count - ch_pred)
  pop_effect <- list("observed" = mean(model$data$childless) * 100,
                     "model" = ch_pred,
                     "counterfactual" = ch_count, 
                     "effect" = pop_effect, 
                     variable = indicator)
  return(pop_effect)
}


# Function to estimate population level effect for fixed effects models
pop_effect_feols <- function(model, rep, indicator = "ar_near") {
  mod <- model
  # Estimate the predicted level of childlessness
  dta2 <- as.data.frame(d3)
  dta2$availability <- dta2[, indicator]
  dta2$birth <- predict(mod, dta2, type = "response")
  ch_pred <- mean(dta2$birth, na.rm = T) * 100
  # Estimate the counterfactual scenario
  dta3 <- counter(dta2, rep, indicator)
  dta3$birth <- predict(mod, dta3, type = "response")
  ch_count <- mean(dta3$birth, na.rm = T) * 100
  # Estimate the population level effect
  pop_effect <- (ch_count - ch_pred)
  pop_effect <- list("observed" = ch_pred, "counterfactual" = ch_count, "effect" = pop_effect, variable = indicator)
  return(pop_effect)
}

# Function to estimate population level effect for survival models
pop_effect_surv <- function(model, data = d1, rep = 0, indicator = "ar_near") {
  mod <- model
  # Estimate the predicted level of childlessness
  data$birth <- predict(mod, data, type = "response")
  data$id <- data[, 1]
  ch_pred <- cum_exp(data)
  # Estimate the counterfactual
  tmp <- data
  tmp <- counter_dts(tmp, rep, indicator = indicator)
  tmp$birth <- predict(mod, tmp, type = "response")
  ch_count <- cum_exp(tmp)
  # Estimate the population level effect
  pop_effect <- (ch_count - ch_pred)
  pop_effect <- list("model" = ch_pred, "counterfactual" = ch_count, "effect" = pop_effect, variable = indicator)
  return(pop_effect)
}

# Function to estimate population effects for matching models
pop_effect_match <- function(model) {
  mod <- model
  # Estimate the predicted level of childlessness
  ch_pred <- mean(mod$fitted.values) * 100
  # Estimate the counterfactual
  dta <- mod$data
  dta <- dta %>% mutate(high_availability = 1)
  ch_count <- mean(predict(mod, dta, type = "response"), na.rm = T) * 100
  # Estimate the population level effect
  pop_effect <- (ch_count - ch_pred)
  pop_effect <- list("observed" = ch_pred, "counterfactual" = ch_count, "effect" = pop_effect)
  return(pop_effect)
}


# Model fit ---------------------------------------------

# Model fit for cumulative logistic regression
model_fit_log <- function (model) {
  mod <- model
  # Estimate the average childlessness in the data
  ch_obs <- mean(mod$data$childless)
  # Estimate the predicted level of childlessness
  ch_pred <- mean(mod$fitted.values)
  ch_pred - ch_obs
  # Estimate the model fit
  model_fit <- (ch_pred - ch_obs) * 100
  return(model_fit)
}

# Model fit for discrete-time-survival model
model_fit_surv <- function (model) {
  mod <- model
  # Estimate the average childlessness in the data
  ch_obs <- cum_exp(mod$data)
  # Estimate the predicted level of childlessness
  dta2 <- mod$data
  dta2$birth <- mod$fitted.values
  ch_pred <- cum_exp(dta2)
  # Estimate the model fit
  model_fit <- 100 * (ch_pred - ch_obs)
  return(model_fit)
}


# Function to estimate population effects for logistic models
model_fit_match <- function(model) {
  mod <- model
  # Estimate the average childlessness in the data
  ch_obs <- mean(mod$model$childless)
  # Estimate the predicted level of childlessness
  ch_pred <- mean(mod$fitted.values)
  # Estimate the population level effect
  model_fit <- (ch_pred - ch_obs) * 100
  return(model_fit)
}

# 2. Estimate models --------------------------------------------------

sibling_fe <- function(indicator = "ar_near_9.2km", data = d3) {
  
  # Make it to data.frame
  data <- as.data.frame(data)
  
  # Rename the variable
  data$availability <- unlist(data[, indicator])
  
  # Estimate the linear probability model
  ms1 <- feols(fml = as.formula('childless ~ availability | sib'), data = data)
  
  # Including covariates
  ms2 <- feols(fml = as.formula('childless ~ availability + inc_quant +  unemployed + factor(bo) + edu| sib'), data = data)
  
  # Including contextual covariates
  ms3 <- feols(fml = as.formula('childless ~ availability + inc_quant+  unemployed +  factor(bo)  + educ_comp + unemployment +  edu | sib'), data = data)
  
  # Interaction
  ms4 <- feols(fml = as.formula('childless ~ availability + availability:inc_quant + factor(bo) +  unemployed  + educ_comp + unemployment | sib'), data = data)
  
  # Create teh model output
  etable(ms1, ms2, ms3, ms4,  tex = T, title = "Sibling FE on childlessness",
         fixef_sizes = T, file = paste0("Results/Models/fe_sibling_", indicator, ".tex"),
         ci = 0.9, fitstat = c('n', 'r2', 'aic', 'bic'))
  
  # Estimate the population level effecty
  models <- list(ms3, ms4)
  pop_effect <- map_df(models, pop_effect_feols, 0, indicator)
  pop_effect$specification <- c("linear", "interactive")
  return(pop_effect)
}

cum_logistic <- function(indicator, data = d2) {

  # Make it to data.frame
  data <- as.data.frame(data)
  
  # Create data
  data$availability <- data[, indicator]
  
  # Generate the regression equations
  logistic_equations <- gen_equations("availability")
  
  # Estimate the models
  logistic_models <- map(logistic_equations, glm, data = data, family = "binomial")
  
  # Estimate the population level effect
  pop_effect <- map_df(logistic_models[3:4], pop_effect_log, replacement = 0, indicator = indicator)
  pop_effect$specification <- c("linear", "interactive")
  
  return(pop_effect)
}

matching_fe <- function(indicator = "ar", data){
  
  # Create a binary treatment variable
  data$high_availability <- ifelse(data[, indicator] >= 0, 1, 0)

  # Match on siblings
  match1 <- matchit(high_availability ~ edu + inc_quant, data = data, exact = c("edu", "inc_quant"))

  ## Get the matched data
  md1 <- match.data(match1)
  md1$high_availability <- as.numeric(md1$high_availability)
  
  # Run the regression
  match_reg1 <- glm(childless ~ high_availability + edu + inc_quant, data = md1, family = "binomial")
  match_reg2 <- glm(childless ~ high_availability + cohort + edu + urban + edu + inc_quant , data = md1, family = "binomial")
  match_reg3 <- glm(childless ~ high_availability + cohort + edu + educ_comp + inequality + unemployment + urban + edu + inc_quant, data = md1, family = "binomial")
  match_reg4 <- glm(childless ~ high_availability + high_availability:inc_quant + cohort + educ_comp + inequality + unemployment + urban + edu + inc_quant, data = md1, family = "binomial")
  
  # Create a regression table
  stargazer(match_reg1, match_reg2, match_reg3, match_reg4,
            title = "Linear regression on matched data for childlessness",
            out = paste0("Results/Models/matching_reg_",indicator, ".tex"),
            align = T, no.space = T, omit.stat = c("f", "LL", "ser"), ci = TRUE, ci.level = 0.95)
  
  # Estimate the population level effecty
  models <- list(match_reg3, match_reg3)
  pop_effect <- map_df(models, pop_effect_match)
  pop_effect$specification <- c("linear", "interactive")
  pop_effect$variable <- indicator
  return(pop_effect)
}


# Function to generate regressin equations -----------------


gen_equations <- function(indicator, eha = F) {
  
  # Create the regression equations
  reg1 <- paste("childless ~",  indicator)
  if (eha)   reg1 <- paste("birth ~",  indicator, "+ age + age2")
  reg2 <- paste(reg1, "+ cohort + inc_quant + unemployed + edu + urban")
  if (eha)   reg2 <- paste(reg1, "+ cohort + inc_quant + act + edu + urban")
  reg3 <- paste(reg2, "+ educ_comp + unemployment + inequality")
  if (eha)   reg3 <- paste(reg2, "+ share_tertiary + unempl_rate + poverty")
  reg4 <- paste(reg3, paste0("+ ", indicator, ":inc_quant"))
  
  # Return a list
  return(c(reg1, reg2, reg3, reg4))

}
