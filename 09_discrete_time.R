##################################################
#         Birth squeezes in Finland              #
#         Henrik-Alexander Schubert              #
#                   Analysis                     #
##################################################

#####   Settings  ##########################

rm(list = ls())

# Load the packages
library(data.table)
library(tidyverse)
library(dtplyr)
library(stargazer)
library(broom)

# Load the functions
source("functions/functions.R")
source("functions/models.R")

# Time lag
time_lag <- 1:3

# Assemble the names of the variables
indicators <- c("sr_near_9.2km", "pr_near_9.2km", "ar_near_9.2km", "sr", "pr", "ar")
specifications <- vector("list", length = length(indicators))
names(specifications) <- indicators
model_spec <- model <- vector("list", length = length(time_lag))
names(model_spec) <- names(model) <- time_lag

### Estimate the models ----------------------------

for (lag in time_lag) {
    
    cat("Time-lag:", lag)
  
  # Load the data    
  path <- paste0("data/fixed_effects_", lag, ".Rda")
  load(path)
  
  # Create age squared
  d1$age2 <- d1$age * d1$age
  
  # Create last point of observation
  dta2 <- d1[, .(maxage = max(age), start = age - 1),  by = id]
  
  # Create last point of observation
  d1 <- bind_cols(d1, dta2)
  
  # Make the data as data.frame
  d1 <- na.omit(as.data.frame(d1))
    
    for (indicator in indicators) {
      
      # Create the indicator
      d1[, indicator] <- log(d1[, indicator])
      
      # Make equations
      equations <- gen_equations(indicator, eha = T)
      
      # Estimate the logit models
      for (i in 1:length(equations)) {
        tmp <- glm(equations[i], data = d1, family = binomial(link = logit))
        assign(paste0("dts", i), tmp)
      }
      
      # Summarise the discrete-time survival regression output
      stargazer(dts1, dts2, dts3, dts4,
                dep.var.labels = "First birth",
                title = paste("Discrete-time survival regression model on giving birth using time lag of", lag , "."),
                out = paste0("Results/Models/discrete_survival_", indicator, lag, ".tex"),
                align = T, no.space = T, omit.stat = c("f", "LL", "ser"), 
                ci = TRUE, ci.level = 0.9, type = "text")
      
      # Create the results
      models <- list("linear" = dts3, "interactive" = dts4)
      pop_effect <- map(models, pop_effect_surv, rep = 0, indicator = indicator)
      pop_effect <- bind_rows(pop_effect, .id = "specification")
      pop_effect$lag <- lag
      assign(paste("popeffect", indicator, lag, sep = "_"), pop_effect)
      
      # Create the model coefficients
      specifications[[indicator]] <- map_df(models, tidy, conf.int = TRUE, .id = "model")

      }

  # Filter the result
  result <- bind_rows(specifications, .id = "indicator")
  
  # Assign the result
  model[[lag]] <- result

} 

### Plot the population level effect -------------------------

# Collect the results
pop_effect <- mget(ls(pattern = "popeffect_"))

# Combine the results
pop_effect <- bind_rows(pop_effect)

# Create a variable for the distance
pop_effect$Model <- "Discrete-time survival Model"

# Save the result
save(pop_effect, file = "results/popeffect_dcsm.Rda")

### Plot the results -----------------------------------------

# Bind the results
result <- bind_rows(model, .id = "lag")  

# Plot the result
result_plot <- result[result$term %in% indicators & result$model != "individual", ]
result_plot$term <- clean_labels(result_plot$term)
result_plot$term <- factor(result_plot$term, levels = c("Availability ratio (near)", 
                                                        "Preference ratio (near)",
                                                        "Sex ratio (near)",
                                                        "Availability ratio",
                                                        "Preference ratio",
                                                        "Sex ratio"),  ordered = T)

# Plot the data
ggplot(result_plot, aes(x = lag, y = estimate, colour = model, group = model)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.4)) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ term) + 
  theme_bw(base_size = 14, base_family = "serif") + 
  ylab("Availability coefficient (95% Confidence Interval)") +
  xlab("Time lag") + 
  coord_flip() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(face ="bold")) +
  scale_colour_manual(values = c(MPIDRorange, MPIDRpurple, MPIDRgreen), name = "Model")
  
ggsave(last_plot(), filename = "figures/odds_dct_childbirth.pdf", height = 15, width = 20, unit = "cm")


### Plot the population level effects ------------------------

# Get the results
effect <- mget(ls(pattern = "^popeffect"))
names(effect) <- str_remove(names(effect), "popeffect_")
names(effect) <- str_remove(names(effect), "_\\d$")
effect <- bind_rows(effect, .id = "indicator")
effect$lag <- factor(effect$lag)
effect$variable <- clean_labels(effect$variable)

# Plot the result
ggplot(effect, aes(x = effect, y = variable, colour = lag, shape = lag)) +
  geom_vline(xintercept = 0) +
  geom_linerange(aes(xmin = 0, xmax = effect), position = position_dodge(width = 0.5), size = 2) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  facet_wrap( ~ specification) +
  scale_colour_brewer(name = "Time lag:", palette = "Set1") +
  scale_shape(name = "Time lag:") +
  ylab("Control variables") +
  xlab("Population level Effect") +
  theme_bw(base_size = 14, base_family = "serif") +
  theme(axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 16),
        legend.position = "bottom")

# Save the plot
ggsave(last_plot(), filename = "figures/pop_effect_survival.pdf", height = 15, width = 25)

### END ######################################################