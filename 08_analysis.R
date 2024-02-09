##################################################
#         Birth squeezes in Finland              #
#         Henrik-Alexander Schubert              #
#                   Analysis                     #
##################################################

rm(list = ls())

# Load the packages
library(data.table)
library(tidyverse)
library(dtplyr)
library(stargazer)
library(ggeffects)
library(fixest)
library(MatchIt)
library(broom)
library(patchwork)
library(ggrepel)
library(marginaleffects)  

# Load the data
load("data/cum_data_1.Rda")

# Load the functions
source("functions/functions.R")
source("functions/models.R")

### Data wrangling -------------------------------

# Rename the mane indicaotr
setnames(d2, "ar_near_9.2km", "availability")

# Create an indicator vecotr
indicators <- c("ar", "pr", "sr")

# What is the partner market distribution by indicator
d2 %>%
  as_tibble() %>% 
  group_by(inc_quant) %>% 
  summarise(count = n(),
            across(indicators, list(mean = mean, lower = function(x) quantile(x, probs = .25), higher =  function(x) quantile(x, probs = .75), count = nrow), .names = "{.col}.{.fn}")) %>% 
  pivot_longer(cols = contains(indicators), names_sep = "\\.", names_to = c("indicator", "measure")) %>% 
  pivot_wider(names_from = "measure", values_from = "value") %>% 
  mutate(indicator = clean_labels(indicator)) %>% 
  ggplot(aes(x = indicator, y = mean, colour = inc_quant)) +
  geom_line(aes(x = indicator, y = mean), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0) +
    geom_point(position = position_dodge(width = 0.5), size = 4) +
    geom_linerange(aes(ymin = lower, ymax = higher), position = position_dodge(width = 0.5), size = 1.1) +
    scale_y_continuous("Average availability indicator") +
    scale_color_brewer(name = "Income quantile: ", palette = "Set1") +
    theme(legend.position = c(0.8, 0.8),
          legend.background = element_rect(colour = "grey", size = 0.3),
          panel.grid.major.x = element_line(colour = "lightgrey"),
          panel.grid.minor.y = element_line(colour = "lightgrey"),
          axis.title.y = element_blank()) +
  coord_flip()

ggsave(last_plot(), filename = "figures/availability_across_incomegroups.pdf", height = 15, width = 25, unit = "cm")

#### 1. Non parametric analysis -----------------------------------

# Create a discrete availability indicator
d2$ar_cat <- ifelse(d2$availability < -0.04431082, "Excess men", 
                    ifelse(d2$availability >= -0.04431082 & d2$availability < 0.22420044, "Balanced",
                           ifelse(d2$availability >= 0.22420044, "Excess females", NA)))

# Order
d2$ar_cat <- factor(d2$ar_cat, levels = c("Excess men", "Balanced", "Excess females") )

# Plot the graph
plot1 <- d2[, .(childless = mean(childless, na.rm = T), count = .N), by = ar_cat]  %>% 
  ggplot(aes(ar_cat, childless, fill = ar_cat)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c(MPIDRpurple, MPIDRgreen, MPIDRyellow), name = "Availability") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  geom_text(aes(label = paste("n =", count/1000, "k")), vjust = -0.5, size = 6, family = "serif") + 
  geom_label(aes(y = 0.05, label = paste(round(childless, 2)*100, "%")),size = 10, colour = "white", fill = "black",  family = "serif") + 
  theme(legend.position = c(0.8, 0.8)) + 
  guides(fill = "none") + 
  ylab("% Childlessness") +
  xlab("Availability by category")

# 2. Cumulative logistic regression -----------------------------------------

# Estimate the logistic regression
clr1 <- glm(childless  ~ availability, data = d2, family = "binomial")

# Estimate the logistic regression
clr2 <- glm(childless  ~ availability + cohort + inc_quant +  unemployed + edu, data = d2, family = "binomial")

# Estimate the logit model with ontextual controls
clr3 <- glm(childless ~ availability + cohort  + inc_quant +  unemployed + edu + educ_comp + unemployment + urban + inequality, data = d2, family = "binomial")

# Estimate the logistic regression
clr4 <- glm(childless ~ availability + inc_quant + availability:inc_quant + edu + cohort +  unemployed  + educ_comp  + unemployment + urban + inequality, data = d2, family = "binomial")

# Summarise logistic regression output
stargazer(clr1, clr2, clr3, clr4, title = paste0("Cumulative logistic regression on childlessness using as predictor variable."), 
          out = paste0("results/Models/main_model.tex"), type = "text",
          align = T, no.space = T, omit.stat = c("f", "ser"), 
          ci = TRUE, ci.level = 0.95)

### Postestimation analysis --------------------------------

# Coutnerfactual prediction
pop_effect_log(clr3, 0, "availability")
pop_effect_log(clr4, 0, "availability")

# Make a classification table
class_table <- d2$prediction <- ifelse(predict(clr4, d2, type = "response") >= 0.5, 1, 0)
print(paste("The share of correctly classified is ", round(mean(d2$childless == d2$prediction), 4)*100, "%."))

# Create the names
prop.table(table("Observed childless" = d2$childless,"Predicted childless" = d2$prediction), 1)

# Prediction plot
prediction <- ggpredict(clr4, terms = c("availability  [all]", "inc_quant", "edu [medium]")) %>% 
  as_tibble()  
ggplot(prediction, aes(x = exp(x), y = predicted, colour = group)) +
    geom_vline(xintercept = 1) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .3) +
    scale_x_continuous("Availability ratio", limits = c(0.8, 1.2), trans = "log", expand = c(0, 0), breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous("Childlessness (predicted)", limits = c(0, .7), labels = scales::percent_format(accuracy = 5L), n.breaks = 6, expand = c(0, 0)) +
    scale_fill_manual(name = "Income quantile", values = MPIDRgradient) +
    scale_colour_manual(name = "Income quantile", values = MPIDRgradient) +
  theme(legend.position = c(0.8, 0.6)) +
  labs(caption = "Prediction based on Model 4. \n Covariate levels: education = basic, cohort = 1960-1970, unemployed = 0 years,\n % tertiary = low, % unemployed = 15")
ggsave(last_plot(), filename = "figures/pred_plot_interaction.pdf", height = 15, width = 25, unit = "cm")

# Prediction at quantiles 
prediction %>% 
  select(x, predicted, group) %>%
  pivot_wider(values_from = predicted, names_from = group) %>% 
  filter(x %in% round(quantile(d2$availability, probs = c(0.25, 0.5, 0.75)), 3))

# Model fit

# Population level effect
models <- mget(ls(pattern = "^clr\\d$"))
map_dbl(models, model_fit_log)
map(models, pop_effect_log, replacement = 0, indicator = "availability")

# 3. Linear probability model --------------------------------------------------------

# Estimate the logistic regression
lpm1 <- lm(childless  ~ availability, data = d2)

# Estimate the logistic regression
lpm2 <- lm(childless  ~ availability + factor(cohort)  + inc_quant +  unemployed + edu + urban, data = d2)

# Estimate the logit model with ontextual controls
lpm3 <- lm(childless ~ availability + factor(cohort)  + inc_quant +  unemployed + edu + educ_comp + unemployment + urban, data = d2)

# Estimate the logistic regression
lpm4 <- lm(childless ~ availability + inc_quant + availability:inc_quant + edu + factor(cohort) +  unemployed  + educ_comp  + unemployment + urban, data = d2)

# Summarise logistic regression output
stargazer(lpm1, lpm2, lpm3, lpm4, title = paste0("Linear probability model on childlessness using the main indicator as predictor variable."), 
          out = paste0("results/Models/cum_linear_probability_main.tex"),
          align = T, no.space = T, omit.stat = c("f", "ser"),  type = "text",
          ci = TRUE, ci.level = 0.95)


# Partnership outcome ------------------------------

# Set the input path
input <- "D:/ready-made/"

# Load the data for 1987-2000
coh1 <- fread(paste0(input, "FOLK_aslii_11a/folk_20112020_tua_aslii21tot_1.csv"), fill = T)

# Load the data for 2001-2010
coh2 <- fread(paste0(input, "FOLK_aslii_0110a/folk_20012010_tua_aslii21tot_1.csv"), fill = T)

# Load the data for 2011-
coh3 <- fread(paste0(input, "FOLK_aslii_8800a/folk_19872000_tua_aslii21tot_1.csv"), fill = T)

# Combine the data
d <- rbindlist(list(coh1, coh2, coh3))
rm(coh1, coh2, coh3)

# Reduce the data
# Variables to be selected
vars <- c("shnro", "vuosi",  "spuhnro", "alku", "loppu", "ymuuttopv",
          "emuuttopv", "vihkipvm", "aeropvm", "pu_kuolpv", "pu_pnro", "pu_jarnro")

# Filter the important variables
d <- d[, ..vars]

# Rename the variables
d <- d[, .(id = shnro,
           year = vuosi,
           pid = spuhnro,
           start = alku,
           last = loppu,
           coh_beg = ymuuttopv,
           coh_end = emuuttopv,
           mar_beg = vihkipvm,
           mar_end = aeropvm,
           coh_nr  = pu_pnro,
           mar_nr  = pu_jarnro)]

# Remove cells which have no information on start
d <- d[!is.na(coh_beg) | !is.na(mar_beg), ]

# Transform the to year
vars <- c("coh_beg", "coh_end", "mar_beg", "mar_end")
d[, (vars) := lapply(.SD, year), .SDcols = (vars)]

# Sort the data
d <- d[order(id, year), ]

# Create marriage and cohabitation indicator
d[, married := ifelse(mar_beg <= year & (is.na(mar_end) | mar_end >= year), 1 , 0)]
d$married[is.na(d$married)] <- 0
d[, cohabit := ifelse(coh_beg <= year & (is.na(coh_end) | coh_end >= year), 1 , 0)]
d$cohabit[is.na(d$cohabit)] <- 0

# Keep only the indicators
d <- d[, .(id, year, cohabit, married)] %>% 
  as_tibble()

# Merge data
d2 <- merge(d2, d, by = c("id", "year"), all.x = T)

# Create the partnership info
d2$single <- 1
d2$single[d2$cohabit == 1 | d2$married == 1] <- 0

# Estimate the logistic regression
clr1_par <- glm(single  ~ availability, data = d2, family = "binomial")

# Estimate the logistic regression
clr2_par <- glm(single  ~ availability + cohort + inc_quant +  unemployed + edu, data = d2, family = "binomial")

# Estimate the logit model with ontextual controls
clr3_par <- glm(single ~ availability + cohort  + inc_quant +  unemployed + edu + educ_comp + unemployment + urban, data = d2, family = "binomial")

# Estimate the logistic regression
clr4_par <- glm(single ~ availability + inc_quant + availability:inc_quant + edu + cohort +  unemployed  + educ_comp  + unemployment + urban, data = d2, family = "binomial")

# Summarise logistic regression output
stargazer(clr1_par, clr2_par, clr3_par, clr4_par, title = paste0("Cumulative logistic regression singlehood at age 45 on partner availability using."), 
          out = paste0("results/Models/main_model_partner.tex"),
          align = T, no.space = T, omit.stat = c("f", "LL", "ser"), 
          ci = TRUE, ci.level = 0.95, type = "text")

### Classification analysis 
class_table <- d2$prediction <- ifelse(predict(clr4_par, d2, type = "response") >= 0.5, 1, 0)
mean(d2$single == d2$prediction)


### Predict for the last cohort --------------------------------------

# Load the data
load("data/ar_near_9.2km.Rda")
load("data/fixed_effects_1.Rda")

# Filter the cohorts
d2 <- d1

# Do the same steps as in 06_covariable_dataset.R
d2$inc[d2$year <= 1995] <- mean(d2$inc, na.rm = T)
d2$inc_quant[d2$year <= 1995] <- 3
d2 <- d2[!is.na(inc), ]
indicators <- names(d2)[str_detect(names(d2), "near")]
d2[, (indicators) :=  lapply(.SD, log), .SDcols = indicators]
d2 <- d2[order(id, year), ]

# Reduce the availability ratio to the values from 1990
ar_near <- ar_near[year == 1990, ]
ar_near <- ar_near[, year := NULL]

# Join
d2 <- merge(d2, ar_near, by.x = c("age_group", "res", "edu"), by.y = c("age", "res", "edu"))

# Summarise the data
d2 <- d2[, .(ar_near_9.2km = mean(ar_near_9.2km, na.rm = T),
             availability = mean(log(ar_near)),
             edu = last(edu),
             childless = max(pc, na.rm= T),
             unemployed = sum(act == "unemployed"),
             cohort = unique(cohort),
             yob = unique(yob),
             age = max(age),
             income = mean(inc),
             inc_quantile = last(inc_quant),
             unemployment = mean(unempl_rate, na.rm = T),
             educ_comp = mean(share_tertiary, na.rm = T),
             poverty = mean(poverty, na.rm = T),
             urban = mean(as.numeric(urban), na.rm = T),
             year = last(year)),
         by = id]

# Create grouped income and education variables
d2[ , inc_quant := factor(inc_quantile, labels = c("Q1", "Q2", "Q3", "Q4"))]
d2[ , educ_comp := factor(ntile(educ_comp, 2), labels = c("low", "high"))]
d2[ , inequality := factor(ntile(poverty, 2), labels = c("low", "high"))]

# Remove missings
d2 <- na.omit(d2)

# Predict
d2$counter <- predict(clr4, d2, type= "response")

# Estimate the share childless
d2[, .(observed = mean(childless), counter  = mean(counter))] %>% 
  mutate(difference = (observed - counter) * 100)

# Estimate the change in the polarization in childlessness
d2[, .(observed = mean(childless), counter  = mean(counter)), by = edu]

# Plot the result
ggplot(d2) +
  geom_density(aes(x = ar_near_9.2km, fill = "Observed"), alpha = .3) +
  geom_density(aes(x = availability, fill = "Counterfactual (1990)"), alpha = .3) +
  scale_fill_brewer(palette = "Set1", name = "") +
  scale_y_continuous("Density", expand = c(0, 0)) +
  scale_x_continuous("Availability ratio (near)") +
  geom_vline(xintercept = 0) +
  theme(legend.position = c(0.2, 0.8),
        legend.title = element_blank())
ggsave(last_plot(), filename = "figures/counterfact_partner_1990.pdf", height = 15, width = 25, unit = "cm")


ggplot(d2) +
  geom_density(aes(x = ar_near_9.2km, fill = "Observed"), alpha = .3) +
  geom_density(aes(x = availability, fill = "Counterfactual (1990)"), alpha = .3) +
  facet_wrap(  ~ ordered(edu, c("basic", "medium", "high"))) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous("Density", expand = c(0, 0)) +
  scale_x_continuous("Availability ratio (near)") +
  geom_vline(xintercept = 0) +
  theme(legend.position = c(0.5, 0.8),
        legend.title = element_blank())

### Regression for spatial distance ----------------------------------

# Estimate the logit model with contextual controls
clr_dist1 <- glm(childless ~ ar_near_0km + cohort  + inc_quant +  unemployed + edu + educ_comp + unemployment + urban + inequality, data = d2, family = "binomial")
clr_dist2 <- glm(childless ~ availability + cohort  + inc_quant +  unemployed + edu + educ_comp + unemployment + urban + inequality, data = d2, family = "binomial")
clr_dist3 <- glm(childless ~ ar_near_20km + cohort  + inc_quant +  unemployed + edu + educ_comp + unemployment + urban + inequality, data = d2, family = "binomial")

# Summarise logistic regression output
stargazer(clr_dist1, clr_dist2, clr_dist3, title = paste0("Cumulative logistic regression on childlessness using different spatial distances predictor variable."), 
          out = paste0("results/Models/model_distance_comp.tex"), type = "text",
          align = T, no.space = T, omit.stat = c("f", "ser"), 
          ci = TRUE, ci.level = 0.95)

# Estimate the pop-effect
dist_models <- list("0km" = clr_dist1, "9.2km" = clr_dist2, "20km" = clr_dist3)
map(dist_models, pop_effect_log)

###################    END         ############################