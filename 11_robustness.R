##################################################
#         Birth squeezes in Finland              #
#         Henrik-Alexander Schubert              #
#             Robustness Checks                  #
##################################################

#####   Settings  ##########################

rm(list = ls())

# Load the packages
library(data.table)
library(tidyverse)
library(dtplyr)
library(stargazer)
library(fixest)
library(MatchIt)
library(broom)
library(survminer)
library(patchwork)
library(ggrepel)
library(survival)

# Load the data
load("data/pop.Rda")

# Load the functions
source("functions/functions.R")
source("functions/models.R")

## Structure:
# 1. Non-parametric analysis
# 2. Sibling fixed effects regression
# 3. Individual fixed effects regression
# 4. Cumulative logistic regression
# 5. Spatial regression
# Change high income to reference group

# Load the data
load("data/cum_data_1.Rda")
load("data/sibling_effects_1.Rda")

### Transform the data -----------------------------------------

# Set the reference group
d2$in_quant <- with(d2, relevel(inc_quant, ref = 4))
d3$in_quant <- with(d3, relevel(inc_quant, ref = 4))

#### Estimate the model specifications -------------------------

# Assemble the names of the variables
specifications <- c("sr_near_9.2km", "pr_near_9.2km", "ar_near_9.2km", "sr", "pr", "ar")

# Estimate the sibling fixed effects
pop_sib_fe <- map_df(specifications, sibling_fe, data = d3)

# Estimate the  cumulative logistic regression
pop_cum_lo <- map_df(specifications, cum_logistic, data = d2)

# Estimate the cumulative logistic regression
pop_match <- map_df(specifications, matching_fe, data = d3)

### Estimate population level effect ---------------------------

# Combine the results
pop_effect_rob <- list("Sibling FE" = pop_sib_fe,
                   "Cumulative logistic" = pop_cum_lo,
                   "Matching" = pop_match) 

# Combine the results
pop_effect_rob <- bind_rows(pop_effect_rob, .id = "Model")

# Load the results from the discrete-time 
load("results/popeffect_dcsm.Rda")

# Combine the data
pop_effect <- bind_rows(pop_effect, pop_effect_rob)

# Clean the variable name
pop_effect$variable <- str_replace(pop_effect$variable, "_near_9.2 km", " (near)")

# Clean the labels
pop_effect$variable <- clean_labels(pop_effect$variable)

# Plot the result
ggplot(pop_effect, aes(x = variable, y = effect, colour = specification, shape = specification)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 4, position = position_dodge(width = 0.4)) +
  facet_grid( ~ Model) + 
  coord_flip() +
  scale_y_continuous("Population level effect", breaks = scales::pretty_breaks()) +
  scale_x_discrete("") +
  guides(shape = "none") +
  theme(panel.grid.major = element_line(colour = "grey", size = 0.4, linetype = "dotted"),
        panel.grid.major.x = element_line(colour = "grey", size = 0.4, linetype = "dotted"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 13))
  
### Save the gaph
ggsave(last_plot(), file = "Figures/robustness_pref_edu.pdf", width = 30, height = 15, unit = "cm")

##########         END             ###############