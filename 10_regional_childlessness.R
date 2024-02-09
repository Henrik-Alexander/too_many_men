##################################################
#         Birth squeezes in Finland              #
#         Henrik-Alexander Schubert              #
#              Descriptives                      #
##################################################

#####   Settings  ##########################

rm(list = ls())

# Load the packages
library(data.table)
library(tidyverse)
library(dtplyr)
library(patchwork)
library(stargazer)
library(ggthemes)  
library(GGally)
library(sf)

# Load the data
load("data/fixed_effects_1.Rda")
load("data/context_data.Rda")
load("data/map_seutukunta.Rda")


# Load the functions
source("functions/Functions.R")


## Structure:
# 1. Univariate statistics
# 2. Bivariate statistics
# 3. Graphs

### What are the areas with the highest childlessness ----------------------

# Arrange the data table
d1 <- d1[order(id, age), ]

# Filter the last observation
d1 <- d1[, .(count = .N, row_number = seq_len(.N), reg, pc, yob, year, res), by = .(id)]

# Select only the last observation
d1 <- d1[row_number == count & yob >=  1968 & yob <= 1975, ]

# Estimate mean childlessness by cohort and region
d2 <- d1[, .(pc = mean(pc), N = .N), by = res]
d1 <- d1[, .(pc = mean(pc), N = .N), by = reg]

# Year
d2$year <- d1$year <- 2019

# Join with context data
d1 <- merge(d1, context_data, by = c("year", "reg"))
d2 <- left_join(d2, shape_seutukunta, by = c("res" =  "Reg"))

# Rename the childlessness column
d1$childlessness <- d1$pc

# Pairwise plot
cors_spat <- as_tibble(d1) %>% 
  select(childlessness, mean_income, poverty, unempl_rate, share_tertiary) %>% 
  ggpairs(title = "Aggregate correlations with childlessness",
          columnLabels = c("Childlessness", "Mean \n Income", "Inequality", "Unemployment \n Rate", "% Teriary")) +
  theme_test(base_size = 10, base_family = "serif")
ggsave(cors_spat, filename = "Figures/cor_plot.pdf", units = "cm", width = 12, height = 10)

# Merge map with childlessness
data_child  <- left_join(shape_seutukunta, d1, by = c("Seutukunta" =  "reg")) %>% 
  filter(!is.na(childlessness)) %>% 
  mutate(childlessness = cut(100 * childlessness, breaks = seq(0, 100, by = 5), ordered_result = T)) 

# Plot the data
ch1 <- ggplot(data_child, aes(fill = childlessness)) + 
  geom_sf(colour = "black") + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  guides(fill = guide_legend(nrow = 2, byrow = T)) + 
  scale_fill_manual(values = MPIDRpalette[c(3, 1, 5, 6, 2)], name = "% Childless") + 
  theme_map(base_family = "serif", base_size = 14) + 
  theme(legend.position = "bottom", 
        legend.key.height = unit(0.1, "cm"),
        plot.margin = unit(c(0, 0, 0, 0), "mm"),
        legend.box.margin = margin(-10, 40, 10, 10)) + 
  guides(fill = guide_colorsteps(title.position = "top", title.hjust = 0.5))

# Merge map with childlessness
ch2 <- left_join(shape_seutukunta, d1, by = c("Seutukunta" =  "reg")) %>% 
  filter(!is.na(childlessness)) %>% 
  ggplot(aes(fill = childlessness)) + 
  geom_sf(colour = "black", size = 0.2) + 
  scale_fill_gradient2(low = MPIDRred, midpoint = mean(d1$childlessness), mid = "lightgrey", high = MPIDRpurple, name = "% Childless") + 
  theme_map(base_family = "serif", base_size = 14) + 
  theme(legend.position = "bottom", 
        legend.key.height = unit(0.1, "cm"),
        legend.key.width = unit(1, "cm"),
        plot.margin = unit(c(0, 0, 0, 0), "mm"),
        legend.box.margin = margin(-30, 10, 10, 10)) + 
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

# Save the pictures
ggsave(ch1, filename = "Figures/childless_spat_discr.pdf", units = "cm", width = 8, height = 12)
ggsave(ch2, filename = "Figures/childless_cont_discr.pdf", units = "cm", width = 8, height = 12)

#############     END       ################## 