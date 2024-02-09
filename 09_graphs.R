##################################################
#         Birth squeezes in Finland              #
#         Henrik-Alexander Schubert              #
#               Visualisation                    #
##################################################

#####   Settings  ##########################

rm(list = ls())

# Set the locatoin
Sys.setlocale("LC_CTYPE", "finnish")

# Load the packages
library(tidyverse)
library(ggrepel)
library(sf)
library(ggthemes)
library(rmapshaper)
library(patchwork)
library(data.table)

# load the data
#d = read.csv("data/pop.csv")
load("data/male_age_preferences.R")
load("data/female_age_preferences.R")

# Load the avprlability information
load("data/ar.Rda")
load("data/ar_near_9.2km.Rda")
load("data/pr.Rda")
load("data/pr_near_9.2km.Rda")
load("data/sr_near_9.2km.Rda")
load("data/pop_sr.Rda")

# load the functions
source("functions/functions.R")

### Plotting the age-education preferences ----------------------

# Make the preferences to tibbles
pref_female <- as_tibble(pref_female)
pref_male <- as_tibble(pref_male)

# Make the contour plot
pref_female %>% 
  ggplot(aes(age_father, age_mother, fill = prop)) + 
  geom_tile() + 
  geom_abline(intercept = 0, slope = 1, colour = "darkred") + 
  facet_grid(edu_mother ~ edu_father) + 
  xlab("Age of the father") + ylab("Age of the mother") + 
  theme(legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(1.5, "cm")) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradient(name = "Proportion", low = "white", high = "darkred")
ggsave(filename = "Figures/age_edu_pref_mal.pdf", height = 30, width = 30, units = "cm")  

# Make the contour plot
pref_male %>% 
  ggplot(aes(age_mother, age_father, fill = prop)) + 
  geom_tile() + 
  geom_abline(intercept = 0, slope = 1, colour = "darkred") + 
  facet_grid(edu_father ~ edu_mother) + 
  ylab("Age of the father") + xlab("Age of the mother") + 
  scale_fill_gradient(name = "Proportion", low = "white", high =  "darkred") + 
  theme(legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(1.5, "cm")) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) 

ggsave(filename = "Figures/age_edu_pref_mal.pdf", height = 30, width = 30, units = "cm")  

## Plotting the life-course Sex Ratio ----------------------------------------

# Plot the life-course pattern
pop_sr   <- as.data.frame(pop_sr)
sr_near  <- as.data.frame(sr_near)

# Plot the pr at age 30 over time
sr_plot <- pop_sr  %>% 
  filter(year %in% seq(1990, 2020, by = 10)) %>% 
  group_by(year, age) %>% 
  summarise(sr = mean(sr))
a <- ggplot(sr_plot, aes(age, sr, colour = as.factor(year), group = as.factor(year))) + 
  geom_smooth(method = "loess", formula = "y ~ x", se = F, size = 2) + 
    geom_hline(yintercept = 1, col = "black", linetype = "dashed") +
    geom_text(data = subset(sr_plot, age == 55), aes(label = year), hjust = -0.04, size = 6) +
    scale_colour_manual(values = MPIDRpalette, name = "Year") +
    scale_y_continuous(trans = "log", limits = c(0.75, 1.75)) +
    scale_x_continuous("Age", expand = c(0, 0), limits = c(18, 60)) +
    ylab("Sex ratio") +
    guides(colour = "none")
ggsave(last_plot(), filename = "figures/change_sr_age.pdf", height = 15, width = 20, unit = "cm")

# Plot the pr at age 30 over time
sr_near_plot <- sr_near  %>% 
  filter(year %in% seq(1990, 2020, by = 10)) %>% 
  group_by(year, age) %>% 
  summarise(sr = mean(sr_near, na.rm = T))
b <- ggplot(sr_near_plot, aes(age, sr, colour = as.factor(year), group = as.factor(year))) + 
  geom_smooth(method = "loess", formula = "y ~ x", se = F, size = 2) + 
  geom_hline(yintercept = 1, col = "black", linetype = "dashed") +
  geom_text(data = subset(sr_near_plot, age == 55), aes(label = year), hjust = -0.04, size = 6) +
  scale_colour_manual(values = MPIDRpalette, name = "Year") +
  scale_y_continuous(trans = "log", limits = c(0.75, 1.75)) +
  scale_x_continuous("Age", expand = c(0, 0), limits = c(18, 60)) +
  ylab("Sex ratio (near)") +
  guides(colour = "none")

# Save the plot
ggsave(a + b, filename = "figures/sr_comparison_lifecourse.pdf", height = 15, width = 30, unit = "cm")


# Time-trend of availability indicators ----------------------------

# Plot the availability ratio
tt_ar <- ar[age == "(25,30]", .(availability = mean(log(ar), na.rm = T)), by = .(edu, year)] %>% 
  ggplot(aes(x = year, y = exp(availability), colour = factor(edu), group = edu)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 1) +
  scale_colour_brewer(palette = "Set2", name = "Education:") +
  scale_y_continuous("Availability ratio", limits = c(0.6, 1.4), n.breaks = 6) +
  scale_x_continuous("Year", expand = c(0, 0), n.breaks = 8)

# Plot the availability ratio
tt_ar_near <- ar[age == "(25,30]", .(availability = mean(log(ar), na.rm = T)), by = .(edu, year)] %>%
  ggplot(aes(x = year, y = exp(availability), colour = factor(edu), group = edu)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 1) +
  scale_colour_brewer(palette = "Set2", name = "Education:") +
  scale_y_continuous("Availability ratio", limits = c(0.6, 1.4), n.breaks = 6) +
  scale_x_continuous("Year", expand = c(0, 0), n.breaks = 8)

# Plot sex ratio
pop_sr <- as.data.table(pop_sr)
tt_sr <- pop_sr[age %in% c(25, 30, 35), .(availability = mean(log(sr), na.rm = T)), by = .(age, year)] %>% 
      ggplot(aes(x = year, y = exp(availability), colour = factor(age), group = age)) +
      geom_line(size = 1.2) +
      geom_hline(yintercept = 1) +
      scale_colour_brewer(palette = "Set1", name = "Age:") +
      scale_y_continuous("Sex ratio", trans = "log", n.breaks = 6, limits = c(0.6, 1.4)) +
      scale_x_continuous("Year", expand = c(0, 0), n.breaks = 8)

# Plot sex ratio
sr_near <- as.data.table(sr_near)
tt_sr_near <- sr_near[age %in% c(25, 30, 35), .(availability = mean(log(sr_near), na.rm = T)), by = .(age,year)] %>% 
  ggplot(aes(x = year, y = exp(availability), colour = factor(age), group = age)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 1) +
  scale_colour_brewer(palette = "Set1", name = "Age:") +
  scale_y_continuous("Sex ratio (near)", trans = "log", n.breaks = 6, limits = c(0.6, 1.4)) +
  scale_x_continuous("Year", expand = c(0, 0), n.breaks = 8)

# Plot preference ratio
tt_pr <- pr[age %in% c(25, 30, 35), .(availability = mean(log(pr), na.rm = T)), by = .(age,year)] %>% 
  ggplot(aes(x = year, y = exp(availability), colour = factor(age), group = age)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 1) +
  scale_colour_brewer(palette = "Set1", name = "Age:") +
  scale_y_continuous("Preference ratio", trans = "log", n.breaks = 6, limits = c(0.6, 1.4)) +
  scale_x_continuous("Year", expand = c(0, 0), n.breaks = 8)

# Plot preference ratio
tt_pr_near <- pr_near[age %in% c(25, 30, 35), .(availability = mean(log(pr_near), na.rm = T)), by = .(age,year)] %>% 
  ggplot(aes(x = year, y = exp(availability), colour = factor(age), group = age)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 1) +
  scale_colour_brewer(palette = "Set1", name = "Age:") +
  scale_y_continuous("Preference ratio (near)", trans = "log", n.breaks = 6, limits = c(0.6, 1.4)) +
  scale_x_continuous("Year", expand = c(0, 0), n.breaks = 8)

(tt_sr + tt_sr_near) / (tt_pr + tt_pr_near) / (tt_ar + tt_ar_near) +
  plot_layout(guides = "collect" )

ggsave(last_plot(), filename = "figures/timetrend_partner_indicators.pdf", height = 25, width = 20, unit = "cm")

### Creating maps -------------------------------------

# Load the map data
load("data/map_seutukunta.Rda")
load("data/map_kunta.Rda")

## Plot the availability-ratio maps --------------------------------------

# Merge pop data with maps
ar_near <- inner_join(shape, ar_near, by = c("Res" = "res"))
ar  <- inner_join(shape_seutukunta, ar, by = c("Seutukunta" = "reg"))

# Create the basic map for cohort born 1970 average until age 30
plot_map1 <- ar %>% 
  filter(age == "(30,35]" & year %in% c(1987, 2005, 2020)) %>%
  group_by(edu, year, Seutukunta) %>% 
  dplyr::summarise(ar = mean(ar, na.rm = T), .groups = "drop")  %>%
  dplyr::mutate(Year = year, Education = factor(edu, levels = c("basic", "medium", "high"), ordered = T)) %>% 
  ggplot(aes(fill = ar)) + 
  geom_sf(size = 0) + 
  facet_grid(Education ~ Year, labeller = label_both) + 
  scale_fill_gradient2(low = "darkred", mid = "grey", high = "navyblue", midpoint = 1, name = "AR", limits = c(0.5, 2)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9),
        # legend.key.height = unit(2.3, "cm"),
        plot.title.position = "plot",
        title = element_text(face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.key.width = unit(2, "cm")
  )

# Create the basic map for cohort born 1970 average until age 30
map_data <- ar_near %>%
  filter(age == "(30,35]" & year %in% c(1987, 2005, 2020)) %>%
  group_by(edu, year, Res) %>% 
  dplyr::summarise(ar = exp(mean(log(ar_near), na.rm = T)), groups = ".drop") %>%
  dplyr::mutate(Education = factor(edu, levels = c("basic", "medium", "high"), ordered = T),
                ar_cat = cut(ar, breaks = c(0, 0.5, 0.95, 1.05, 2, 3))) %>% 
  rename(Year = year)
  
# Create the map
plot_map2 <- ggplot(map_data, aes(fill = ar)) + 
  geom_sf(size = 0) + 
  scale_fill_gradient2(low = "darkred", mid = "grey", high = "navyblue", midpoint = 1, name = "AR (near)", limits = c(0.5, 2)) +
  facet_grid(Education ~ Year, labeller = label_both) + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.key.width = unit(2, "cm"),
        plot.title.position = "plot",
        title = element_text(face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank()
        )

# Save the maps
ggsave(plot_map1, filename = "figures/map_ar_years.pdf", units = "cm", width = 15, height = 25)
ggsave(plot_map2, filename = "figures/map_ar_near_years.pdf", units = "cm", width = 15, height = 25)

## Plot the preference-ratio maps --------------------------------------

# Merge pop data with maps
pr_near <- inner_join(shape, pr_near, by = c("Res" = "res"))
pr  <- inner_join(shape_seutukunta, pr, by = c("Seutukunta" = "reg"))

# Create the basic map for cohort born 1970 average until age 30
plot_map3 <- pr %>% 
  filter(age %in% c(25, 30, 35) & year %in% c(1987, 2005, 2020)) %>%
  group_by(age, year, Seutukunta) %>% 
  dplyr::summarise(pr = exp(mean(log(pr), na.rm = T)), .groups = "drop")  %>%
  rename(Age = age, Year = year) %>% 
  ggplot(aes(fill = pr)) + 
  geom_sf(size = 0) + 
  facet_grid(Age ~ Year, labeller = label_both) + 
  scale_fill_gradient2(low = "darkred", mid = "grey", high = "navyblue", midpoint = 1, name = "PR", limits = c(0.5, 2)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.key.width = unit(2, "cm"),
        plot.title.position = "plot",
        title = element_text(face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )


# Create the basic map for cohort born 1970 average until age 30
map_data <- pr_near %>%
  filter(age %in% c(25, 30, 35) & year %in% c(1987, 2005, 2019)) %>%
  group_by(age, year, Res) %>% 
  dplyr::summarise(pr = mean(pr_near, na.rm = T), groups = ".drop") %>% 
  rename(Age = age, Year = year)

# Create the map
plot_map4 <- ggplot(map_data, aes(fill = pr)) + 
  geom_sf(size = 0) + 
  facet_grid(Age ~ Year) + 
  scale_fill_gradient2(low = "darkred", mid = "grey", high = "navyblue", midpoint = 1, name = "PR (near)", limits = c(0.5, 2)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.key.width = unit(2, "cm"),
        plot.title.position = "plot",
        title = element_text(face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )

# Save the maps
ggsave(plot_map3, filename = "Figures/map_pr_years.pdf", units = "cm", width = 15, height = 25)
ggsave(plot_map4, filename = "Figures/map_pr_near_years.pdf", units = "cm", width = 15, height = 25)

## Plot the sex-ratio maps --------------------------------------

# Merge pop data with maps
sr_near <- inner_join(shape, sr_near, by = c("Reg" = "res"))
pop_sr  <- inner_join(shape_seutukunta, pop_sr, by = c("Seutukunta" = "reg"))

# Create the basic map for cohort born 1970 average until age 30
plot_map5 <- pop_sr %>% 
  filter(age %in% c(25, 30, 35) & year %in% c(1987, 2005, 2020)) %>%
  group_by(age, year, Seutukunta) %>% 
  dplyr::summarise(sr = exp(mean(log(sr), na.rm = T)))  %>%
  ggplot(aes(fill = sr)) + 
  geom_sf(size = 0.01) + 
  facet_grid(age ~ year) + 
  scale_fill_gradient2(low = "darkred", mid = "grey", high = "navyblue", midpoint = 1, name = "SR", limits = c(0.5, 2)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.key.width = unit(2, "cm"),
        plot.title.position = "plot",
        title = element_text(face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )

# Create the basic map for cohort born 1970 average until age 30
map_data <- sr_near %>%
  filter(age %in% c(25, 30, 35) & year %in% c(1987, 2005, 2019)) %>%
  group_by(age, year, Res) %>% 
  dplyr::summarise(sr = exp(mean(log(sr_near), na.rm = T)), groups = ".drop") %>%
  rename(Age = age, Year = year)

# Create the map
plot_map6 <- ggplot(map_data, aes(fill = sr)) + 
  geom_sf(size = 0) + 
  facet_grid(Age ~ Year) + 
  scale_fill_gradient2(low = "darkred", mid = "grey", high = "navyblue", midpoint = 1, name = "SR (near)", limits = c(0.5, 2)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.key.width = unit(2, "cm"),
        plot.title.position = "plot",
        title = element_text(face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )

# Save the maps
ggsave(plot_map5, filename = "Figures/map_sr_years.pdf", units = "cm", width = 15, height = 25)
ggsave(plot_map6, filename = "Figures/map_sr_near_years.pdf", units = "cm", width = 15, height = 25)

###########       END       ######################  