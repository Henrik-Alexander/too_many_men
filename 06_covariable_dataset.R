##################################################
#         Birth squeezes in Finland              #
#         Henrik-Alexander Schubert              #
#             Covariable dataset                 #
##################################################

#####   Settings  ################################

rm(list = ls())

# Load the packages
library(data.table)
library(tidyverse)
library(dtplyr)
library(haven)

# Load the functions
source("functions/functions.R")

# Create teh funciton
load_obj <- function (f) {
  env <- new.env()
  nm  <- load(f, env)[1]
  env[[nm]]
}

### Load the data ----------------------------------------

# Load the data
load("data/parent_child.Rda")
load("data/regions.Rda")
load('data/context_data.Rda')

# Load the availability information
load("data/ar.Rda")
load("data/pr.Rda")
load("data/pop_sr.Rda")

# Load the data for the different proximate determinants

# What are the distances
distances <- c("0km", "9.2km", "20km")

for ( i in c("ar", "pr", "sr")) {
    
  # Load the data
  indicators <- paste0("data/", i, "_near_", distances,".Rda")
  
  # Load the spatial objects
  spatial <- map(indicators, load_obj)
  names(spatial) <- str_remove(str_extract(indicators, "[a-z]r_near_.*"), ".Rda")
  spatial <- map(spatial, as_tibble)
  spatial <- bind_rows(spatial, .id = "indicator")
  spatial <- pivot_wider(spatial, names_from = "indicator", values_from = paste0(i, "_near"))
  
  # Make it to a data.tble
  spatial <- as.data.table(spatial)
  
  # Assign the result
  assign(paste0(i, "_near"), spatial)
  rm(spatial)
}

# Want to create log of indicators 
logged <- "logged"

# What are the time lags
time_lag <- 1:3

# Set the age cut off
CutOff <- 45

# Define the ages
ages <- 18:CutOff

# years
max <- 2020 - max(ages)
min <- 1987 - min(ages)

# Set the age breaks
age_breaks <- c(18, 25, 30, 35, 40, 45, 50, 55)
    
### Create the data for FE regression -----------------------------

# Load the basic data
load("data/basic.Rda")

# Filter only the Finnish men
basic <- basic[ori == 12 | ori == 11, ]

# Filter 
basic <- basic[sex == "Male" & age %in% ages, ]

# Select
basic <- basic[, c("year", "id", "yob", "yod", "res", "edu", "act", "age", "inc", "urban")]

# Combine with regions
basic <- merge(basic, reg, by = "res")

# Order the data
basic <- basic[order(id, year), ]

# Make parent child
father <- parent_child[!is.na(id_father), .(id_father, cid, age_father, nch_father, yod_parent_father, yob_child)]

# Select the first birth
father <- father[nch_father == 1, ]

# Merge with the regional information
basic <- merge(basic, father, by.x = "id", by.y = "id_father", all.x = T, all.y = F)

# Remove spells that are larger then age of father
basic <- basic[age <= age_father | is.na(age_father), ]

for (i in time_lag) {
  
  # Create the data
  d1 <- basic
  
  # Create the variables for permanent and temporary childless
  d1[, tc := ifelse(age < age_father - i | is.na(age_father), 1, 0)]
  d1[, pc := ifelse(age_father - i > CutOff | is.na(age_father), 1, 0)]
  d1[, birth := if_else(age == age_father - i, 1, 0, missing = 0)]
                    
  # Filter if a person dies before the cut-off, unless he has given birth before
  d1 <- d1[tc == 1| (birth == 1), ]
  
  # Save the data for analysis of childlessness trends
  if (i == 1) save(d1, file = "data/childless_trend.Rda")
  
  # Filter if a person get's a child before data observation
  d1 <- d1[yob >= min & yob <= max, ]
  
  # Order the results
  d1 <- d1[order(id, year), ]
  
  # Filter persons that are either permanent childless or temporary childless and younger than the cutoff
  d1 <- d1[(pc == 0 & (tc == 1 | birth == 1)) | pc == 1, ]
  
  # Filter if any variable is missing
  d1 <- d1[!is.na(id) & !is.na(year) & !is.na(age) & !is.na(id) & !is.na(act) & !is.na(res) & !is.na(reg)]
  
  # Categorize cohort variable
  d1$cohort <- cut(d1$yob, breaks = seq(min, max+1, length.out = 3), right = F, include.lowest = T)
  
  # Create income quantiles
  d1[ , inc_quant := factor(ntile(inc, 4)), by = year]
  
  # Join d1 with context data
  d1 <- left_join(d1, context_data, by = c("reg", "year"))
  
  # Merge with preference ratio
  d1 <- left_join(d1, pr, by = c("age", "year", "reg" ))
  
  # Merge with preference ratio index based on spatial proximity
  d1 <- left_join(d1, pr_near, by = c( "age", "year" , "res"))
  
  # Merge with sex ratio at population level
  d1 <- left_join(d1, pop_sr, by = c("age", "year" , "reg"))
  
  # Merge with sex ratio of neighbourhoods
  d1 <- left_join(d1, sr_near, by = c("age", "year" , "res"))
  
  # Transform the age variable
  d1 <- d1 %>% mutate(age_group = cut(age, breaks = age_breaks, include.lowest = T, ordered = T))
  
  # Merge with avprlability ratio
  d1 <- left_join(d1, ar, by = c("age_group" = "age", "year", "reg", "edu"))
  
  # Merge with avprlability ratio based on spatial proximity
  d1 <- left_join(d1, ar_near, by = c("age_group" = "age", "year", "res" , "edu"))
  
  # Change the activity coding
  d1$act <- ifelse(d1$act %in% c("7.other_outside_labor",  "4.pensioner", "5.conscript", ""), "4.other", d1$act)
  
  #### Checking the data for correctness ------------------------------------
  
  # Make it a data table
  d1 <- as.data.table(d1)
  
  # Save for the fixed effects approach
  save(d1, file = paste0("data/fixed_effects_", i, ".Rda"))
  
  ### Create the data for cum regression -------------------------------------
  
  # Filter the cohorts
  d2 <- d1
  
  # Impute the income
  d2$inc[d2$year <= 1995] <- mean(d2$inc, na.rm = T)
  d2$inc_quant[d2$year <= 1995] <- 3
  
  # Filter non-missing
  d2 <- d2[!is.na(inc), ]
  
  # Filter people who have the same number of spells as years in the data and who have been observed for 27 years
  #missings <- d2[ , .(years = max(year) - min(year), spells = .N), by = id]
  #missings <- missings[spells >= years ]$id
  #d2 <- d2[id %in% missings]
  
  # Indicators
  indicators <- names(d2)[str_detect(names(d2), "near")]
  d2[, (indicators) :=  lapply(.SD, log), .SDcols = indicators]
  
  # Order the data
  d2 <- d2[order(id, year), ]
  
  # Summarise the data
  d2 <- d2[, .(pr = mean(log(pr), na.rm = T),
                     ar = mean(log(ar), na.rm = T), 
                     sr = mean(log(sr), na.rm = T),
                     ar_near_9.2km = mean(ar_near_9.2km, na.rm = T),
                     ar_near_0km = mean(ar_near_0km, na.rm = T),
                     ar_near_20km = mean(ar_near_20km, na.rm = T),
                     sr_near_9.2km = mean(sr_near_9.2km, na.rm = T),
                     pr_near_9.2km = mean(pr_near_9.2km, na.rm = T),
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
  
  # Save the data
  save(d2, file = paste0("data/cum_data_", i, ".Rda"))
  
  ### Create sibling data -----------------------------------------------------
  
  # Merge with sibling data
  load("data/sibling.Rda")
  
  # Merge
  d3 <- left_join(d2, siblings, by = c("id" = "cid"))
  
  # Make it a tibble
  d3 <- as_tibble(d3)
  
  # Filter male sibling pprrs
  d3 <- subset(d3, subset = !is.na(sib))
  
  # Create sibling order for singletons
  d3$bo <- ifelse(d3$bo == 1, 1, 0)
  
  # Remove missings
  d3 <- na.omit(d3)
  
  # Save the data
  save(d3, file = paste0("data/sibling_effects_", i, ".Rda"))

}

### Compare the results ---------------------------------------

plot_distribution <- function (data, variable = "ar_near") {
  label <- clean_labels(variable)
  # Plot the data
  plota <- data %>% 
    pivot_longer(cols = starts_with(variable), names_to = "distance", values_to = "value") %>% 
    mutate(distance = str_remove(distance, pattern = paste0(variable, "_"))) %>% 
    as_tibble() %>% 
    ggplot(., aes(x = value, fill = distance, colour = distance)) +
    geom_vline(xintercept = 1) +
    geom_density(alpha = .4) +
    scale_x_continuous(label, trans = "log", breaks = scales::pretty_breaks(n = 8), limits = c(0.5, 2), expand = c(0, 0)) +
    scale_y_continuous("Density", expand = c(0, 0), limits = c(0, 4.8)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  return(plota)
}

a <- plot_distribution(sr_near, "sr_near")
b <- plot_distribution(pr_near, "pr_near")
c <- plot_distribution(ar_near, "ar_near")

a + b + c + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A", tag_suffix = ")")
ggsave(last_plot(), filename = "figures/distance_indicators.pdf", height = 15, width = 30, unit = "cm")

#############         END         #################