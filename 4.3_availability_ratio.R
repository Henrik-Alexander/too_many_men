##################################################
#         Birth squeezes in Finland              #
#         Henrik-Alexander Schubert              #
#           Availability Ratio                   #
##################################################

#####   Settings  ################################

# last edited on: 12th January 2023
# last edited by: Henrik-Alexander Schubert

rm(list = ls())

# Load the functions
source("Functions/Functions.R")

# Load the packages
library(tidyverse)
library(data.table)
library(dbplyr)

# Load the conditional birth distribution
load("data/parent_child.Rda")

# Load the population distribution
load("data/pop.Rda")

# Load the regional data
reg <- read.csv("D:/metadata/classifications/region/alueet22.csv", encoding = "latin1")

# Set the age breaks
age_breaks <- c(18, 25, 30, 35, 40, 45, 50, 55)

# Years
years <- 1987:2020

### Functions ----------------------------------------

# The new function
avail_ratio <- function(pop = dta, pref_f = pref_female, pref_m = pref_male){
        
        # Characteristics
        ages <- unique(pop$age)
        education <- unique(pop$edu)
        
        # Create the grids
        fem_grid <- CJ(Edu_m = education, Age_m = ages, Edu_f = education, Age_f = ages)
        denominator <- CJ(Edu_m = education, Age_m = ages, Edu_f = education, Age_f = ages)
        male_grid <- CJ(Edu_f = education, Age_f = ages, Edu_m = education, Age_m = ages)
        
        # Result vectors
        res <- res2 <- tmp <- as.numeric()
        
        # Create population vectors
        fem_pop <- pop[sex == "Female"]
        mal_pop <- pop[sex == "Male"]
        
        # Estimate the right part of the denominator
        for(b in education){
                for(j in ages){
                        
                        # Subset the female preferences
                        pref <- pref_f[age_mother==j&edu_mother==b]
                        
                        # Using left join
                        pop_pref <- mal_pop[pref, on = .(age  == age_father, edu == edu_father)]
                        
                        # Sort the data
                        pop_pref <- pop_pref[order(edu_mother, age_mother)]
                        
                        # Estimate the availability
                        res <- c(res, unlist(pop_pref[, .(Value = Total * prop)]))
                        
                        
                }
        }
        
        # Assign the results
        male_grid$Value <- res
        
        # Estiamte the numerator
        for(e in education){
                for(i in ages){
                        
                        # Subset the male preferences
                        pref <- pref_m[age_father==i&edu_father==e]
                        
                        # Merging male preferences with female population
                        pop_pref <- fem_pop[pref, on = .(age == age_mother, edu == edu_mother)]
                        
                        # Sort 
                        pop_pref <- pop_pref[order(edu, age)]
                        
                        # Estimate the availability
                        res2 <- c(res2, unlist(pop_pref[, .(Value = Total * prop )]))
                        
                        ### Estimate the male population -----
                        
                        # Estimate the population
                        male_pop <- male_grid[ , .(male_pop = sum(Value, na.rm = T)),.(Age_m, Edu_m)]
                        
                        # Join weight and male_pop
                        result <- male_pop[pref, on = .(Edu_m == edu_mother,Age_m == age_mother)]
                        
                        # Sort
                        result <- result[order(Edu_m, Age_m)]
                        
                        # Estimate the availability
                        tmp <- c(tmp, unlist(result[, .(Value = prop * male_pop)]))
                        
                } } 
        
        
        # Assign the result
        fem_grid$Value <- res2
        denominator$Value <- tmp
        
        
        # Estimate the numerator
        numerator <- fem_grid[, .(num = sum(Value)), by = .(Edu_m, Age_m)]
        
        # Estimate the denominator
        denominator <- denominator[, .(den = sum(Value)), by = .(Edu_m, Age_m)]
        
        # Join the two columns
        AR <- numerator[denominator, on = .(Edu_m, Age_m)]
        
        # Estimate the AR
        AR <- AR[, .(ar = num / den), .(edu = Edu_m, age = Age_m)]
        
        
        return(AR)
        
} 


#### Aggregate the population data -------------------

# Rename the variables
reg <- rename(reg, Res = kunta, Kunta = Kunta0, reg = Mkkoodi, Reg = Seutukunta) 

# Create the data
reg <- subset(reg, select = c(Res, Kunta, Reg, reg))

# Male the education variable a factor
pop <- as_tibble(mutate(pop, edu = factor(edu, levels = c("basic", "medium", "high"))))

# Kunta as numeri
pop$res <- as.numeric(pop$res)

# Merge with pop data
pop <- left_join(reg, pop, by = c("Res" = "res"))

# Filter the data
pop <- pop %>% filter(age %in% 18:55)

# Create age classes
pop <- pop %>% mutate(age = cut(age, breaks = age_breaks, include.lowest = T, ordered = T))

# Make it to data.table
pop <- as.data.table(pop)

# Aggregate the data
pop <- pop[, .(Total = sum(Total)), by = .(sex, year, reg, age, Reg, edu)]

##### 1. Estimate weights    --------------------------

# Filter the data
df <- subset(parent_child, subset = yob_child %in% years)

# Subset the data
df$age_mother[df$age_mother < 18] <- 18
df$age_mother[df$age_mother > 55] <- 55
df$age_father[df$age_father < 18] <- 18
df$age_father[df$age_father > 55] <- 55

# Subset the data
df <- df[!is.na(edu_mother) & !is.na(edu_father), ]

# Male preferences -----------------
pref_male <- as_tibble(df) %>% 
        mutate(age_mother = cut(age_mother, breaks = age_breaks, include.lowest = T, ordered = T),
               age_father = cut(age_father, breaks = age_breaks, include.lowest = T, ordered = T)) %>% 
        group_by(age_mother, age_father, edu_father, edu_mother) %>% 
        summarise(n = n(), .groups = "drop") %>% 
        complete(age_mother, age_father, edu_father, edu_mother, fill = list(n = 0))  %>% 
        group_by(edu_father, age_father) %>% 
        mutate(prop = n / sum(n),
               edu_father = ordered(edu_father, levels = c("basic", "medium", "high")),
               edu_mother = ordered(edu_mother, levels = c("basic", "medium", "high")),
               prop = if_else(is.nan(prop), 0, prop / sum(prop, na.rm = T))) %>% 
        select(-n)

# Female preferences -----------------
pref_female <- df %>%
        as_tibble() %>% 
        mutate(age_mother = cut(age_mother, breaks = age_breaks, include.lowest = T, ordered = T),
               age_father = cut(age_father, breaks = age_breaks, include.lowest = T, ordered = T)) %>% 
        group_by(age_mother, age_father, edu_father, edu_mother) %>% 
        summarise(n = n(), .groups = "drop") %>% 
        complete(age_mother, age_father, edu_father, edu_mother, fill = list(n = 0))  %>% 
        group_by(edu_mother, age_mother) %>% 
        mutate(prop = n / sum(n, na.rm = T),
               edu_father = ordered(edu_father, levels = c("basic", "medium", "high")),
               edu_mother = ordered(edu_mother, levels = c("basic", "medium", "high")),
               prop = if_else(is.nan(prop), 0, prop / sum(prop, na.rm = T))) %>% 
        select(-n)

# Make it to data.frames
pref_male <- as.data.table(pref_male)
pref_female  <- as.data.table(pref_female)

## Save the preference data
save(pref_male, file = "data/male_age_preferences.R")
save(pref_female, file =  "data/female_age_preferences.R")

## Estimate the availability index =====================

# Transform the data into data.table for speeding up the calculations
pop <- as.data.table(pop)

# Fill up the missing values
pop <- completeDT(pop, cols = c("sex", "age", "edu", "year", "Reg"), defs = c(Total=0))

# Split the data
pop1 <- split(pop, list(pop$Reg, pop$year))

# Use vectorisation
ar <- map(pop1, ~ avail_ratio(.x), .progress = T)

# Change the format
ar <- bind_rows(ar, .id = "meta")

# Seperate the column
ar$reg <- lapply(str_split(ar$meta, pattern = "\\."), "[", 1)
ar$year  <- as.numeric(lapply(str_split(ar$meta, pattern = "\\."), "[", 2))

# Delete the meta variable
ar <- ar[, .(edu, age, ar, reg, year)]

# Region as character
ar$reg <- as.character(ar$reg)

# Save the result
save(ar, file = "data/ar.Rda")

### Estimate the availability index for close regions ----------------------

# Load the data
load("data/pop_near.Rda")

# Get the indicators
dist <- unique(pop_near$dist)

for (i in dist) {
                
        cat("Distance", i, "\n")
        
        # Select the indicator
        pop <- pop_near[dist == i,]
                
        # Remove the missings
        pop <- pop[!is.na(Res) & !is.na(sex) & age %in% 18:55, .(Reg = Res, sex, year, age, edu, Total) ]
        
        # Create the age categories
        pop$age <- cut(pop$age, breaks = age_breaks, include.lowest = T, ordered = T)
        
        # Aggregate the data
        pop <- pop[, .(Total = sum(Total)), by = .(Reg, sex, year, age, edu)]
        
        # Complete the data
        pop <- completeDT(pop, cols = c("Reg", "sex", "year", "age", "edu"), defs = c(Total=0))
        
        # Split the data
        pop <- split(pop, list(pop$year, pop$Reg))
        
        # Use vectorisation
        ar_near <- map(pop, ~ avail_ratio(.x), pref_f = pref_female, pref_m = pref_male, .progress = TRUE)
        
        # Change the format
        ar_near <- bind_rows(ar_near, .id = "meta")
        
        # Seperate the column
        ar_near$year <- as.numeric(lapply(str_split(ar_near$meta, pattern = "\\."), "[", 1))
        ar_near$res  <- as.numeric(lapply(str_split(ar_near$meta, pattern = "\\."), "[", 2))
        
        # Delete the meta variable
        ar_near <- ar_near[, .(edu, age, ar_near = ar, res, year)]
        
        # Save the result
        save(ar_near, file = paste0("data/ar_near_", i, ".Rda"))

}

##########    END     ##########################