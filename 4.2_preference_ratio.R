##################################################
#         Birth squeezes in Finland              #
#         Henrik-Alexander Schubert              #
#           Avprlability Index                    #
##################################################

#####   Settings  ##########################

# last edited on: 19th January 2023
# last edited by: Henrik-Alexander Schubert
rm(list = ls())

# Load the packages
library(tidyverse)
library(data.table)
library(dbplyr)
library(haven)

# Load the functions
source("functions/functions.R")

# Load the data
load("data/parent_child.Rda")
load("data/pop.Rda")
load("data/regions.Rda")  

# Merge with pop data
pop <- left_join(reg, pop, by = "res")

# Aggregate the data
pop <- aggregate(Total ~ sex + year + age + reg, FUN = sum, data = pop)

#### Function to estimate the preference ratio -------------------------

preference_ratio <- function(data = pop, prefM = pref_male) {
  
  # Filter the data
  pop_fem <- data[sex == "Female", ]
  pop_mal <- data[sex == "Male", ]
  
  # Result
  res <- numeric()
  
  # For the different ages of ego
  for(i in ages){
    
    # Filter the preference
    pref <- prefM[age_father == i]
    
    # Filter the pop
    p <- unlist(pop_mal[age == i, .(Total)])
    
    # Merge pop and preference data
    tmp <- pop_fem[pref, on = .(age == age_mother)]
    
    # Estimate the denominator
    tmp <- unlist(tmp[, .(den = sum(Total * prop, na.rm = T))])
    
    # Bind the results
    res <- c(res, tmp / p)
    
  }
  
  res <- data.table(
    age = ages,
    pr  = res
  )
  
  return(res)
}

##### 1. Estimate weights    --------------------------

# Male ages
ages <- c(18, seq(20, 55, by = 5))

# Share of missing 
years <- 1987:2018

# Filter the data
df <- subset(parent_child, subset = yob_child %in% years)

##### 2. Estimate weights    --------------------------

# Subset the data
df$age_mother[df$age_mother < 18] <- 18
df$age_mother[df$age_mother > 55] <- 55
df$age_father[df$age_father < 18] <- 18
df$age_father[df$age_father > 55] <- 55

### Male preferences -----------------
pref_male <- as_tibble(df) %>% 
  group_by(age_father, age_mother) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  complete(age_mother = 18:55, age_father = 18:55, fill = list(n = 0))  %>% 
  group_by(age_father) %>% 
  mutate(total = sum(n)) %>% 
  group_by(age_mother, age_father) %>% 
  summarise(prop = n / total, .groups = "drop")  %>% 
  as_tibble()

### Female preferences -----------------
pref_female <- as_tibble(df) %>% 
  group_by(age_father, age_mother) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  complete(age_mother = 18:55, age_father = 18:55, fill = list(n = 0))  %>% 
  group_by(age_mother) %>% 
  mutate(total = sum(n)) %>% 
  group_by(age_mother, age_father) %>% 
  summarise(prop = n / total, .groups = "drop") 

### Tests ----------------------------------------

# Are the weights 1 for women 
if(any(pref_female %>% group_by(age_mother) %>% summarise(total = sum(prop)) %>% pull(total) != 1) )  stop("Weights differ from one!")

# Are the weights 1 for men  
if(any(pref_male %>% group_by(age_father) %>% summarise(total = sum(prop)) %>% pull(total) != 1) )  stop("Weights differ from one!")

### Estimate the preference ratio ---------------------

# Dimensions
ages <- 18:55
regions <- unique(pop$reg)

# Reduce the data to one year and region
pop <- as.data.table(pop)
pref_female <- as.data.table(pref_female)
pref_male <- as.data.table(pref_male)

# Filter age range
pop <- pop[age %in% ages, ]

# Complete the data
pop <- completeDT(pop, cols = c("reg", "sex" , "year", "age"), defs = c(Total=0))

# Split the data
pop <- split(pop, list(pop$year, pop$reg))

# Use vectorisation
pr <- map(pop, ~ preference_ratio(.x))

# Change the format
pr <- bind_rows(pr, .id = "meta")

# Separate the column
pr$year <- as.numeric(lapply(str_split(pr$meta, pattern = "\\."), "[", 1))
pr$reg  <- as.character(lapply(str_split(pr$meta, pattern = "\\."), "[", 2))

# Delete the meta variable
pr <- pr[, .(year, age, pr, reg)]

# Save the result
save(pr, file = "data/pr.Rda")

### Estimate the preference ratio for close regions ----------------------

# Load the data
load("data/pop_near.Rda")

# Get the indicators
dist <- unique(pop_near$dist)

for (i in dist) {
  
    cat("Distance", i, "\n")
    
  # Filter the data
  pop <- pop_near[dist == i, ]
    
  # Aggregate
  pop <- pop[, .(Total = sum(Total)), by = .(age, year, Res, sex)]
  
  # Filter the missings
  pop <- pop[!is.na(Res) & !is.na(sex) & age %in% 18:55, .(Reg = Res, sex, year, age, Total) ]
  
  # Complete the data
  pop <- completeDT(pop, cols = c("Reg", "sex" , "year", "age"), defs = c(Total=0))
  
  # Split the data
  pop <- split(pop, list(pop$year, pop$Reg))
  
  # Use vectorisation
  pr_near <- map(pop, ~ preference_ratio(.x))
  
  # Change the format
  pr_near <- bind_rows(pr_near, .id = "meta")
  
  # Seperate the column
  pr_near$year <- as.numeric(lapply(str_split(pr_near$meta, pattern = "\\."), "[", 1))
  pr_near$res  <- as.numeric(lapply(str_split(pr_near$meta, pattern = "\\."), "[", 2))
  
  # Delete the meta variable
  pr_near <- pr_near[, .(year, age, pr_near = pr, res = res)]
  
  # Remove missing information
  pr_near <- pr_near %>% filter(pr_near != Inf)
  
  # Save the result
  save(pr_near, file = paste0("data/pr_near_", i, ".Rda"))

}

############          END           ####################