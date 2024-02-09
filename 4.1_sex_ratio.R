##################################################
#         Birth squeezes in Finland              #
#         Henrik-Alexander Schubert              #
#                 Sex-Ratio                      #
##################################################

#####   Settings  ################################

# last edited on: 12 February 2023
# last edited by: Henrik-Alexander Schubert

rm(list = ls() )

# Load the functions
source("functions/functions.R")

# Load the packages
library(haven)
library(tidyverse)
library(pander)
library(data.table)
library(patchwork)

# Define the age range
ages <- 18:55
years <- 1986:2020

# Load the population distribution
load("data/pop_near.Rda")
load("data/pop.Rda")
reg <- read.csv("D:/metadata/classifications/region/alueet22.csv", encoding = "latin1")

# Load the maps
load("data/map_seutukunta.Rda")
load("data/map_kunta.Rda")

#### Estimate for Seutukunta ---------------------------

# Rename the variables
reg <- rename(reg, Res = kunta,Kunta = Kunta0, reg = Mkkoodi, Reg = Seutukunta) 

# Create the data
reg <- subset(reg, select = c(Res, Kunta, Reg, reg))

# Kunta as numeric
pop$Res <- as.numeric(pop$Res)

# Merge with pop data
pop <- left_join(pop, reg, by = c("res" = "Res"))

# Aggregate the data
pop_sr <- as.data.table(pop)[, .(Total = sum(Total)), by = .(sex, year, age, Reg)]

# Estimate the sex ratio
pop_sr <- dcast(pop_sr, year + age + Reg ~ sex, value.var = "Total")

# Estimate the sex ratio
pop_sr <-  pop_sr[, .(year, age, reg = Reg, sr = Female / Male, Female, Male)]


#### Estimate sex ratio for neighbours -----------------

# Get the indicators
dist <- unique(pop_near$dist)

for (i in dist) {
  
  cat("Distance", i, "\n")
    
  # Select the data
  pop  <- pop_near[dist == i, ]  
    
  # Estimate the totals
  sr_near <- pop[, .(Total = sum(Total)), by = .(sex, year, age, Res)]
  
  # Reshape wider
  sr_near <- dcast(sr_near, year + age + Res ~ sex, value.var = "Total")
  
  # Estimate the sex ratio
  sr_near <-  sr_near[, .(year, age, res = Res, sr_near = Female / Male, Female, Male)]
  
  # Subset the region 
  sr_near <- sr_near[age %in% ages & year %in% years, .(year, age, res, sr_near)]
  
  # Save the result
  save(sr_near, file = paste0("data/sr_near_", i, ".Rda"))

}

## 3. Combine the results -----------------------------

# Subset the data
pop_sr <- pop_sr[age %in% ages & year %in% years, .(year, age, reg, sr)]

# Save the data
save(sr_near, file = "data/sr_near.Rda")
save(pop_sr, file = "data/pop_sr.Rda")

### END ####################################################################