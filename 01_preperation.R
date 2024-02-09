### 1. Population ###################
# Purpose: Regional exposures       #
# Author: Henrik-Alexander Schubert #
# Date: 16.06.2023                  #
# E-Mail: schubert@demogr.mpg.de    #
# Pre-requirements: 
#####################################

# Load the functions
source("functions/functions.R")

# Load the packages
library(data.table)
library(tidyverse)
library(sf)

### Clean the data ---------------------------------------------

# Create a contianer
data <- vector("list", length = 3)

for(i in 1:3){
  
  cat("Round", i, "out of ", 3, "!")
  
  # First group of years
  d <- load_data(i)
  d <- clean_data(d)
  
  # Assign dataset to the list
  data[[i]] <- d
}

# Combine the data
basic <- rbindlist(data)

# Save the basic data
save(basic, file = "data/basic.Rda")

### Create population data -------------------------------------

# Filter only the finnish men
basic <- basic[ori == 12 | ori == 11, ]

# Filter columns
pop <- basic[(is.na(yod) | yod <= year) & age <= 55 , .(year, edu, sex, age, res)]
rm(basic)

# Aggregate the number of people
pop <- pop[ ,.(Total = .N), by = .(year, edu, sex, age, res)]

# Save the ata
save(pop, file = "data/pop.Rda")

### Create regional data --------------------------------------

# Load the data
reg <- fread("D:/metadata/classifications/region/alueet22.csv")

# Select the variables
reg <- reg[ , .(kunta, kunta22, Seutukunta)]

# Replace kunta with missing
reg <- reg[ , kunta := ifelse(kunta22 == kunta, NA_character_, kunta )]

# Generate id
reg <- reg[, id := .I]

# Replace kunta with missing
reg <- reg[ , kunta := ifelse(is.na(kunta), kunta22, kunta)]

# Filter if kunta is not missing
reg <- reg[!is.na(kunta), ]

# Rename variables
reg <- reg[ , .(res = kunta, reg = Seutukunta)]

# Make as integer
reg$res <- as.integer(reg$res)

# Save the regional data
save(reg, file = "data/regions.Rda")


#### Create the map data ---------------------------------------

# Load the shape files
shape <- st_read("D:/metadata/classifications/shapefiles/kunta4500k_2020Polygon.shp")

# Load the Regional data
reg <- read.csv("D:/metadata/classifications/region/alueet22.csv", encoding = "latin1")
pop <- read_dta("data/pop.dta")

# Change the encoding
Encoding(shape$nimi) <- "latin1"

# Bind the regional data with the shape data
shape <- full_join(shape, reg, by = c("nimi" = "Kunta0"), suffix = c("_shp", "_reg"))

# Rename the variables
shape <- dplyr::rename(shape, Res = kunta_reg, Kunta = nimi, Reg = Mkkoodi) 

# Create the data
shape <- subset(shape, select = c(Res, Kunta, Reg, Seutukunta, geometry))

# Seutukunta
shape_seutukunta <- shape  %>% 
  group_by(Seutukunta, Reg) %>% summarize(.groups = "drop")  

#Duplicates
duplicates <- shape$Kunta[duplicated(shape$Kunta)]

# Create a data for Mariehamn
shape$Res[shape$Kunta == "Maarianhamina - Mariehamn"] <- 13
shape$Res[shape$Kunta == "Honkajoki"] <- 37

# Save the data
save(shape_seutukunta, file = "data/map_seutukunta.Rda")
save(shape, file = "data/map_kunta.Rda")

### END ##########################################################