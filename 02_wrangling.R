##################################################
#         Birth squeezes in Finland              #
#         Henrik-Alexander Schubert              #
#             Data wrangling                     #
##################################################

rm(list = ls())

# Load the packages
library(data.table)
library(tidyverse)
library(lubridate)

# Set the working directories
input <- "D:/ready-made"

#### 1. Demographic data -------------------------

# Load the data
demographics <- haven::read_dta("D:/d40/custom-made/al3_update/demographics.dta")
#demographics <- fread("D:/d40/custom-made/Demographics/demographics.rdata")

# Make it to a data table
demographics <- as.data.table(demographics)

# Rename the variables
demographics <- setnames(demographics,
                         c("shnro", "syntyv", "syntykk", "sukup", "kuolv", "kuolkk"),
                         c("cid", "yob", "mob", "sex", "yod", "mod"))

# Create a binary sex variable
demographics$sex <- ifelse(demographics$sex == 1, "Male", "Female")

# Select variables
demographics <- demographics[, .(cid, sex, yob, mob, yod, mod)]

# Save the output
save(demographics, file = "data/birth_dates.Rda")

######    2.  Kids data  ------------------------

# Load the data
load("data/basic.Rda")
load("data/regions.Rda")

# Make residence as character
reg$Res <- as.numeric(reg$Res)

# Select variables
basic2 <- basic[, .(id, yob, bpl, sex)]

# Drop duplicates
basic2 <- basic2[, head(.SD, 1), by = id]

# Count the births per year, sex and bpl
births <- as.data.frame(basic2[yob >= 1950 & !is.na(bpl), .N, by = .(yob, bpl, sex)])
rm(basic2)

# Combine birth information with regional information
births <- full_join(basic, reg, by = c("bpl" = "Res"))

# Save the birth counts
save(births, file = "data/birth_counts.Rda")

###  3. Create births data -------------------------

# The births data is used for estimating the partner preferences

# Keep the parents data
parents <- basic[ , .(year, id, sex, res, age)]

# Load the parent-child data
parent_child <- fread("D:/ready-made/FOLK_laps_70a/folk_19702020_tua_laps21_1.csv")

# Subset and rename the variables
oldnames <- c("shnro", "shnro_m", "shnro_f", "syntyv")
parent_child <- parent_child[, ..oldnames]
setnames(parent_child, old = oldnames, new = c("cid", "mother", "father", "yob_child"))

# Make it to long data
births <- melt(parent_child, id =c(1, 4), measure = c("mother", "father"), na.rm = T)

# Merge the data
births <- merge(births, parents, by.x = c("value", "yob_child"), by.y = c("id", "year"), all.x = T)
births <- births[value != "" & yob_child >= 1987, ]

# Make it to wide data
births <- dcast(births, cid + yob_child  ~ variable, value.var = c("sex", "res", "age"))

# Impute the maternal information if the paternal information is missing
births$res_father <- ifelse(is.na(births$res_father), births$res_mother, births$res_father)
births$age_father <- ifelse(is.na(births$age_father), births$age_mother+3, births$age_father)
births$sex_father <- "Male"
births$sex_mother <- "Female"

# Aggregate
births <- births[, .(births = .N), by = c("yob_child", "res_father", "age_father", "sex_father")]
setnames(births, c("yob_child", "res_father", "age_father", "sex_father"), c("year", "res", "age", "sex"))
births <- na.omit(births)

# Save the data
save(births, file = "data/birth_counts.Rda")
rm(births)

### Create the birth dates -----------------------

# Reshape the data
parent_child <- melt(parent_child, id.vars = c("cid", "yob_child"), measure.vars = c("mother", "father"), variable.name = "parent", value.name = "id")

# Sort the data
parent_child <- parent_child[order(id, yob_child), ]

# Remove missing parents
parent_child <- parent_child[id != "", ]

# Number the children
parent_child <- parent_child[, nch := 1:.N, by = id]

### Create the sibling data ----------------------------  

# Pivot wider
siblings <- dcast(parent_child, cid + yob_child  ~ parent, value.var = c("nch", "id"))

# Get the sibling identification  
siblings <- siblings[, sib := .GRP, by = id_mother]
siblings <- siblings[, sib := ifelse(is.na(id_mother), NA_integer_, sib)]
siblings <- siblings[, nos := .N, by = id_mother]

# Merge with demographics
siblings <- siblings[demographics, on = "cid"]

# Create birth order and select male siblings
siblings <- siblings[order(sib, yob), ]
siblings <- siblings[ , bo := 1:.N, by = sib]
siblings <- siblings[ , cohort := if_else(yob >= 1968 & yob <= 1975, 1, 0)]
siblings <- siblings[cohort == 1 & sex == "Male", ]
siblings <- siblings[ , count := .N, by = sib]
siblings <- siblings[count >= 2, .(cid, bo, sib)]

# Save sibling information
save(siblings, file = "data/sibling.Rda")

### Create the parent child data ------------------------------

# Load the data
parent_child <- parent_child[demographics, on = "cid"]

# Rename child's age
parent_child <- parent_child[, yob := NULL]

# Get the parent's age
parent_child <- merge(parent_child, demographics, by.x = "id", by.y = "cid", suffixes = c("_child", "_parent"))

# Estimate the age
parent_child[, age := floor((yob_child + (mob_child - 1) / 12) - (yob + (mob_parent - 1) / 12))]

# Get the parental specific information
moms <- parent_child[parent == "mother", ]
dads <- parent_child[parent == "father", ]
moms <-  dcast(moms, ... ~ parent, value.var = c("id", "age"))
dads <-  dcast(dads, ... ~ parent, value.var = c("id", "age"))

### Combine with regional data ----------------------------
# Get the parental education and residence

# Load the education data
load("data/regions.Rda")

# Make regions to integer
basic$res <- as.integer(basic$res)
reg$res <- as.integer(reg$res)

### Here, it would be possible to incorporate education into the analysis, by using the "basic.Rda" data, which is reduced to a year,
#education and id column and joined to the data
basic <- merge(basic, reg, by.x = "res", by.y = "res")
basic <- basic[, .(id, year, edu, reg)]
rm(parent_child, demographics)
moms <- merge(moms, basic, by.x = c("id_mother", "yob_child"), by.y = c("id", "year"), all.x = T)
dads <- merge(dads, basic, by.x = c("id_father", "yob_child"), by.y = c("id", "year"), all.x = T)

# Join the data
parent_child <- merge(moms, dads, by = c("cid", "sex_child", "yob_child", "mob_child", "yod_child", "mod_child"), suffixes = c("_mother", "_father"))

# Remove homosexual couples
parent_child <- parent_child[sex_parent_mother == "Female" | sex_parent_father == "Male", ]

# -> tested for data quality: father and mother variables for children are consistentr
parent_child <- parent_child[, .(cid, id_mother, yob_child, age_mother, reg_mother,
                                 edu_mother, id_father, age_father, reg_father,
                                 edu_father, nch_father, nch_mother, 
                                 yod_parent_father, yod_parent_mother)]

# Save the data
save(parent_child, file = "data/parent_child.Rda")

##################    END   ########################