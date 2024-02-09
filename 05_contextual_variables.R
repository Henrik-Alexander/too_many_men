##################################################
#         Birth squeezes in Finland              #
#         Henrik-Alexander Schubert              #
#           Contextual variables                 #
##################################################

#####   Settings  ##########################

rm(list = ls())

# Load the packages
source("functions/functions.R")

# Load the packages
library(data.table)
library(tidyverse)
library(lubridate)
library(patchwork)

# Set the path
path_data <- "D:/ready-made/"

# Filenames
files <- c("FOLK_perus_8800a/folk_19872000_tua_perus21tot_1.csv",
           "FOLK_perus_0110a/folk_20012010_tua_perus21tot_1.csv",
           "FOLK_perus_11a/folk_20112020_tua_perus22tot_1.csv")

# Load the regions
load("data/regions.Rda")

#### 1. Create contextual variables --------------

# Create a container
context_data <- vector("list", length = length(files))

for (i in seq_along(files)){

  cat("Iteration:", i, "\n")
  
# Load the data
filename <- paste0(path_data, files[i])
d <- fread(filename)

# Select variables
d <- d[, .(vuosi, ika, kunta, kturaha_k, ptoim1, ututku_aste, tyrtuo_k)]

# Rename variables
d <-  setnames(d,
               old = c("vuosi", "ika", "kunta", "kturaha_k", "ptoim1", "ututku_aste"),
               new = c("year", "age", "res", "income", "activity", "education"))

# Join with regions
d <- inner_join(d, reg, by = "res")

# Select the age range
d <- d[age > 18 & age <= 65, ]

# Create binary indicators
d$tertiary <- ifelse(d$education %in% c(5, 6, 7, 8), 1, 0)
d$income <- d$income / 12
d$unemployed <- ifelse(d$activity == 12, 1, 0)
d <- d[ , poverty_th := mean(income, na.rm = T)/2, by = year]
d$poor <- ifelse(d$income < d$poverty_th, 1, 0)

# Estimate the summary indicators
d <- d[ , .(unempl_rate = mean(unemployed, na.rm = T) * 100, 
            mean_income = mean(income, na.rm = T),
            share_tertiary = mean(tertiary, na.rm = T) * 100,
            poverty = mean(poor, na.rm = T) * 100),
        by = .(reg, year)]

# Assign the result
context_data[[i]] <- d
rm(d)

}

# Combine the context data
context_data <- bind_rows(context_data)

# Save the data
save(context_data, file = "data/context_data.Rda")

### Plot maps ---------------------------------------------------

library(sf)

# Load the shape files
shape <- st_read("D:/metadata/classifications/shapefiles/kunta4500k_2020Polygon.shp")

# Load the Regional data
reg <- read.csv("D:/metadata/classifications/region/alueet22.csv", encoding = "latin1")

# Change the encoding
Encoding(shape$nimi) <- "latin1"

# Bind the regional data with the shape data
shape <- full_join(shape, reg, by = c("nimi" = "Kunta0"), suffix = c("_shp", "_reg"))

# Rename the variables
shape <- dplyr::rename(shape, reg = Seutukunta)  
shape <- shape %>%
  group_by(reg) %>%
  summarize()

# Join with data
cd <- inner_join(shape, context_data, by = "reg")

# Plot the unemployment rate
plot_map <- function (data = cd, label = "Unemployment (%)", variable = unempl_rate, years = c(1996, 2005, 2018), file = "unemp_rate") {
  tmp <- data %>% filter(year %in% years)
  res <- ggplot(data = tmp) +
    geom_sf(aes(fill = {{variable}}), size = .2, colour = "lightgrey") +
    facet_wrap( ~ year) +
    scale_fill_viridis_c(name = label) +
    theme(legend.key.width = unit(2, "cm"),
          legend.key.height = unit(.2, "cm"),
          strip.background = element_blank(),
          strip.text = element_text(size = 15, face = "bold"),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  ggsave(res, filename = paste0("figures/map_context", file, ".pdf"), height = 15, width = 20, unit = "cm")
  return(res)
}

# Create the plots
plot_unempl <- plot_map()
plot_income <- plot_map(label = "Mean income (Euro)", variable = mean_income, file = "mean_income")
plot_tertia <- plot_map(label = "Share tertiary (%)", variable = share_tertiary, file = "share_teriary")
plot_poverty <- plot_map(label = "Share poverty (%)", variable = poverty, file = "share_poverty")


# Plot them together
plot_unempl 
plot_income
plot_tertia
plot_poverty

### END ################################################
