##################################################
#         Birth squeezes in Finland              #
#         Henrik-Alexander Schubert              #
#             Spatial Aggregation                #
##################################################

#####   Settings  ##########################

rm(list = ls())

# Load the packages
library(tidyverse)
library(sf)
library(data.table)

# Load the functions
source("functions/functions.R")

# Load the shape data
load("data/map_kunta.Rda")
load("data/map_seutukunta.Rda")

### 2. Spatial Aggregation ------------------------

# Define the different threshholds
thresholds <- c(0, 9.2, 20)

for (i in thresholds) {
  
  # Create a variable  
  threshold <- i
  
  # Specify a threshold
  units(threshold) <- "km"
  
  # Estimate the distances
  distance <- st_distance(shape, shape)
  
  # Change to km
  units(distance) <- "km"
  
  ### Estimate the matrix
  if (i == 0) {
    near <- st_join(shape, shape, st_touches, suffix = c("", "_near"))
    near$distance <- threshold
      } else {
    near <- st_join(shape, shape, st_is_within_distance, dist = threshold, suffix = c("", "_near"))
    # Attach the distances
    near$distance <- distance[distance <= threshold]
  }
  
  # Group number
  near <- near %>%
    filter(!is.na(distance)) %>% 
    mutate(Group = as.numeric(factor(Kunta))) %>%
    group_by(Res) %>% 
    mutate(n = n()) %>%
    select(Res, Kunta, Res_near, distance, n) %>% 
    st_drop_geometry()
  
  # Assign the thresholds 
  assign(paste0("near_", i), near)

}


### 3. Comparisons ---------------------------------------------

# Make a list 
near <- list(near_0, near_9.2, near_20)
names(near) <- c(1, 2, 3)

# Combine the result
near <- bind_rows(near, .id = "dist")

# Order the results
near$dist <- factor(near$dist, labels = c("1" = "0km","2" = "9.2km","3" = "20km"), order = T)

# Group the results
distance <- near %>% 
  group_by(dist, Res) %>% 
  summarise(distance = as.numeric(mean(distance)),
            n = n(),
            .groups = "drop") 

# Join the data
distance <- inner_join(shape, distance, by = "Res")

# Plot the number
ggplot(distance) +
  geom_sf(aes(fill = n)) +
  facet_wrap( ~ dist) +
  scale_fill_gradient(low = "white", high = "darkred", name = "N municipalities:") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15, family = "serif", face = "bold"),
        legend.key.width = unit(2, "cm"),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(last_plot(), filename = "figures/spat_dist_mun.pdf", height = 15, width = 25, unit = "cm")


# Plot the number
ggplot(distance) +
  geom_sf(aes(fill = distance)) +
  facet_wrap( ~ dist) +
  scale_fill_gradient(low = "white", high = "darkred", name = "Mean distance:") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15, family = "serif", face = "bold"),
        legend.key.width = unit(2, "cm"),
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave(last_plot(), filename = "figures/spat_distance_mun.pdf", height = 15, width = 25, unit = "cm")

# Summarise the result
distance %>% 
  group_by(dist) %>% 
  summarise(mean = mean(n), min = min(n), max = max(n))

### 3. Merge with pop data ----------------------

# Load the population data
load("data/pop.Rda")

# Make pop data numeric
pop$res <- as.integer(pop$res)

# Combine pop and nb
pop_near <- inner_join(near, pop, by = c("Res_near" = "res"))

# Make it to data.table
pop_near <- as.data.table(pop_near)

# Group the data and summarise
pop_near <- pop_near[, .(Total = sum(Total)), by = .(sex, year, age, edu, Res, dist)]

# Save the result
save(pop_near, file = "data/pop_near.Rda")

#########           END       #####################