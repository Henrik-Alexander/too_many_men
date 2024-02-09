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
load("data/cum_data_1.Rda")
load("data/fixed_effects_1.Rda")
load("data/sibling_effects_1.Rda")

# Load the functions
source("functions/Functions.R")
source("functions/models.R")

## Structure:
# 1. Univariate statistics
# 2. Bivariate statistics
# 3. Graphs

#### 1. Univariate statistics -----------------------

# Making histograms of time-varying variables in the fe data
a <- histo(basic, year, "years", bins = 33)
b <- histo(basic, yob, "cohorts")
c <- histo(basic, inc, "income")
e <- histo(basic, unempl_rate, "regional unemployment rate")

# Assemble the plots
dis_fe <- (a + b ) / (c + e) + plot_annotation(title = "Distribution of continuous variables in fixed effects dataset",
                                               tag_levels = "A", tag_suffix = ")")


# Making histograms of time-varying variables in the fe data
a <- histo(d2, age, "max age")
b <- ggplot(d2, aes(cohort)) + geom_bar(stat = "count", fill = "navyblue", colour = "black") + scale_y_continuous(expand = c(0,0)) + ggtitle("Cohort")
c <- ggplot(d2, aes(income)) + geom_bar(stat = "count", fill = "navyblue", colour = "black") + scale_y_continuous(expand = c(0,0)) + ggtitle("Income Quantiles")
e <- histo(d2, unemployment, "regional unemployment (%)") + scale_x_continuous(labels = scales::percent)

# Assemble the plots
dis_cum <- (a + b ) / (c + e) + plot_annotation(title = "Distribution of continuous variables in cumulative dataset",
                                                tag_levels = "A", tag_suffix = ")")


# Making histograms of time-varying variables in the FE data
a <- ggplot(d3, aes(bo)) + geom_bar(stat = "count", fill = "navyblue", colour = "black") + scale_y_continuous(expand = c(0,0)) + ggtitle("Birth Orders")
b <- ggplot(d3, aes(cohort)) + geom_bar(stat = "count", fill = "navyblue", colour = "black") + scale_y_continuous(expand = c(0,0)) + ggtitle("Cohort")
c <- ggplot(d3, aes(income)) + geom_bar(stat = "count", fill = "navyblue", colour = "black") + scale_y_continuous(expand = c(0,0)) + ggtitle("Income Quantiles")
e <- histo(d3, unemployment, "regional unemployment (%)") + scale_x_continuous(labels = scales::percent)

# Assemble the plots
dis_sib <- (a + b ) / (c + e) + plot_annotation(title = "Distribution of continuous variables in sibling dataset",
                                                tag_levels = "A", tag_suffix = ")")

### Save the plots
ggsave(dis_fe, filename = "figures/distr_fe.pdf")
ggsave(dis_cum, filename = "figures/distr_cum.pdf")
ggsave(dis_sib, filename = "figures/distr_sib.pdf")

### Plot childlessness in the data

# Plot the childlessness
a <- basic %>%
  group_by(id) %>% 
  summarise(childless = unique(pc), .groups = "drop") %>% 
  as_tibble() %>% 
  ggplot(data = ., aes(childless, ..prop..)) + 
         geom_bar(fill = "navyblue") + 
  xlab("Childless") + ylab("Percentage") + 
  ggtitle("FE data") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.8), labels = scales::percent) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Father", "Childless")) 

# Plot 
b <- ggplot(d2, aes(childless, ..prop..)) + 
  geom_bar(fill = "navyblue") + 
  xlab("Childless") + ylab("Percentage") + 
  ggtitle("Cumulative data") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.8), labels = scales::percent) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Father", "Childless")) 


# Plot 
c <- ggplot(d3, aes(childless, ..prop..)) + 
  geom_bar(fill = "navyblue") + 
  xlab("Childless") + ylab("Percentage") + 
  ggtitle("Sibling data") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.8), labels = scales::percent) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Father", "Childless")) 

# Assemble the plot
plot <- a + b + c + plot_annotation(title = "Distribution of childlessness across data sets",
                            tag_levels = "A", tag_suffix = ")")

ggsave(plot, filename = "Figures/childlessness_data.pdf", height = 15, width = 22, unit = "cm")

### 2. Summary tables: Indiators ----------------------------------------

# Indicators
vars <- c("sr_near_9.2 km", "pr_near_9.2 km", "ar_near_9.2 km", "sr", "pr", "ar")
basic[, ..vars] %>%
  stargazer(title = "Availability indicators in fixed effects data", 
            style = "demography",
            covariate.labels = c("Sex ratio (near)",
                                 "Preference ratio (near)",
                                 "Availability ratio (near)",
                                 "Sex ratio",
                                 "Preference ratio",
                                 "Availability ratio"),
            out = "Results/Summary/avprlability_fe.tex")


# Summary
d2 %>% select(sr_near, pr_near, ar_near, sr, pr, ar) %>%
  stargazer(title = "Avprlability indicators in cumulative data", 
            style = "demography",
            covariate.labels = c("Sex ratio (near)",
                                 "Preference ratio (near)",
                                 "Availability ratio (near)",
                                 "Sex ratio",
                                 "Preference ratio",
                                 "Availability ratio"),
            out = "Results/Summary/avprlability_cum.tex")

# Summary
d3 %>% select(sr_near, pr_near, ar_near, sr, pr, ar) %>%
  stargazer(title = "Avprlability indicators in cumulative data", 
            style = "demography",
            covariate.labels = c("Sex ratio (near)",
                                 "Avprlability ratio (near)",
                                 "Avprlability ratio (near)",
                                 "Sex ratio",
                                 "Avprlability ratio",
                                 "Avprlability ratio"),
            out = "Results/Summary/avprlability_sib.tex")

## Main variables

# Indicators
basic[, .(birth, age, yob, ar_near, pr_near, sr_near, inc, edu = as.numeric(as.factor(edu)), act)] %>%
  stargazer(title = "Descriptives of indicators in FE data", 
            style = "demography",
            covariate.labels = c("Childbirth",
                                 "Age of fatherhood",
                                 "Cohort",
                                 "Avprlability ratio (near)",
                                 "Avprlability ratio (near)",
                                 "Sex ratio (near)",
                                 "Income",
                                 "Education",
                                 "Activity"),
            out = "Results/Summary/summary_fe.tex")


# Summary
d2 %>% mutate(edu = as.numeric(as.factor(edu)),
              ar = exp(`ar_near_9.2 km`),
              income = as.numeric(income),
              educ_comp = as.numeric(educ_comp),
              inequality = as.numeric(inequality))  %>% 
  select(childless, age, yob, ar, income, edu, unemployed, unemployment, educ_comp, inequality) %>%
  as_tibble() %>% 
  stargazer(title = "Descriptives of indicators in cumulative data", 
            style = "demography",
            covariate.labels = c("Childlessness",
                                 "Max age",
                                 "Cohort",
                                 "Availability ratio (near)",
                                 "Income",
                                 "Education",
                                 "Duration unemployment",
                                 "% Unemployed",
                                 "Educational Composition",
                                 "Income inequality"),
            out = "Results/Summary/summary_cum.tex")


# Summary
d3 %>% mutate(edu = as.numeric(as.factor(edu)))  %>%
  select(childless, age, yob, bo, ar_near, pr_near, sr_near, income, edu, unemployed, unemployment,educ_comp, inequality) %>%
  stargazer(title = "Descriptives of indicators in sibling data", 
            style = "demography",
            covariate.labels = c("Childlessness",
                                 "Max age",
                                 "Cohort",
                                 "Birth order",
                                 "Availability ratio (near)",
                                 "Preference ratio (near)",
                                 "Sex ratio (near)",
                                 "Income",
                                 "Education",
                                 "Duration unemployment",
                                 "% Unemployed",
                                 "Educational Composition",
                                 "Income inequality"),
            out = "Results/Summary/summary_sib.tex")


### 3. Correlations between avprlability indicators -------------------------

# Indicators
basic[, .(sr_near, pr_near, ar_near, sr, pr, ar)] %>%
  cor(use = "pairwise") %>% 
  stargazer(title = "Correlation availability indicators in fixed effects data", 
            summary = F,
            style = "demography",
            out = "Results/Correlation/cor_avprlability_fe.tex")



# Correlation
d2 %>% select(sr_near, pr_near, ar_near, sr, pr, ar) %>% 
  cor(use = "pairwise") %>% 
  stargazer(title = "Correlation avprlability indicators in cumulative data", 
            style = "demography",
            summary = F,
            out = "Results/Correlation/cor_avprlability_cum.tex")


# Correlation
d3 %>% select(sr_near, pr_near, ar_near, sr, pr, ar) %>% 
  cor(use = "pairwise") %>% 
  stargazer(title = "Correlation of avprlability indicators in cumulative data", 
            style = "demography",
            summary = F,
            out = "Results/Correlation/cor_avprlability_sib.tex")

### Development of childlessness over time -----------------------------

# Load the data 
load("data/childless_trend.Rda")

# Filter the data
dta <- as.data.frame(basic[age == 45 & yob <= 1975, mean(pc, na.rm = T), by = .(yob) ])

# Plot
plot_childless <- ggplot(dta, aes(yob, V1)) + 
  geom_line(size = 1) + 
  geom_point(size = 3, col = "white") + 
  geom_point(size = 1) + 
  labs(title = "Ultimate male childlessness (Age 45)")+ 
  xlab("Cohort") + ylab("Male Childlessness") + 
  scale_y_continuous(limits = c(0.15, 0.3), labels = scales::percent)

# Save the plot
ggsave(plot_childless, filename = "Figures/trend_male_childless.pdf", units = "cm", width = 12, height = 10)

# Filter the data
dta <- as.data.frame(basic[age == 45 & yob <= 1975, mean(pc, na.rm = T), by = .(yob, edu) ])

# Plot
plot_childless <- ggplot(dta, aes(yob, V1, colour = edu, shape = edu)) + 
  geom_line(size = 1) + 
  geom_point(size = 3, col = "white") + 
  geom_point(size = 2) + 
  labs(title = "Ultimate male childlessness (Age 45)")+ 
  xlab("Cohort") + ylab("Male Childlessness") + 
  scale_y_continuous(limits = c(0.1, 0.45), breaks = scales::pretty_breaks(), labels = scales::percent) +
  scale_x_continuous(expand = c(0, 0.3), breaks = scales::pretty_breaks()) +
  scale_color_brewer(name = "Education", palette = "Set1") +
  scale_shape_discrete(name = "Education", solid = T )

# Save the plot
ggsave(plot_childless, filename = "Figures/trend_male_childless_edu.pdf", units = "cm", width = 20, height = 15)

### Summarise the indicators -----------------------------------------

# Load the avprlability information
load("data/ar.Rda")
load("data/ar_near.Rda")
load("data/pr.Rda")
load("data/pr_near.Rda")
load("data/sr_near.Rda")
load("data/pop_sr.Rda")

# Plot the avprlability ratio
ggplot(subset(ar_near,year %in% c(1987, 2000, 2019)), aes(x = ar_near, fill = year)) +
geom_histogram(bins = 20) +
facet_wrap( ~ year) +
geom_vline(xintercept = 1, col = "firebrick", linetype = "dashed") +
scale_x_continuous(trans = "log", breaks = c(0.5, 1, 2)) +
scale_y_continuous(limits = c(0, 22000), expand = c(0, 0)) +
scale_fill_gradient(low = MPIDRgreen, high = MPIDRyellow ) +
xlab("Avprlability ratio") +
guides(fill = "none")

# Save the result
ggsave(last_plot(), filename = "figures/ar_near_histo.pdf")

# Plot the sex ratio
ggplot(subset(sr_near,year %in% c(1987, 2000, 2020)), aes(x = sr_near, fill = year)) +
geom_histogram(bins= 30) +
facet_wrap( ~ year) +
geom_vline(xintercept = 1, col = "firebrick", linetype = "dashed") +
scale_x_continuous(trans = "log", breaks = c(0.5, 1, 2)) +
scale_y_continuous(limits = c(0, 10000), expand = c(0, 0)) +
scale_fill_gradient(low = MPIDRgreen, high = MPIDRyellow ) +
xlab("Sex ratio") +
guides(fill = "none")


# Save the result
ggsave(last_plot(), filename = "figures/sr_near_histo.pdf")

# Plot the sex ratio
ggplot(subset(pr_near,year %in% c(1987, 2000, 2020)), aes(x = pr_near, fill = year)) +
geom_histogram(bins = 25) +
facet_wrap( ~ year) +
geom_vline(xintercept = 1, col = "firebrick", linetype = "dashed") +
scale_x_continuous(trans = "log", breaks = c(0.2, 0.5, 1, 2, 5)) +
scale_y_continuous(limits = c(0, 10000), expand = c(0, 0)) +
scale_fill_gradient(low = MPIDRgreen, high = MPIDRyellow ) +
xlab("Preference ratio") +
guides(fill = "none")

# Save the result
ggsave(last_plot(), filename = "figures/sr_near_histo.pdf")


# Compare the indicators ----------------------------------------

# Plot the different distributions
a <- as_tibble(basic) %>% 
  select(matches("^[a-z]r")) %>% 
  ggpairs()
ggsave(a, filename = "figures/distance_comp_fe.pdf", height = 40, width = 40, unit = "cm")

# Plot the different distributions
b <- d2 %>% 
  select(matches("^[a-z]r")) %>% 
  ggpairs()
ggsave(b, filename = "figures/distance_comp_log.pdf", height = 40, width = 40, unit = "cm")


# Plot the different distributions
c <- d3 %>% 
  select(matches("^[a-z]r")) %>% 
  ggpairs()
ggsave(c, filename = "figures/distance_comp_sib.pdf", height = 40, width = 40, unit = "cm")


# Distribution of indicators
variables <- names(d2)[str_detect(names(d2), pattern = "^[a-z]r")]

# Plot the distribution
data <- d2 %>% 
  select(matches(variables)) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(indicator = str_sub(name, 1, 2),
         distance = str_extract(name, "[0-9].*"),
         distance = if_else(is.na(distance), "Seutukunta", distance)) 
ggplot(subset(data, distance != "Seutukunta"),  aes(exp(value), fill = distance)) +
  geom_density(alpha = 0.3) +
  geom_density(data = subset(data, distance == "Seutukunta"), colour = "black", fill = "white") +
  facet_wrap( ~ indicator, scales = "free") +
  scale_x_continuous(trans = "log", breaks = scales::pretty_breaks()) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(family = "serif", size = 15, face = "bold"))
  
ggsave(last_plot(), filename = "figures/distr_indicators_distance.pdf")

# Plot the distributions individually
variables <- set_names(variables)
plot_dist <- function (variable = "pr", data = data) {
  # Filter the variable
  data <- data %>% filter(indicator == variable)
  tmp <- ggplot(subset(data, distance != "Seutukunta"),  aes(exp(value), fill = distance)) +
    geom_density(alpha = 0.3) +
    geom_density(data = subset(data, distance == "Seutukunta"), colour = "black", fill = "white") +
    scale_x_continuous(variable, trans = "log", breaks = scales::pretty_breaks()) +
    scale_y_continuous(expand = c(0, 0))
 ggsave(tmp, filename = paste0("figures/distr_cum_", variable, ".pdf"))
}

# Apply the function
map(c("ar", "pr", "sr"),  ~ plot_dist(.x, data))

### END ##########################################
