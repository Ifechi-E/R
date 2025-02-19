library(tidyverse)
library(AER)
library(sandwich)
library(lmtest)
library(ggplot2)
library(modelsummary)

options(scipen =4)

### Load data + Clean Data ##################################
setwd('/Users/ifechiekekwe/Documents/Uni work/4th Year/Econ/Econometrics/Topic 2 Labs/Assignment 2 (DiD Topic)-20241029')
data <- read.csv('drinkhh_city_panel.csv')
data <- subset(data, year != 2022)

  ### Adoption Year Table ################
treatment_counts <- data %>%
  group_by(treatment_start) %>%
  summarize(cities_adopted = n_distinct(city)) %>%
  arrange(treatment_start)
# Print the table
print(treatment_counts)

data <- subset(data, data$treatment_start > 2014)#zero observations dropped. Data Set is complete with no missing values
view(data)
### Setting up variables and Descriptive Stats ###########################

data$treat <- ifelse(data$treatment_start ==2017,1,0)

data$post <- ifelse(data$year >=2017,1,0)

data$D <- ifelse(data$treatment_start == 2017 & data$year >= 2017, 1,0)

### Plot for intuition + Visual check of common Trends assumption ######### 
ggplot(data, 
       aes(x = year, y = drinkhh, colour = factor(treat), group = factor(treat))) +
  stat_summary(geom = 'line') +
  geom_vline(xintercept = 2017) +
  labs(
    title = "Alchol Related Hospitalisations over Time",
    x = "Years",
    y = "Alcohol related Hospitalisations per 10k of population"
  ) +
  theme_minimal()

### Regressions  #############################
First <- lm(drinkhh ~ treat + post + treat*post, data = data)
summary(First)

Second <- lm(drinkhh ~ factor(city) + factor(year) + D, data = data)
summary(Second)
### The above should be the same because we are just allowing for each city to have its own linear coefficient

# allowing for unobserved linear variations across cities. (Group Linear Trends)
Third <- lm(drinkhh ~ year*factor(city) + factor(year) + factor(city) + D, data = data)
summary(Third)
### Too intensive on the data and removes almost all other variation

###############################################################################
### Final Specification + Robustness Check:
###############################################################################
Final <- lm(drinkhh ~ factor(city) + factor(year) + D, data = data)
Final_coefs <- data.frame(summary(Final)$coefficients)
coi_indices <- which(!startsWith(row.names(Final_coefs),'factor(city)'))
FinalShort <- Final_coefs[coi_indices,]
FinalShort

# Robust Standard errors (clustering cities on a national level)
Final_R <- coeftest(Final, vcov = vcovCL, cluster =~city)
FinalShort_R <- Final_R[coi_indices,]
FinalShort_R
###############################################################################
### Event Study ###################
    # Create Leads #####
j = 2017
for(k in 0:4) {
  # assign leads 0-4
  print(j)
  if(k < 4) {
    data[[paste0("Lead_",k)]] <- ifelse(data$treat==1 & data$year==j, 1,0)
  } 
# assign last lead 
  else {   
    data[[paste0("Lead_",k)]] <- ifelse(data$treat==1 & data$year<=j, 1,0)
  }
  j = j - 1
}

    # Create Lags ####
j=2018
for(k in 1:3) {
  if(k < 3) {
  print(j)
  # assign lags 1-4
  data[[paste0("Lag_",k)]] <- ifelse(data$treat==1 & data$year==j, 1, 0)
}
#assign last lag
else {   
    data[[paste0("Lag_",k)]] <- ifelse(data$treat==1 & data$year>=j, 1,0)
  }
  j = j + 1
} 

  # Run event study ######
eventdd <- lm(drinkhh ~ Lead_4 + Lead_3 + Lead_2 + Lead_0 + Lag_1 + Lag_2 + Lag_3
                + factor(year) + factor(city), data=data)
# don't report the group FE coefs to declutter results window
event_coefs <- data.frame(summary(eventdd)$coefficients)
coi_indices <- which(!startsWith(row.names(event_coefs), 'factor(city)'))

# Extract all coefficients and standard errors
event_coeffs <- coef(eventdd)
event_se <- sqrt(diag(vcov(eventdd)))

# Filter only the coefficients starting with "Lead_" or "Lag_"
event_se_filtered <- event_se[grep("Lead_\\d+|Lag_\\d+", names(event_se))]
event_coeffs_filtered <- event_coeffs[grep("Lead_\\d+|Lag_\\d+", names(event_coeffs))]

# Extract event time indicators as specified by the order
event_time_names <- names(event_coeffs_filtered)

# Manually define event time ordering by
# Creating a vector to specify the order
event_time_order <- c("Lead_4", "Lead_3", "Lead_2", "Lead_1", "Lead_0", 
                      "Lag_1", "Lag_2", "Lag_3")

# Reorder coefficients and standard errors according to event_time_order
event_coeffs_ordered <- event_coeffs_filtered[event_time_order]
event_se_ordered <- event_se_filtered[event_time_order]

# Define a numeric event time variable aligned with this specific order
# (e.g., Furthest lead -> Event time <- Furthest lag)
event_time <- c(-4, -3, -2, -1, 0, 1, 2, 3)

# Create Event Study df
ES_df <- data_frame(
  event_time = event_time,
  coefficient = event_coeffs_ordered,
  std_error = event_se_ordered
)

# Add confidence intervals
ES_df$ci_upper <- ES_df$coefficient + 1.96 * ES_df$std_error
ES_df$ci_lower <- ES_df$coefficient - 1.96 * ES_df$std_error

# Plot using ggplot2
ggplot(ES_df, aes(x = event_time, y = coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(x = "Event Time", y = "Coefficient Estimate",
       title = "Coefficient Plot Over Event Time") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +    # Add horizontal line at y = 0
  geom_vline(xintercept = -0, linetype = "dotted", color = "blue") +   # Add vertical line just before event time 0 (e.g., at x = -0.5)
  theme_minimal()

###################################################################
mean(data$drinkhh[data$treatment_start == 2017 & data$year <= 2017])

ggplot(data, 
       aes(x = year, y = drinkhh, colour = factor(city))) +
  stat_summary(geom = 'line') +
  geom_vline(xintercept = 2017) +
  labs(
    title = "Alchol Related Hospitalisations over Time",
    x = "Years",
    y = "Alcohol related Hospitalisations per 10k of population"
  ) +
  theme_minimal()