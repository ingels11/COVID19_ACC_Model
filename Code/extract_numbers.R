rm(list = ls())
library(readr)
library(dplyr)
library(magrittr)

latest_date <- "2020-03-31"
### Read in and Combine Scenario Models ----------------------------------------
# Add in marking of peak new cases and hospital capacity by scenario
poor_df <- read_rds(paste0("Models/epidemic_base_hosp_", 
                           latest_date, ".rds")) %>%
  mutate(scenario = "poor")
max(which(poor_df$Hosp_case_high == max(poor_df$Hosp_case_high)))
max(which(poor_df$Hosp_capacity_high == max(poor_df$Hosp_capacity_high)))
max(which(poor_df$Case_new == max(poor_df$Case_new)))

avg_df <- read_rds(paste0("Models/social_distance_upper_hosp_", 
                          latest_date, ".rds")) %>%
  mutate(scenario = "average")
max(which(avg_df$Hosp_case_high == max(avg_df$Hosp_case_high)))
max(which(avg_df$Hosp_capacity_high == max(avg_df$Hosp_capacity_high)))
max(which(avg_df$Case_new == max(avg_df$Case_new)))

exc_df <- read_rds(paste0("Models/social_distance_base_hosp_", 
                          latest_date, ".rds")) %>%
  mutate(scenario = "excellent")
max(which(exc_df$Hosp_case_high == max(exc_df$Hosp_case_high)))
max(which(exc_df$Hosp_capacity_high == max(exc_df$Hosp_capacity_high)))
max(which(exc_df$Case_new == max(exc_df$Case_new)))

df <- bind_rows(poor_df, avg_df, exc_df) %>%
  mutate(scenario = factor(scenario, 
                           levels = c("poor", "average", "excellent"),
                           ordered = TRUE))
rm(poor_df, avg_df, exc_df)

### Extract Data ---------------------------------------------------------------
df %>%
  group_by(scenario) %>%
  summarise(total_cases = max(Case_tot),
            total_hosps = max(Hosp_tot_high),
            total_crit_low = max(Crit_tot_low),
            total_crit_high = max(Crit_tot_high),
            max_capacity = max(Hosp_capacity_high),
            total_deaths = max(Dead_tot_high))

# Determine peak dates

