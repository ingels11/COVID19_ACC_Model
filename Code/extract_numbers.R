rm(list = ls())
library(readr)
library(dplyr)
library(magrittr)

latest_date <- "2020-04-01"
### Read in and Combine Scenario Models ----------------------------------------
# Add in marking of peak new cases and hospital capacity by scenario
poor_df <- read_rds(paste0("Models/epidemic_base_hosp_", 
                           latest_date, ".rds")) %>%
  mutate(scenario = "poor")
poor_df$max_new_hosp <- NA
poor_df$max_tot_hosp <- NA
poor_df$max_new_case <- NA
poor_df$max_new_hosp[max(which(poor_df$Hosp_new_high == max(poor_df$Hosp_new_high)))] <- 1
poor_df$max_tot_hosp[max(which(poor_df$Hosp_tot_high == max(poor_df$Hosp_tot_high)))] <- 1
poor_df$max_new_case[max(which(poor_df$Case_new == max(poor_df$Case_new)))] <- 1

avg_df <- read_rds(paste0("Models/social_distance_upper_hosp_", 
                          latest_date, ".rds")) %>%
  mutate(scenario = "average")
avg_df$max_new_hosp <- NA
avg_df$max_tot_hosp <- NA
avg_df$max_new_case <- NA
avg_df$max_new_hosp[max(which(avg_df$Hosp_new_high == max(avg_df$Hosp_new_high)))] <- 1
avg_df$max_tot_hosp[max(which(avg_df$Hosp_tot_high == max(avg_df$Hosp_tot_high)))] <- 1
avg_df$max_new_case[max(which(avg_df$Case_new == max(avg_df$Case_new)))] <- 1

exc_df <- read_rds(paste0("Models/social_distance_base_hosp_", 
                          latest_date, ".rds")) %>%
  mutate(scenario = "excellent")
exc_df$max_new_hosp <- NA
exc_df$max_tot_hosp <- NA
exc_df$max_new_case <- NA
exc_df$max_new_hosp[max(which(exc_df$Hosp_new_high == max(exc_df$Hosp_new_high)))] <- 1
exc_df$max_tot_hosp[max(which(exc_df$Hosp_tot_high == max(exc_df$Hosp_tot_high)))] <- 1
exc_df$max_new_case[max(which(exc_df$Case_new == max(exc_df$Case_new)))] <- 1

df <- bind_rows(poor_df, avg_df, exc_df) %>%
  mutate(scenario = factor(scenario, 
                           levels = c("poor", "average", "excellent"),
                           ordered = TRUE))
rm(poor_df, avg_df, exc_df)
df[is.na(df)] <- 0

### Extract Data ---------------------------------------------------------------
df %>%
  group_by(scenario) %>%
  summarise(total_cases = max(Case_tot),
            total_hosps_low = sum(Hosp_new_low),
            total_hosps_high = sum(Hosp_new_high),
            max_capacity_low = max(Hosp_tot_low),
            max_capacity_high = max(Hosp_tot_high),
            max_capacity_crit_low = max(Crit_tot_low),
            max_capacity_crit_high = max(Crit_tot_high))

# Determine peak dates (high scenario only)
df %>%
  filter(max_tot_hosp == 1) %>%
  select(scenario, Dates)

df %>%
  filter(max_new_case == 1) %>%
  select(scenario, Dates)
