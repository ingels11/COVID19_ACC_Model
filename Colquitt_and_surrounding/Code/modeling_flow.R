# Update the daily county of cases in ACC and sorrounding area
# writes to "primary and secondary counties cases.csv" and
#   "primSecNewCasesDaily.csv"
# source("Code/updateDailyCountyCounts.R")
library(ggplot2)
rm(list = ls())
source("Code/model_fncs.R")

# run models
# models found in athens_simulations_secondary_ga_scaling.R
# evaluate.model() -> model() -> onestep()
# save one or more models, will include most up to date counts if 
#   updateDailyCountyCounts.R has been run
# Should save model as .rds (with date as part of title) for further processing
#   * social_distance_base_

# analyze models
# run the following functions using the saved .rds files
# each produces an output that should be fed to the next
# model_hospitalizations()
# summarise_model_hospitalizations()
# hospital_capacity()

latest_date <- "2020-04-01"



# Plot results
cumCases <- read_csv("Data/primary and secondary counties cases.csv")
cumCases$secondary <- rowSums(cumCases[,2:18])
cumCases %<>% filter(date <= Sys.Date())

### Poor social distancing -----------------------------------------------------
epid_mod <- read_rds(paste0("Models/epidemic_base_", latest_date, ".rds"))
epid_hosp_mod <- model_hospitalizations(epid_mod)
epid_hosp_mod <- summarise_model_hospitalizations(epid_hosp_mod)
epid_hosp_mod <- hospital_capacity(epid_hosp_mod)
write_rds(epid_hosp_mod, paste0("Models/epidemic_base_hosp_", latest_date, ".rds"))

acc_natural <- plot.model.acc(epid_mod, cumCases$date, cumCases$secondary,
                              log='y', title='Natural Epidemic Model')
# ggsave(paste0("Plots/Athens_natural_epidemic_", Sys.Date(), ".png"), acc_natural)
plot.max.y <- return_maxvals(epid_mod)
plot.max.y <- plot.max.y[["max.y"]]

plot_hospitalizations(epid_hosp_mod, type = "cum", 
                      title = "Model with Poor Social Distancing (Total Hospitalization Count)")
ggsave(paste0("Plots/poor_socdist_hosp_total_", Sys.Date(), ".png"))
plot_hospitalizations(epid_hosp_mod, type = "capacity",
                      title = "Model with Poor Social Distancing (Current Hospitalization Count)")
ggsave(paste0("Plots/poor_socdist_hosp_current_", Sys.Date(), ".png"))

### Excellent social distancing ------------------------------------------------
# Social distancing works well
# 15 simulations of the base social distancing model
base_mod <- read_rds(paste0("Models/social_distance_base_", latest_date, ".rds"))
base_hosp_mod <- model_hospitalizations(base_mod)
base_hosp_mod <- summarise_model_hospitalizations(base_hosp_mod)
base_hosp_mod <- hospital_capacity(base_hosp_mod)
write_rds(base_hosp_mod, paste0("Models/social_distance_base_hosp_", 
                                latest_date, ".rds"))

plot.model.acc(base_mod,  cumCases$date, cumCases$secondary,
               log='y', title='Model With Excellent Social Distancing',
               max.y = plot.max.y)
plot_hospitalizations(base_hosp_mod, type = "cum", 
                      title = "Model with Excellent Social Distancing (Total Hospitalization Count)")
ggsave(paste0("Plots/exc_socdist_hosp_total_", Sys.Date(), ".png"))
plot_hospitalizations(base_hosp_mod, type = "capacity",
                      title = "Model with Excellent Social Distancing (Current Hospitalization Count)")
ggsave(paste0("Plots/exc_socdist_hosp_current_", Sys.Date(), ".png"))
#plot_hospitalizations(base_hosp_mod, type = "pct")

### Average social distancing --------------------------------------------------
# 15 simulations of the base social distancing model
upper_mod <- read_rds(paste0("Models/social_distance_upper_", latest_date, ".rds"))
upper_hosp_mod <- model_hospitalizations(upper_mod)
upper_hosp_mod <- summarise_model_hospitalizations(upper_hosp_mod)
upper_hosp_mod <- hospital_capacity(upper_hosp_mod)
write_rds(upper_hosp_mod, paste0("Models/social_distance_upper_hosp_", 
                                latest_date, ".rds"))

# Plot results
plot.model.acc(upper_mod,  cumCases$date, cumCases$secondary,
               log='y', title='Model With Average Social Distancing',
               max.y = plot.max.y)
plot_hospitalizations(upper_hosp_mod, type = "cum", 
                      title = "Model with Average Social Distancing (Total Hospitalization Count)")
ggsave(paste0("Plots/avg_socdist_hosp_total_", Sys.Date(), ".png"))
plot_hospitalizations(upper_hosp_mod, type = "capacity",
                      title = "Model with Average Social Distancing (Current Hospitalization Count)")
ggsave(paste0("Plots/avg_socdist_hosp_current_", Sys.Date(), ".png"))




