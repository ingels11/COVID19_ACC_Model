# Update the daily county of cases in ACC and sorrounding area
# writes to "primary and secondary counties cases.csv" and
#   "primSecNewCasesDaily.csv"
source("Code/updateDailyCountyCounts.R")

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

latest_date <- "2020-03-31"

# Social distancing works well
# 15 simulations of the base social distancing model
base_mod <- read_rds(paste0("Models/social_distance_base_", latest_date))
base_hosp_mod <- model_hospitalizations(base_mod)
base_hosp_mod <- summarise_model_hospitalizations(base_hosp_mod)
base_hosp_mod <- hospital_capacity(base_hosp_mod)

# Plot results
cumCases <- read_csv("Data/primary and secondary counties cases.csv")
cumCases$secondary <- rowSums(cumCases[,2:18])
cumCases %<>% filter(date <= Sys.Date())
plot.model.acc(base_mod,  cumCases$date, cumCases$secondary,
               log='y', title='Model With Excellent Social Distancing')
plot_hospitalizations(base_hosp_mod, type = "cum", 
                      title = "Model with Excellent Social Distancing (Total Hospitalization Count)")
ggsave(paste0("Plots/exc_socdist_hosp_total_", Sys.Date(), ".png"))
plot_hospitalizations(base_hosp_mod, type = "capacity",
                      title = "Model with Excellent Social Distancing (Current Hospitalization Count)")
ggsave(paste0("Plots/exc_socdist_hosp_current_", Sys.Date(), ".png"))
plot_hospitalizations(base_hosp_mod, type = "pct")

# Social distancing works not as well
# 15 simulations of the base social distancing model
upper_mod <- read_rds(paste0("Models/social_distance_upper_", latest_date))
upper_hosp_mod <- model_hospitalizations(upper_mod)
upper_hosp_mod <- summarise_model_hospitalizations(upper_hosp_mod)
upper_hosp_mod <- hospital_capacity(upper_hosp_mod)

# Plot results
plot.model.acc(upper_mod,  cumCases$date, cumCases$secondary,
               log='y', title='Model With Average Social Distancing')
plot_hospitalizations(upper_hosp_mod, type = "cum", 
                      title = "Model with Average Social Distancing (Total Hospitalization Count)")
ggsave(paste0("Plots/avg_socdist_hosp_total_", Sys.Date(), ".png"))
plot_hospitalizations(upper_hosp_mod, type = "capacity",
                      title = "Model with Average Social Distancing (Current Hospitalization Count)")
ggsave(paste0("Plots/avg_socdist_hosp_current_", Sys.Date(), ".png"))


# Natural epidemic
epid_mod <- read_rds(paste0("Models/epidemic_base_", latest_date))
epid_hosp_mod <- model_hospitalizations(epid_mod)
epid_hosp_mod <- summarise_model_hospitalizations(epid_hosp_mod)
epid_hosp_mod <- hospital_capacity(epid_hosp_mod)

plot.model.acc(epid_mod, cumCases$date, cumCases$secondary,
               log='y', title='Natural Epidemic Model')
plot_hospitalizations(epid_hosp_mod, type = "cum", 
                      title = "Model with Poor Social Distancing (Total Hospitalization Count)")
ggsave(paste0("Plots/poor_socdist_hosp_total_", Sys.Date(), ".png"))
plot_hospitalizations(epid_hosp_mod, type = "capacity",
                      title = "Model with Poor Social Distancing (Current Hospitalization Count)")
ggsave(paste0("Plots/poor_socdist_hosp_current_", Sys.Date(), ".png"))


