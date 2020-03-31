# Update the daily county of cases in ACC and sorrounding area
# writes to "primary and secondary counties cases.csv" and
#   "primSecNewCasesDaily.csv"
source("Code/updateDailyCountyCounts.R")

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
# 15 simulations of the base social distancing model
base_mod <- read_rds(paste0("Models/social_distance_base_", latest_date))
base_hosp_mod <- model_hospitalizations(base_mod)
base_hosp_mod <- summarise_model_hospitalizations(base_hosp_mod)
base_hosp_mod <- hospital_capacity(base_hosp_mod)

# Plot results
dailyCases <- read_csv("Data/primSecNewCasesDaily.csv")
dailyCases$secondary <- rowSums(dailyCases[,2:18])
plot.model.acc(outSD,  dailyCases$date[1:which(dailyCases$date == Sys.Date())], 
               dailyCases$secondary[1:which(dailyCases$date == Sys.Date())],
               log='y', title='Baseline Model With Social Distancing')
plot_hospitalizations(base_hosp_mod, type = "cum")
plot_hospitalizations(base_hosp_mod, type = "capacity")
plot_hospitalizations(base_hosp_mod, type = "pct")

# Natural epidemic
epid_mod <- read_rds(paste0("Models/epidemic_base_", latest_date))
epid_hosp_mod <- model_hospitalizations(epid_mod)
epid_hosp_mod <- summarise_model_hospitalizations(epid_hosp_mod)
epid_hosp_mod <- hospital_capacity(epid_hosp_mod)

plot.model.acc(epid_mod,  dailyCases$date[1:which(dailyCases$date == Sys.Date())], 
               dailyCases$secondary[1:which(dailyCases$date == Sys.Date())],
               log='y', title='Natural Epidemic Model')
plot_hospitalizations(epid_hosp_mod, type = "cum")
plot_hospitalizations(epid_hosp_mod, type = "capacity")
#plot_hospitalizations(epid_hosp_mod, type = "pct")

