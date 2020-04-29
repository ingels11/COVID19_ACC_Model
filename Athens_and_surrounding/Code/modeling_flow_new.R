# Update the daily county of cases in ACC and sorrounding area
# writes to "primary and secondary counties cases.csv" and
#   "primSecNewCasesDaily.csv"
# source("Code/updateDailyCountyCounts.R")
# This is being done every day by Ishaan/Nicholas
### Setup ----------------------------------------------------------------------
rm(list = ls())
if ("Athens_and_surrounding" %in% dir()) {
  source("Athens_and_surrounding/Code/model_fncs.R")
} else {
  source("Code/model_fncs.R")
} 
if ("Athens_and_surrounding" %in% dir()) setwd("Athens_and_surrounding")
library(ggplot2)

cumCases <- read_csv("Data/primary and secondary counties cases.csv")
cumCases$secondary <- rowSums(cumCases[, 2:18])
#cumCases %<>% filter(date <= Sys.Date())
# This corresponds to the most recent reported case information
# cumCases %<>% filter(date <= as.Date("2020-04-28"))


# run models
# models found in acc_report_model_final.R
# evaluate.model() -> model() -> onestep()
# save one or more models, will include most up to date counts if 
#   updateDailyCountyCounts.R has been run
# Should save model as .rds (with date as part of title) for further processing
#   * poor_model_DATE.rds, avg_model_DATE.rds, exc_model_DATE.rds

# analyze models
# run the following functions using the saved .rds files
# each produces an output that should be fed to the next
# model_hospitalizations()
# summarise_model_hospitalizations()
# hospital_capacity()

# this corresponds to the most recent model run date (see Models folder)
latest_date <- "2020-04-29"

### Poor Social Distancing Model -----------------------------------------------
# Natural epidemic
poor_mod <- read_rds(paste0("Models/poor_model_", latest_date, ".rds"))
poor_hosp_mod <- model_hospitalizations(poor_mod)
poor_hosp_mod <- summarise_model_hospitalizations(poor_hosp_mod)
poor_hosp_mod <- hospital_capacity(poor_hosp_mod)
write_rds(poor_hosp_mod, paste0("Models/poor_hosp_model_", latest_date, ".rds"))

# plot.max.y <- return_maxvals(poor_mod)
# plot.max.y <- plot.max.y[["max.y"]]
# plot.max.y <- 1e5

acc_natural <- plot.model.acc(poor_mod, cumCases$date, cumCases$secondary,
                              log='y', title='Natural Epidemic Model')
# ggsave(paste0("Plots/Athens_natural_epidemic_", Sys.Date(), ".png"), acc_natural)

plot_hospitalizations(poor_hosp_mod, type = "cum", 
                      title = "Model with Poor Social Distancing (Total Hospitalization Count)")
ggsave(paste0("Plots/poor_socdist_hosp_total_", Sys.Date(), ".png"))
plot_hospitalizations(poor_hosp_mod, type = "capacity",
                      title = "Model with Poor Social Distancing (Current Hospitalization Count)")
ggsave(paste0("Plots/poor_socdist_hosp_current_", Sys.Date(), ".png"))

### Excellent Social Distancing Model ------------------------------------------
# Social distancing works well
# 15 simulations of the base social distancing model
exc_mod <- read_rds(paste0("Models/exc_model_", latest_date, ".rds"))
exc_hosp_mod <- model_hospitalizations(exc_mod)
exc_hosp_mod <- summarise_model_hospitalizations(exc_hosp_mod)
exc_hosp_mod <- hospital_capacity(exc_hosp_mod)
write_rds(exc_hosp_mod, paste0("Models/exc_hosp_model_", 
                                latest_date, ".rds"))

# Plot results
plot.model.acc(exc_mod,  cumCases$date, cumCases$secondary,
               log='y', title='Model With Excellent Social Distancing')
# plot.model.acc(exc_mod,  cumCases$date, cumCases$secondary,
#                log='y', title='Model With Excellent Social Distancing',
#                meanonly = TRUE, trim.days = 5, include.lines = c("C", "Inf"))
plot_hospitalizations(exc_hosp_mod, type = "cum", 
                      title = "Model with Excellent Social Distancing (Total Hospitalization Count)")
ggsave(paste0("Plots/exc_socdist_hosp_total_", Sys.Date(), ".png"))
plot_hospitalizations(exc_hosp_mod, type = "capacity",
                      title = "Model with Excellent Social Distancing (Current Hospitalization Count)")
ggsave(paste0("Plots/exc_socdist_hosp_current_", Sys.Date(), ".png"))
plot_hospitalizations(exc_hosp_mod, type = "pct")

### Average Social Distancing Model --------------------------------------------
# Social distancing works not as well
# 15 simulations of the base social distancing model
avg_mod <- read_rds(paste0("Models/avg_model_", latest_date, ".rds"))
avg_hosp_mod <- model_hospitalizations(avg_mod)
avg_hosp_mod <- summarise_model_hospitalizations(avg_hosp_mod)
avg_hosp_mod <- hospital_capacity(avg_hosp_mod)
write_rds(avg_hosp_mod, paste0("Models/avg_hosp_model_", 
                                latest_date, ".rds"))

# Plot results
plot.model.acc(avg_mod,  cumCases$date, cumCases$secondary,
               log='y', title='Model With Average Social Distancing')
plot_hospitalizations(avg_hosp_mod, type = "cum", 
                      title = "Model with Average Social Distancing (Total Hospitalization Count)")
ggsave(paste0("Plots/avg_socdist_hosp_total_", Sys.Date(), ".png"))
plot_hospitalizations(avg_hosp_mod, type = "capacity",
                      title = "Model with Average Social Distancing (Current Hospitalization Count)")



# Now free to run the summary in athens_secondary_summary.Rmd with updated
# models and case counts
setwd("..")
