# Update the daily county of cases in Colquitt and sorrounding area
# writes to "SW_primary and secondary counties cases.csv" and
#   "primSecNewCasesDaily.csv"
# source("Code/updateDailyCountyCounts.R")
# This is being done every day by Ishaan/Nicholas
### Setup ----------------------------------------------------------------------
rm(list = ls())
source("Code/model_fncs.R")
library(ggplot2)

cumCases <- read_csv("Colquitt_and_surrounding/Data/SW_primary and secondary counties cases.csv")
cumCases$secondary <- rowSums(cumCases[, 2:15])
#cumCases %<>% filter(date <= Sys.Date())
# This corresponds to the most recent reported case information
# cumCases %<>% filter(date <= as.Date("2020-04-13"))


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
hc_df <- read_csv("Colquitt_and_surrounding/Data/hc_params.csv")

### Poor Social Distancing Model -----------------------------------------------
# Natural epidemic
poor_mod <- read_rds(paste0("Colquitt_and_surrounding/Models/poor_model_", 
                            latest_date, ".rds"))
poor_hosp_mod <- model_hospitalizations(poor_mod, hc_df)
poor_hosp_mod <- summarise_model_hospitalizations(poor_hosp_mod, hc_df)
poor_hosp_mod <- hospital_capacity(poor_hosp_mod, hc_df)
write_rds(poor_hosp_mod, 
          paste0("Colquitt_and_surrounding/Models/poor_hosp_model_", 
                 latest_date, ".rds"))

# plot.max.y <- return_maxvals(poor_mod)
# plot.max.y <- plot.max.y[["max.y"]]
# plot.max.y <- 1e5

swga_natural <- plot.model.acc(poor_mod, cumCases$date, cumCases$secondary,
                              log='y', title='Natural Epidemic Model',
                              include.lines = c("C", "Inf", "L"))
# ggsave(paste0("Plots/Athens_natural_epidemic_", Sys.Date(), ".png"), acc_natural)

plot_hospitalizations(poor_hosp_mod, type = "cum", 
                      title = "Model with Poor Social Distancing (Total Hospitalization Count)",
                      add_hosp = FALSE)
ggsave(paste0("Colquitt_and_surrounding/Plots/poor_socdist_hosp_total_", 
              Sys.Date(), ".png"))
plot_hospitalizations(poor_hosp_mod, type = "capacity",
                      title = "Model with Poor Social Distancing (Current Hospitalization Count)",
                      add_hosp = FALSE)
ggsave(paste0("Colquitt_and_surrounding/Plots/poor_socdist_hosp_current_", 
              Sys.Date(), ".png"))

### Average (Real) Social Distancing Model -------------------------------------
# Social distancing works well
# 15 simulations of the base social distancing model
avg_mod <- read_rds(paste0("Colquitt_and_surrounding/Models/avg_model_", latest_date, ".rds"))
avg_hosp_mod <- model_hospitalizations(avg_mod, hc_df)
avg_hosp_mod <- summarise_model_hospitalizations(avg_hosp_mod, hc_df)
avg_hosp_mod <- hospital_capacity(avg_hosp_mod, hc_df)
write_rds(avg_hosp_mod, paste0("Colquitt_and_surrounding/Models/avg_hosp_model_", 
                                latest_date, ".rds"))

# Plot results
plot.model.acc(avg_mod,  cumCases$date, cumCases$secondary,
               log='y', title='Model With Average Social Distancing',
               include.lines = c("C", "L", "Inf"))
plot_hospitalizations(avg_hosp_mod, type = "cum", 
                      title = "Model with Average Social Distancing (Total Hospitalization Count)",
                      add_hosp = FALSE)
ggsave(paste0("Colquitt_and_surrounding/Plots/avg_socdist_hosp_total_", Sys.Date(), ".png"))
plot_hospitalizations(avg_hosp_mod, type = "capacity",
                      title = "Model with Average Social Distancing (Current Hospitalization Count)",
                      add_hosp = FALSE)
ggsave(paste0("Colquitt_and_surrounding/Plots/avg_socdist_hosp_current_", Sys.Date(), ".png"))

### Average (Change) Social Distancing Model -----------------------------------
# Social distancing works not as well
# 15 simulations of the base social distancing model
avg_mod <- read_rds(paste0("Colquitt_and_surrounding/Models/avg_change_model_", latest_date, ".rds"))
avg_hosp_mod <- model_hospitalizations(avg_mod, hc_df)
avg_hosp_mod <- summarise_model_hospitalizations(avg_hosp_mod, hc_df)
avg_hosp_mod <- hospital_capacity(avg_hosp_mod, hc_df)
write_rds(avg_hosp_mod, paste0("Colquitt_and_surrounding/Models/avg_change_hosp_model_", 
                               latest_date, ".rds"))

# Plot results
plot.model.acc(avg_mod,  cumCases$date, cumCases$secondary,
               log='y', title='Model With Average Social Distancing (Lower Bound Change)',
               include.lines = c("C", "L", "Inf"))
plot_hospitalizations(avg_hosp_mod, type = "cum", 
                      title = "Model with Average Social Distancing (Lower Bound Change; Total Hospitalization Count)",
                      add_hosp = FALSE)
ggsave(paste0("Colquitt_and_surrounding/Plots/avg_change_socdist_hosp_total_", Sys.Date(), ".png"))
plot_hospitalizations(avg_hosp_mod, type = "capacity",
                      title = "Model with Average Social Distancing (Lower Bound Change) Current Hospitalization Count)",
                      add_hosp = FALSE)
ggsave(paste0("Colquitt_and_surrounding/Plots/avg_change_socdist_hosp_current_", Sys.Date(), ".png"))


### Average (Bad Change) Social Distancing Model -------------------------------
# Social distancing works not as well
# 15 simulations of the base social distancing model
avg_mod <- read_rds(paste0("Colquitt_and_surrounding/Models/avg_badchange_model_", latest_date, ".rds"))
avg_hosp_mod <- model_hospitalizations(avg_mod, hc_df)
avg_hosp_mod <- summarise_model_hospitalizations(avg_hosp_mod, hc_df)
avg_hosp_mod <- hospital_capacity(avg_hosp_mod, hc_df)
write_rds(avg_hosp_mod, paste0("Colquitt_and_surrounding/Models/avg_badchange_hosp_model_", 
                               latest_date, ".rds"))

# Plot results
plot.model.acc(avg_mod,  cumCases$date, cumCases$secondary,
               log='y', title='Model With Average Social Distancing (Upper Bound Change)',
               include.lines = c("C", "L", "Inf"))
plot_hospitalizations(avg_hosp_mod, type = "cum", 
                      title = "Model with Average Social Distancing (Upper Bound Change; Total Hospitalization Count)",
                      add_hosp = FALSE)
ggsave(paste0("Colquitt_and_surrounding/Plots/avg_badchange_socdist_hosp_total_", Sys.Date(), ".png"))
plot_hospitalizations(avg_hosp_mod, type = "capacity",
                      title = "Model with Average Social Distancing (Upper Bound Change) Current Hospitalization Count)",
                      add_hosp = FALSE)
ggsave(paste0("Colquitt_and_surrounding/Plots/avg_badchange_socdist_hosp_current_", Sys.Date(), ".png"))

### Now free to run summaries including hospitalizations
