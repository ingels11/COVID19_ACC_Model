cumrm(list = ls())
source("Colquitt_and_surrounding/Code/model_fncs.R")
library(ggplot2)
library(lubridate)
library(scales)
library(readr)
library(tidyverse)

## Data to be read in that will be used to create raw cumulative counts

data <- read.csv("Colquitt_and_surrounding/Data/SW_primary and secondary counties cases.csv")
data$date = as_date(data$date)
datatwo <- data[1:which(data$date == Sys.Date()), ]

datatwo$total <- rowSums(datatwo[,c(2:15)])


# the important line:
datatwo$newdate <- as.Date(parse_date_time(datatwo$date, "%y/%m/%d"))

# plot(datatwo$newdate, datatwo$total, type='h', lwd=10, col='rosybrown',
#      lend='butt', xlab='Date', ylab='Case Notifications', main='Case Notifications for Clarke and Surrounding Counties')

ggplot(data = datatwo, aes(x = newdate, y = total)) +
  geom_bar(stat = "identity", fill = "black", width=.3) + 
  ylim(0, max(datatwo$total)+1) +
  theme_classic()


ggplot(data = datatwo, aes(x = newdate, y = total, label = total)) +
  geom_bar(stat = "identity", fill = "black", width=.3) +
  geom_text(data = datatwo, aes(x = newdate, y = total, label = total),
            position=position_dodge(width=1), vjust=-1) +
  scale_y_continuous(breaks = seq(0, max(datatwo$total), by = 20))+
  scale_x_date(breaks = function(x) seq.Date(from = min(x, na.rm = TRUE),
                                             to = max(x, na.rm = TRUE)+3,
                                             by = "3 days"), date_labels = "%b %d")+
  labs(title = "Case Notifications for Clarke and Surrounding Counties",
       x = "Date", y = "Case Notifications") + theme(panel.grid.major = element_blank(),
                                                     plot.title = element_text(size = 22),
                                                     axis.title.x = element_text(size = 14, margin = margin(20, 0,0,0)),
                                                     axis.title.y = element_text(size = 14, margin = margin(0, 20, 0, 0)),
                                                     panel.grid.minor = element_blank(),
                                                     panel.background = element_blank(), axis.line = element_line(colour = "black"))


























### Read and Format Athens Cases Data ------------------------------------------
dailyCases <- read_csv("Colquitt_and_surrounding/Data/SW_primSecNewCasesDaily.csv")

dailyCases$secondary <- rowSums(dailyCases[,2:15])
# NOTE:


dailyCases2 <- dailyCases[1:which(dailyCases$date == as.character(Sys.Date())), ]
dailyCases2$secondary_cum <- cumsum(dailyCases2$secondary)
# NICK TO FIX THIS PLOT !!!!!!!! (Hopefully)

# Plot of daily Athens cases
# ggplot(data = dailyCases2, mapping = aes(x = date, y = secondary)) +
#   geom_bar(stat = "identity") +
#   scale_x_date(breaks = function(x) seq.Date(from = min(x)+2, 
#                                              to = max(x), 
#                                              by = "3 days"), date_labels = "%b %d")+
#   # minor_breaks = function(x) seq.Date(from = min(x), 
#   #                                     to = max(x), 
#   #                                     by = "2 years")) +
#   labs(x = "Day",
#        y = "New Cases (Primary Service Area)") +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# # Plot of cumulative Athens cases
# ggplot(data = dailyCases2, mapping = aes(x = date, y = secondary_cum)) +
#   geom_bar(stat = "identity") +
#   labs(x = "Day",
#        y = "Cumulative Cases (Primary Service Area)") +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Read and Format Athens Scenarios -------------------------------------------
# Most difficult part is setting the initial numbers of E and I in the model
# Based on the Georgia work of Handel and Drake
#  * Intermediate: 136 cases (15 early cases / 0.11); see Georgia model assumptions
#  * Upper Bound: hard, not date of death in ACC area
#     * will use GA scaling, 2*136 = 272
#     * not using GA scaling, step up one day and go with intermediate method
#     *   then 24 early cases / 0.11 = 218
#  * Lower Bound: hard, not really to two-ish week mark since first case
#     * will use GA scaling, 1/8 * 136 = 17
#     * not using GA scaling, step back one day and go with intermediate method
#     *   then 9 early cases / 0.11 = 82

scenarios <- read.csv("Colquitt_and_surrounding/Data/SW scenarios.csv")

scenarios[8, 15:24] = scenarios[7, 15:24] # forgot to update the E's and I's for social distancing 
                                          # This code takes those values from the baseline 
                                          # with no intervention (row 7) since we are supposed 
                                          # to have the same number of starting cases (55)
                                          # and NOT!! 82 like it says in the actual scenario datasheet.


scenarios[9, 15:ncol(scenarios)] = scenarios[5, 15:ncol(scenarios)]
scenarios[9, 1:14] = scenarios[7, 1:14]
# Only the first 8 are currently setup
scenarios <- scenarios[c(1:9), ]
# move columns 11 and 12 to the end
scenarios <- scenarios[, c(1:10, 13:31, 11, 12)]


### Baseline, natural epidemic -- no interventions

scen_row <- 7

# If nationally was 3/12/20 then this is prior to ACC outbreak so z = 0
gamma <- function(z = scenarios[scen_row, "z"], b=scenarios[scen_row, "b"], a0=scenarios[scen_row, "a0"], t){
  # piecewise function
  # default parameters z = 12, b=1/7, a0=1/1.5
  #    z: time at start of intervention (notionally March 12)
  #    b: intercept (positive)
  #    a0: post intervention isolation ratae
  #    t: time in the model
  
  gamma <- ifelse(t<=z, gamma <- b, gamma <- a0)
  return(gamma)
}
eta <- function(t, w = scenarios[scen_row, "w"]) ifelse(t<=w, 1/3, 1/3)
q <- function(t, w = scenarios[scen_row, "w"], q0=scenarios[scen_row, "q0"], q1=scenarios[scen_row, "q1"]) ifelse(t<=w, q0, q1)
beta <- function(t, w = scenarios[scen_row, "w"], beta0=scenarios[scen_row, "beta0"], beta.factor=2) {
  ifelse(t<=w, beta0, beta0 / beta.factor)
} 

start = as.Date("2020-03-14")
s <- scenarios[,3:31]
i <- scen_row
outBaselineInt <- evaluate.model(params=list(beta0=s[i,1], sigma=s[i,2], z=s[i,3], b=s[i,4], a0=s[i,5], w=s[i,6], presymptomatic=s[i,8], c=s[i,7], dt=0.05),
                       init = list(S=s[i,10], E1=s[i,11], E2=s[i,12], E3=s[i,13], E4=s[i,14], E5=s[i,15], E6=s[i,16],
                                   I1 = s[i,17], I2 = s[i,18], I3 = s[i,19], I4 = s[i,20], Iu1=s[i,21], Iu2=s[i,22], Iu3=s[i,23], Iu4=s[i,24],
                                   H=s[i,25], Ru=s[i,26], C=s[i,27]),
                       nsims=15, nstep=NULL, start=start)



plot.model.acc(outBaselineInt, dailyCases2$date[1:which(dailyCases$date == Sys.Date())], 
               dailyCases2$secondary_cum[1:which(dailyCases$date == Sys.Date())],
               log='y', title='Natural Epidemic (No Social Distancing)')

write_rds(outBaselineInt, 
          paste0("Colquitt_and_surrounding/Models/", "epidemic_base_", Sys.Date()))





### Upper bound of natural epidemic
scen_row <- 9

# If nationally was 3/12/20 then this is prior to ACC outbreak so z = 0
gamma <- function(z = scenarios[scen_row, "z"], b=scenarios[scen_row, "b"], a0=scenarios[scen_row, "a0"], t){
  # piecewise function
  # default parameters z = 12, b=1/7, a0=1/1.5
  #    z: time at start of intervention (notionally March 12)
  #    b: intercept (positive)
  #    a0: post intervention isolation ratae
  #    t: time in the model
  
  gamma <- ifelse(t<=z, gamma <- b, gamma <- a0)
  return(gamma)
}
eta <- function(t, w = scenarios[scen_row, "w"]) ifelse(t<=w, 1/3, 1/3)
q <- function(t, w = scenarios[scen_row, "w"], q0=scenarios[scen_row, "q0"], q1=scenarios[scen_row, "q1"]) ifelse(t<=w, q0, q1)
beta <- function(t, w = scenarios[scen_row, "w"], beta0=scenarios[scen_row, "beta0"], beta.factor=2) {
  ifelse(t<=w, beta0, beta0 / beta.factor)
} 

start = as.Date("2020-03-14")
s <- scenarios[,3:31]
i <- scen_row
outBaselineUpper <- evaluate.model(params=list(beta0=s[i,1], sigma=s[i,2], z=s[i,3], b=s[i,4], a0=s[i,5], w=s[i,6], presymptomatic=s[i,8], c=s[i,7], dt=0.05),
                                 init = list(S=s[i,10], E1=s[i,11], E2=s[i,12], E3=s[i,13], E4=s[i,14], E5=s[i,15], E6=s[i,16],
                                             I1 = s[i,17], I2 = s[i,18], I3 = s[i,19], I4 = s[i,20], Iu1=s[i,21], Iu2=s[i,22], Iu3=s[i,23], Iu4=s[i,24],
                                             H=s[i,25], Ru=s[i,26], C=s[i,27]),
                                 nsims=15, nstep=NULL, start=start)



plot.model.acc(outBaselineUpper, dailyCases$date[1:which(dailyCases$date == Sys.Date())], 
               dailyCases2$secondary_cum[1:which(dailyCases$date == Sys.Date())],
               log='y', title='Natural Epidemic (No Social Distancing) Upper Bound')





### Social Distancing implemented -- early intervention  
## This model ssuming whole area put shelter-in-place order on March 21 (when Dougherty county did) 

scen_row <- 8

# If nationally was 3/12/20 then this is prior to ACC outbreak so z = 0
gamma <- function(z = 7, b=scenarios[scen_row, "b"], a0=scenarios[scen_row, "a0"], t){
  # piecewise function
  # default parameters z = 12, b=1/7, a0=1/1.5
  #    z: time at start of intervention (notionally March 12)
  #    b: intercept (positive)
  #    a0: post intervention isolation ratae
  #    t: time in the model
  
  gamma <- ifelse(t<=z, gamma <- b, gamma <- a0)
  return(gamma)
}
eta <- function(t, w = scenarios[scen_row, "w"]) ifelse(t<=w, 1/3, 1/3)
q <- function(t, w = scenarios[scen_row, "w"], q0=scenarios[scen_row, "q0"], q1=scenarios[scen_row, "q1"]) ifelse(t<=w, q0, q1)
beta <- function(t, w = scenarios[scen_row, "w"], beta0=scenarios[scen_row, "beta0"], beta.factor=2) {
  ifelse(t<=w, beta0, beta0 / beta.factor)
} 

s <- scenarios[,3:31]
i <- scen_row
outSDearly<- evaluate.model(params=list(beta0=s[i,1], sigma=s[i,2], z=7, b=s[i,4], a0=s[i,5], w=s[i,6], presymptomatic=s[i,8], c=s[i,7], dt=s[i,9]),
                       init = list(S=s[i,10], E1=s[i,11], E2=s[i,12], E3=s[i,13], E4=s[i,14], E5=s[i,15], E6=s[i,16],
                                   I1 = s[i,17], I2 = s[i,18], I3 = s[i,19], I4 = s[i,20], Iu1=s[i,21], Iu2=s[i,22], Iu3=s[i,23], Iu4=s[i,24],
                                   H=s[i,25], Ru=s[i,26], C=s[i,27]),
                       nsims=15, nstep=NULL, start=start)

plot.model.acc(outSDearly,  dailyCases$date[1:which(dailyCases$date == Sys.Date())], 
               dailyCases2$secondary_cum[1:which(dailyCases$date == Sys.Date())],
               log='y', title='Social Distancing 3/21 (Baseline)')

write_rds(outSDearly, paste0("Colquitt_and_surrounding/Models/", "social_distance_base_", Sys.Date()))






## Social Distancing Upper Bound (poor social distancing)
## Shelter in place order on March 21

scen_row <- 8

# Too stringent, earlier assumptions likely make more sense
# If nationally was 3/12/20 then this is prior to ACC outbreak so z = 0
gamma <- function(z = 7, b=scenarios[scen_row, "b"], a0=scenarios[scen_row, "a0"], t){
  # piecewise function
  # default parameters z = 12, b=1/7, a0=1/1.5
  #    z: time at start of intervention (notionally March 12)
  #    b: intercept (positive)
  #    a0: post intervention isolation ratae
  #    t: time in the model
  
  gamma <- ifelse(t<=z, gamma <- b, gamma <- a0)
  return(gamma)
}
eta <- function(t, w = scenarios[scen_row, "w"]) ifelse(t<=w, 1/3, 1/3)
q <- function(t, w = scenarios[scen_row, "w"], q0=scenarios[scen_row, "q0"], q1=scenarios[scen_row, "q1"]) ifelse(t<=w, q0, q1)
beta <- function(t, w = scenarios[scen_row, "w"], beta0=scenarios[scen_row, "beta0"], beta.factor=1.5) {
  ifelse(t<=w, beta0, beta0 / beta.factor)
} 

s <- scenarios[,3:31]
i <- scen_row
outSDearlyUpper <- evaluate.model(params=list(beta0=s[i,1], sigma=s[i,2], z=7, b=s[i,4], a0=s[i,5], w=s[i,6], presymptomatic=s[i,8], c=s[i,7], dt=s[i,9]),
                       init = list(S=s[i,10], E1=s[i,11], E2=s[i,12], E3=s[i,13], E4=s[i,14], E5=s[i,15], E6=s[i,16],
                                   I1 = s[i,17], I2 = s[i,18], I3 = s[i,19], I4 = s[i,20], Iu1=s[i,21], Iu2=s[i,22], Iu3=s[i,23], Iu4=s[i,24],
                                   H=s[i,25], Ru=s[i,26], C=s[i,27]),
                       nsims=15, nstep=NULL, start=start)

plot.model.acc(outSDearlyUpper, dailyCases$date[1:which(dailyCases$date == Sys.Date())], 
               dailyCases2$secondary_cum[1:which(dailyCases$date == Sys.Date())], 
               log='y', title='Social Distancing 3/21 (Upper Bound)')


write_rds(outSDearlyUpper, paste0("Colquitt_and_surrounding/Models/", "social_distance_upper_", Sys.Date()))



















## Social Distancing implemented late
## Assumes all counties followed Governor Kemp's shelter in place order on 4/2

scen_row <- 8

# If nationally was 3/12/20 then this is prior to ACC outbreak so z = 0
gamma <- function(z = 19, b=scenarios[scen_row, "b"], a0=scenarios[scen_row, "a0"], t){
  # piecewise function
  # default parameters z = 12, b=1/7, a0=1/1.5
  #    z: time at start of intervention (notionally March 12)
  #    b: intercept (positive)
  #    a0: post intervention isolation ratae
  #    t: time in the model
  
  gamma <- ifelse(t<=z, gamma <- b, gamma <- a0)
  return(gamma)
}
eta <- function(t, w = scenarios[scen_row, "w"]) ifelse(t<=w, 1/3, 1/3)
q <- function(t, w = scenarios[scen_row, "w"], q0=scenarios[scen_row, "q0"], q1=scenarios[scen_row, "q1"]) ifelse(t<=w, q0, q1)
beta <- function(t, w = scenarios[scen_row, "w"], beta0=scenarios[scen_row, "beta0"], beta.factor=2) {
  ifelse(t<=w, beta0, beta0 / beta.factor)
} 

s <- scenarios[,3:31]
i <- scen_row
outSDlate<- evaluate.model(params=list(beta0=s[i,1], sigma=s[i,2], z=19, b=s[i,4], a0=s[i,5], w=s[i,6], presymptomatic=s[i,8], c=s[i,7], dt=s[i,9]),
                       init = list(S=s[i,10], E1=s[i,11], E2=s[i,12], E3=s[i,13], E4=s[i,14], E5=s[i,15], E6=s[i,16],
                                   I1 = s[i,17], I2 = s[i,18], I3 = s[i,19], I4 = s[i,20], Iu1=s[i,21], Iu2=s[i,22], Iu3=s[i,23], Iu4=s[i,24],
                                   H=s[i,25], Ru=s[i,26], C=s[i,27]),
                       nsims=15, nstep=NULL, start=start)

plot.model.acc(outSDlate,  dailyCases$date[1:which(dailyCases$date == Sys.Date())], 
               dailyCases2$secondary_cum[1:which(dailyCases$date == Sys.Date())],
               log='y', title='Social Distancing 4/2 (Baseline)')

write_rds(outSDlate, paste0("Colquitt_and_surrounding/Models/", "social_distance_base_", Sys.Date()))






## Social Distancing Upper Bound (social distancing done poorly)
## Shelter in place on 4/2 (Kemp's date)
scen_row <- 8

# Too stringent, earlier assumptions likely make more sense
# If nationally was 3/12/20 then this is prior to ACC outbreak so z = 0
gamma <- function(z = 19, b=scenarios[scen_row, "b"], a0=scenarios[scen_row, "a0"], t){
  # piecewise function
  # default parameters z = 12, b=1/7, a0=1/1.5
  #    z: time at start of intervention (notionally March 12)
  #    b: intercept (positive)
  #    a0: post intervention isolation ratae
  #    t: time in the model
  
  gamma <- ifelse(t<=z, gamma <- b, gamma <- a0)
  return(gamma)
}
eta <- function(t, w = scenarios[scen_row, "w"]) ifelse(t<=w, 1/3, 1/3)
q <- function(t, w = scenarios[scen_row, "w"], q0=scenarios[scen_row, "q0"], q1=scenarios[scen_row, "q1"]) ifelse(t<=w, q0, q1)
beta <- function(t, w = scenarios[scen_row, "w"], beta0=scenarios[scen_row, "beta0"], beta.factor=1.5) {
  ifelse(t<=w, beta0, beta0 / beta.factor)
} 

s <- scenarios[,3:31]
i <- scen_row
outSDlateUpper <- evaluate.model(params=list(beta0=s[i,1], sigma=s[i,2], z=19, b=s[i,4], a0=s[i,5], w=s[i,6], presymptomatic=s[i,8], c=s[i,7], dt=s[i,9]),
                             init = list(S=s[i,10], E1=s[i,11], E2=s[i,12], E3=s[i,13], E4=s[i,14], E5=s[i,15], E6=s[i,16],
                                         I1 = s[i,17], I2 = s[i,18], I3 = s[i,19], I4 = s[i,20], Iu1=s[i,21], Iu2=s[i,22], Iu3=s[i,23], Iu4=s[i,24],
                                         H=s[i,25], Ru=s[i,26], C=s[i,27]),
                             nsims=15, nstep=NULL, start=start)

plot.model.acc(outSDlateUpper, dailyCases$date[1:which(dailyCases$date == Sys.Date())], 
               dailyCases2$secondary_cum[1:which(dailyCases$date == Sys.Date())], 
               log='y', title='Social Distancing 4/2 (Upper Bound)')


write_rds(outSDlateUpper, paste0("Colquitt_and_surrounding/Models/", "social_distance_upper_", Sys.Date()))























## Getting final estimated case count for each scenario


estCountRaw = NULL
finalEstCountSDearly = NULL
finalEstCountSDearlyUpp = NULL
finalEstCountSDlate= NULL
finalEstCountSDlateUpp = NULL

for (i in 1:15){
  estCountRaw[i]              = c((outBaselineInt[[i]]$C[nrow(outBaselineInt[[i]])]))
  finalEstCountSDearly[i]     = c((outSDearly[[i]]$C[nrow(outSDearly[[i]])]))
  finalEstCountSDearlyUpp[i]  = c((outSDearlyUpper[[i]]$C[nrow(outSDearlyUpper[[i]])]))
  finalEstCountSDlate[i]      = c((outSDlate[[i]]$C[nrow(outSDlate[[i]])]))
  finalEstCountSDlateUpp[i]   = c((outSDlateUpper[[i]]$C[nrow(outSDlateUpper[[i]])]))
}


nums = ceiling(c(mean(estCountRaw), mean(finalEstCountSDearly), mean(finalEstCountSDearlyUpp),
          mean(finalEstCountSDlate), mean(finalEstCountSDlateUpp)))

scenLabels = c("Baseline No Intervention", "Early SD Intervention 3/21 Intermediate",
               "Early SD Intervention 3/21 Upper", "Late SD Intervention 4/2 Intermediate",
               "Late SD Intervention 4/2 Upper")

estCounts = tibble(scenLabels, nums)
print(kable(estCounts))



empty = data.frame(matrix(0L, nrow = nrow(outSD[[15]]), ncol = 16))

for (i in 1:15){
  empty[, 1] = as.data.frame(outBaselineInt[[1]]$cum.time)
  empty[, i+1] = as.data.frame(outBaselineInt[[i]]$C)
}

names(empty)[1] = c("time")

empty2 = empty[which(empty$time %% 1 == 0),]
empty2$date = seq(as.Date("2020-03-14"), as.Date("2020-03-14")+nrow(empty2)-1, by = "day")



for(row in 1:nrow(empty2)){
  empty2$estCumCases[row] = ceiling(mean(as.numeric(empty2[row, 2:16])))
}



estCasesByDay = empty2[, 17:18]
estCasesByDay$date = format(estCasesByDay$date,  "%B %d")
write.csv(estCasesByDay, paste0("/Users/ishaandave/Desktop/COVID Scratch Work/Cases By Day", Sys.Date(), ".csv"), row.names = F)

## Lower Bound Social Distancing 

# scen_row <- 3
# 
# 
# # If nationally was 3/12/20 then this is prior to ACC outbreak so z = 0
# gamma <- function(z = scenarios[scen_row, "z"], b=scenarios[scen_row, "b"], a0=scenarios[scen_row, "a0"], t){
#   # piecewise function
#   # default parameters z = 12, b=1/7, a0=1/1.5
#   #    z: time at start of intervention (notionally March 12)
#   #    b: intercept (positive)
#   #    a0: post intervention isolation ratae
#   #    t: time in the model
#   
#   gamma <- ifelse(t<=z, gamma <- b, gamma <- a0)
#   return(gamma)
# }
# eta <- function(t, w = scenarios[scen_row, "w"]) ifelse(t<=w, 1/3, 1/3)
# q <- function(t, w = scenarios[scen_row, "w"], q0=scenarios[scen_row, "q0"], q1=scenarios[scen_row, "q1"]) ifelse(t<=w, q0, q1)
# beta <- function(t, w = scenarios[scen_row, "w"], beta0=scenarios[scen_row, "beta0"], beta.factor=2) {
#   ifelse(t<=w, beta0, beta0 / beta.factor)
# } 
# 
# s <- scenarios[,3:31]
# i <- scen_row
# outSDLower <- evaluate.model(params=list(beta0=s[i,1], sigma=s[i,2], z=s[i,3], b=s[i,4], a0=s[i,5], w=s[i,6], presymptomatic=s[i,8], c=s[i,7], dt=s[i,9]),
#                        init = list(S=s[i,10], E1=s[i,11], E2=s[i,12], E3=s[i,13], E4=s[i,14], E5=s[i,15], E6=s[i,16],
#                                    I1 = s[i,17], I2 = s[i,18], I3 = s[i,19], I4 = s[i,20], Iu1=s[i,21], Iu2=s[i,22], Iu3=s[i,23], Iu4=s[i,24],
#                                    H=s[i,25], Ru=s[i,26], C=s[i,27]),
#                        nsims=15, nstep=NULL, start=start)
# 
# plot.model.acc(outSDLower, dailyCases$date[1:which(dailyCases$date == Sys.Date())], 
#                dailyCases$secondary[1:which(dailyCases$date == Sys.Date())],
#                log='y', title='With Social Distancing (Lower Bound)')
# 
# 
# 



# Doesn't work at all, too small of a starting size

# Bigger next


# ### Social Distancing Intervention (Scenario 8) --------------------------------
# scen_row <- 8
# 
# # If nationally was 3/12/20 then this is prior to ACC outbreak so z = 0
# gamma <- function(z = scenarios[scen_row, "z"], b=scenarios[scen_row, "b"], a0=scenarios[scen_row, "a0"], t){
#   # piecewise function
#   # default parameters z = 12, b=1/7, a0=1/1.5
#   #    z: time at start of intervention (notionally March 12)
#   #    b: intercept (positive)
#   #    a0: post intervention isolation ratae
#   #    t: time in the model
#   
#   gamma <- ifelse(t<=z, gamma <- b, gamma <- a0)
#   return(gamma)
# }
# eta <- function(t, w = scenarios[scen_row, "w"]) ifelse(t<=w, 1/3, 1/3)
# q <- function(t, w = scenarios[scen_row, "w"], q0=scenarios[scen_row, "q0"], q1=scenarios[scen_row, "q1"]) ifelse(t<=w, q0, q1)
# beta <- function(t, w = scenarios[scen_row, "w"], beta0=scenarios[scen_row, "beta0"], beta.factor=2) {
#   ifelse(t<=w, beta0, beta0 / beta.factor)
# } 
# 
# s <- scenarios[,3:31]
# i <- scen_row
# out8 <- evaluate.model(params=list(beta0=s[i,1], sigma=s[i,2], z=s[i,3], b=s[i,4], a0=s[i,5], w=s[i,6], presymptomatic=s[i,8], c=s[i,7], dt=s[i,9]),
#                        init = list(S=s[i,10], E1=s[i,11], E2=s[i,12], E3=s[i,13], E4=s[i,14], E5=s[i,15], E6=s[i,16],
#                                    I1 = s[i,17], I2 = s[i,18], I3 = s[i,19], I4 = s[i,20], Iu1=s[i,21], Iu2=s[i,22], Iu3=s[i,23], Iu4=s[i,24],
#                                    H=s[i,25], Ru=s[i,26], C=s[i,27]),
#                        nsims=15, nstep=NULL, start=start)
# 
# plot.model.acc(out8,  dailyCases$date[1:which(dailyCases$date == Sys.Date())], 
#                dailyCases$secondary[1:which(dailyCases$date == Sys.Date())],
#                log='y', title='With Social Distancing')
# 
# 
# ### Smaller and Larger Starting Sizes (Scenarios 3 and 5) ----------------------
# # Smaller first
# scen_row <- 3
# 
# 
# # If nationally was 3/12/20 then this is prior to ACC outbreak so z = 0
# gamma <- function(z = scenarios[scen_row, "z"], b=scenarios[scen_row, "b"], a0=scenarios[scen_row, "a0"], t){
#   # piecewise function
#   # default parameters z = 12, b=1/7, a0=1/1.5
#   #    z: time at start of intervention (notionally March 12)
#   #    b: intercept (positive)
#   #    a0: post intervention isolation ratae
#   #    t: time in the model
#   
#   gamma <- ifelse(t<=z, gamma <- b, gamma <- a0)
#   return(gamma)
# }
# eta <- function(t, w = scenarios[scen_row, "w"]) ifelse(t<=w, 1/3, 1/3)
# q <- function(t, w = scenarios[scen_row, "w"], q0=scenarios[scen_row, "q0"], q1=scenarios[scen_row, "q1"]) ifelse(t<=w, q0, q1)
# beta <- function(t, w = scenarios[scen_row, "w"], beta0=scenarios[scen_row, "beta0"], beta.factor=2) {
#   ifelse(t<=w, beta0, beta0 / beta.factor)
# } 
# 
# s <- scenarios[,3:31]
# i <- scen_row
# out3 <- evaluate.model(params=list(beta0=s[i,1], sigma=s[i,2], z=s[i,3], b=s[i,4], a0=s[i,5], w=s[i,6], presymptomatic=s[i,8], c=s[i,7], dt=s[i,9]),
#                        init = list(S=s[i,10], E1=s[i,11], E2=s[i,12], E3=s[i,13], E4=s[i,14], E5=s[i,15], E6=s[i,16],
#                                    I1 = s[i,17], I2 = s[i,18], I3 = s[i,19], I4 = s[i,20], Iu1=s[i,21], Iu2=s[i,22], Iu3=s[i,23], Iu4=s[i,24],
#                                    H=s[i,25], Ru=s[i,26], C=s[i,27]),
#                        nsims=15, nstep=NULL, start=start)
# 
# plot.model.acc(out3, primSecCounties$date[1:which(primSecCounties$date == Sys.Date())], 
#                primSecCounties$secondary[1:which(primSecCounties$date == Sys.Date())],
#                log='y', title='With Social Distancing (Lower Bound)')
# # Doesn't work at all, too small of a starting size
# 
# # Bigger next
# scen_row <- 5
# 
# # Too stringent, earlier assumptions likely make more sense
# # If nationally was 3/12/20 then this is prior to ACC outbreak so z = 0
# gamma <- function(z = scenarios[scen_row, "z"], b=scenarios[scen_row, "b"], a0=scenarios[scen_row, "a0"], t){
#   # piecewise function
#   # default parameters z = 12, b=1/7, a0=1/1.5
#   #    z: time at start of intervention (notionally March 12)
#   #    b: intercept (positive)
#   #    a0: post intervention isolation ratae
#   #    t: time in the model
#   
#   gamma <- ifelse(t<=z, gamma <- b, gamma <- a0)
#   return(gamma)
# }
# eta <- function(t, w = scenarios[scen_row, "w"]) ifelse(t<=w, 1/3, 1/3)
# q <- function(t, w = scenarios[scen_row, "w"], q0=scenarios[scen_row, "q0"], q1=scenarios[scen_row, "q1"]) ifelse(t<=w, q0, q1)
# beta <- function(t, w = scenarios[scen_row, "w"], beta0=scenarios[scen_row, "beta0"], beta.factor=2) {
#   ifelse(t<=w, beta0, beta0 / beta.factor)
# } 
# 
# s <- scenarios[,3:31]
# i <- scen_row
# out5 <- evaluate.model(params=list(beta0=s[i,1], sigma=s[i,2], z=s[i,3], b=s[i,4], a0=s[i,5], w=s[i,6], presymptomatic=s[i,8], c=s[i,7], dt=s[i,9]),
#                        init = list(S=s[i,10], E1=s[i,11], E2=s[i,12], E3=s[i,13], E4=s[i,14], E5=s[i,15], E6=s[i,16],
#                                    I1 = s[i,17], I2 = s[i,18], I3 = s[i,19], I4 = s[i,20], Iu1=s[i,21], Iu2=s[i,22], Iu3=s[i,23], Iu4=s[i,24],
#                                    H=s[i,25], Ru=s[i,26], C=s[i,27]),
#                        nsims=15, nstep=NULL, start=start)
# 
# plot.model.acc(out5, primSecCounties$date[1:which(primSecCounties$date == Sys.Date())], 
#            primSecCounties$secondary[1:which(primSecCounties$date == Sys.Date())], 
#            log='y', title='With Social Distancing (Upper Bound)')
# # No presymptomatic here, that seems to make a difference, doesn't behave well
# # Adding presymptomatic == 1, seems to make sense, creates a definite upper bound
# # in cumulative reported cases
# 
# ### Both Immedidate Interventions (Scenario 15) --------------------------------
# scen_row <- 9
# 
# # Too stringent, earlier assumptions likely make more sense
# # If nationally was 3/12/20 then this is prior to ACC outbreak so z = 0
# gamma <- function(z = scenarios[scen_row, "z"], b=scenarios[scen_row, "b"], a0=scenarios[scen_row, "a0"], t){
#   # piecewise function
#   # default parameters z = 12, b=1/7, a0=1/1.5
#   #    z: time at start of intervention (notionally March 12)
#   #    b: intercept (positive)
#   #    a0: post intervention isolation ratae
#   #    t: time in the model
#   
#   gamma <- ifelse(t<=z, gamma <- b, gamma <- a0)
#   return(gamma)
# }
# eta <- function(t, w = scenarios[scen_row, "w"]) ifelse(t<=w, 1/3, 1/3)
# q <- function(t, w = scenarios[scen_row, "w"], q0=scenarios[scen_row, "q0"], q1=scenarios[scen_row, "q1"]) ifelse(t<=w, q0, q1)
# beta <- function(t, w = scenarios[scen_row, "w"], beta0=scenarios[scen_row, "beta0"], beta.factor=2) {
#   ifelse(t<=w, beta0, beta0 / beta.factor)
# } 
# 
# s <- scenarios[,3:31]
# i <- scen_row
# out15 <- evaluate.model(params=list(beta0=s[i,1], sigma=s[i,2], z=s[i,3], b=s[i,4], a0=s[i,5], w=s[i,6], presymptomatic=s[i,8], c=s[i,7], dt=s[i,9]),
#                         init = list(S=s[i,10], E1=s[i,11], E2=s[i,12], E3=s[i,13], E4=s[i,14], E5=s[i,15], E6=s[i,16],
#                                     I1 = s[i,17], I2 = s[i,18], I3 = s[i,19], I4 = s[i,20], Iu1=s[i,21], Iu2=s[i,22], Iu3=s[i,23], Iu4=s[i,24],
#                                     H=s[i,25], Ru=s[i,26], C=s[i,27]),
#                         nsims=15, nstep=NULL, start=start)
# 
# plot.model.acc(out15, log='y', title='Both early interventions')
# # Not sure this makes sense.
