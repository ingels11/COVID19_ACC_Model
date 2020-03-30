rm(list = ls())
source("Code/model_fncs.R")
library(ggplot2)





## Data to be read in that will be used to create raw cumulative counts

data <- read.csv("Data/primary and secondary counties cases.csv")
data$date = as_date(data$date)
datatwo <- data[1:which(data$date == Sys.Date()), ]

datatwo$total <- rowSums(datatwo[,c(2:17)])


library(lubridate)
library(scales)
library(ggplot2)

# the important line:
datatwo$newdate <- as.Date(parse_date_time(datatwo$date, "%y/%m/%d"))

# plot(datatwo$newdate, datatwo$total, type='h', lwd=10, col='rosybrown',
#      lend='butt', xlab='Date', ylab='Case Notifications', main='Case Notifications for Clarke and Surrounding Counties')

library(ggplot2)
ggplot(data = datatwo, aes(x = newdate, y = total)) +
  geom_bar(stat = "identity", fill = "black", width=.3) + ylim(0, max(datatwo$total)+1)+
  scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                             to = max(x)+3, 
                                             by = "2 days"), date_labels = "%b %d")+
  labs(title = "Case Notifications for Clarke and Surrounding Counties",
       x = "Date", y = "Case Notifications") + theme(panel.grid.major = element_blank(), 
                                                     plot.title = element_text(size = 22),
                                                     axis.title.x = element_text(size = 14, margin = margin(20, 0,0,0)),
                                                     axis.title.y = element_text(size = 14, margin = margin(0, 20, 0, 0)),
                                                     panel.grid.minor = element_blank(),
                                                     panel.background = element_blank(), axis.line = element_line(colour = "black"))

























### Read and Format Athens Cases Data ------------------------------------------
dailyCases <- read_csv("Data/primSecNewCasesDaily.csv")

dailyCases$secondary <- rowSums(dailyCases[,2:18])
# NOTE:
# I only see Morgan county in the secondary service area 
# Going to stop here for the moment and return to this if more data exists on 
# those counties


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

scenarios <- read.csv("Data/athens secondary scenarios ga scaling.csv")

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



### Baseline Model (Scenario 7) ------------------------------------------------
# out.base <- evaluate.model(params=list(beta0=0.6584, sigma=1/6.4, z=0, b=0.143, a0=1/1.5, w=100, c=1, presymptomatic=1, dt=0.05),
#                                  init = list(S=447451, E1=ei, E2=ei, E3=ei, E4=ei, E5=ei, E6=ei,
#                                              I1 = ii, I2= ii, I3=ii, I4=ii, Iu1=0, Iu2=0, Iu3=0, Iu4=0,
#                                              H=0, Ru=0, C=0),
#                                  nsims=15, nstep=NULL, start=as.Date("2020-03-01"))

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



plot.model.acc(outBaselineInt, dailyCases$date[1:which(dailyCases$date == Sys.Date())], 
               dailyCases$secondary[1:which(dailyCases$date == Sys.Date())],
               log='y', title='Natural Epidemic (No Social Distancing)')








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
               dailyCases$secondary[1:which(dailyCases$date == Sys.Date())],
               log='y', title='Natural Epidemic (No Social Distancing) Upper Bound')










scen_row <- 8

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

s <- scenarios[,3:31]
i <- scen_row
outSD<- evaluate.model(params=list(beta0=s[i,1], sigma=s[i,2], z=s[i,3], b=s[i,4], a0=s[i,5], w=s[i,6], presymptomatic=s[i,8], c=s[i,7], dt=s[i,9]),
                       init = list(S=s[i,10], E1=s[i,11], E2=s[i,12], E3=s[i,13], E4=s[i,14], E5=s[i,15], E6=s[i,16],
                                   I1 = s[i,17], I2 = s[i,18], I3 = s[i,19], I4 = s[i,20], Iu1=s[i,21], Iu2=s[i,22], Iu3=s[i,23], Iu4=s[i,24],
                                   H=s[i,25], Ru=s[i,26], C=s[i,27]),
                       nsims=15, nstep=NULL, start=start)

plot.model.acc(outSD,  dailyCases$date[1:which(dailyCases$date == Sys.Date())], 
               dailyCases$secondary[1:which(dailyCases$date == Sys.Date())],
               log='y', title='With Social Distancing')








## Baseline Upper Bound Social Distancing
scen_row <- 5

# Too stringent, earlier assumptions likely make more sense
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

s <- scenarios[,3:31]
i <- scen_row
outSDUpper <- evaluate.model(params=list(beta0=s[i,1], sigma=s[i,2], z=s[i,3], b=s[i,4], a0=s[i,5], w=s[i,6], presymptomatic=s[i,8], c=s[i,7], dt=s[i,9]),
                       init = list(S=s[i,10], E1=s[i,11], E2=s[i,12], E3=s[i,13], E4=s[i,14], E5=s[i,15], E6=s[i,16],
                                   I1 = s[i,17], I2 = s[i,18], I3 = s[i,19], I4 = s[i,20], Iu1=s[i,21], Iu2=s[i,22], Iu3=s[i,23], Iu4=s[i,24],
                                   H=s[i,25], Ru=s[i,26], C=s[i,27]),
                       nsims=15, nstep=NULL, start=start)

plot.model.acc(outSDUpper, dailyCases$date[1:which(dailyCases$date == Sys.Date())], 
               dailyCases$secondary[1:which(dailyCases$date == Sys.Date())], 
               log='y', title='With Social Distancing (Upper Bound)')




## Getting final estimated case count for each scenario


estCountRaw = NULL
for (i in 1:15){
  estCountRaw[i] = c((outBaselineInt[[i]]$C[841]))
}

mean(estCountRaw)


estCountRawUpper = NULL
for (i in 1:15){
  estCountRawUpper[i] = c((outBaselineUpper[[i]]$C[841]))
}


finalEstCountSD = NULL
for (i in 1:15){
  finalEstCountSD[i] = c((outSD[[i]]$C[841]))
}



finalEstCountSDUpp = NULL
for (i in 1:15){
  finalEstCountSDUpp[i] = c((outSDUpper[[i]]$C[841]))
}


ceiling(c(mean(estCountRaw), mean(estCountRawUpper), mean(finalEstCountSD), mean(finalEstCountSDUpp)))


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
# plot.model.acc(outSDLower, dailyCases$date[1:which(dailyCases$date == Sys.Date()-1)], 
#                dailyCases$secondary[1:which(dailyCases$date == Sys.Date()-1)],
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
# plot.model.acc(out8,  dailyCases$date[1:which(dailyCases$date == Sys.Date()-1)], 
#                dailyCases$secondary[1:which(dailyCases$date == Sys.Date()-1)],
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
