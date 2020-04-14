### ----------------------------------------------------------------------------
# This is the initial Georgia-specific file created to ensure we could
# reproduce what Handel and Drake were reporting for Georgia. Once everything
# seemed good, the functions used for the simulation here have been updated
# for the Athens area. 
### ----------------------------------------------------------------------------
rm(list = ls())
source("Athens_and_surrounding/Code/model_fncs.R")
# Simulations that are included on
# http://2019-coronavirus-tracker.com/stochastic-GA.html

# In the Data folder there is a file called "Georgia scenarios - Sheet1.csv"
# Each of these simulations comes from one of those scenarios, my understanding
# is that what they report to the state includes solutions of each of those models
# In some way they represent different expections about what could/will occur
# in the future.

# I don't have access to the updated cases by day in Georgia file they used
# so the vertical lines represented cumulative cases based on data will
# not be shown in the plots

# https://raw.githubusercontent.com/CEIDatUGA/ncov-wuhan-stochastic-model/master/Georgia%20scenarios%20-%20Sheet1.csv?token=AA6F3ZKMORB6XSHDU4AFT4S6QM7LC

### Natural epidemic -----------------------------------------------------------
# Scenario 6 in "Georgia scenarios - Sheet1.csv"
start=as.Date("2020-03-01")
out64.natural <- evaluate.model(params=list(beta0=0.6584, sigma=1/6.4, z=1200, b=0.143, a0=1/1.5, w=100, presymptomatic=1, c=1, dt=0.05),
                                init = list(S=10600000, E1=6, E2=6, E3=6, E4=6, E5=6, E6=6,
                                            I1 = 7, I2= 7, I3=7, I4=7, Iu1=0, Iu2=0, Iu3=0, Iu4=0,
                                            H=0, Ru=0, C=0),
                                nsims=15, nstep=NULL, start=as.Date("2020-03-01"))

# Matches figure on simulation page
plot.model(out64.natural, log='y', title='Benchmark: Natural epidemic')

### Baseline -------------------------------------------------------------------
# Scenario 7 in "Georgia scenarios - Sheet1.csv"
start=as.Date("2020-03-01")
out64.baseline <- evaluate.model(params=list(beta0=0.6584, sigma=1/6.4, z=12, b=0.143, a0=1/1.5, w=100, c=1, presymptomatic=1, dt=0.05),
                                 init = list(S=10600000, E1=6, E2=6, E3=6, E4=6, E5=6, E6=6,
                                             I1 = 7, I2= 7, I3=7, I4=7, Iu1=0, Iu2=0, Iu3=0, Iu4=0,
                                             H=0, Ru=0, C=0),
                                 nsims=15, nstep=NULL, start=as.Date("2020-03-01"))

# Matches figure on simulation page
plot.model(out64.baseline, log='y', title='Baseline: Rapid case identification but no social distancing')


### Interventions --------------------------------------------------------------
# Scenario 3 in "Georgia scenarios - Sheet1.csv"
start=as.Date("2020-03-01")
set.seed(1292020)                #set seed
out8 <- evaluate.model(params=list(beta0=0.6584, sigma=1/6.4, z=12, b=0.143, a0=1/1.5, w=12, c=1, presymptomatic=1, dt=0.05),
                       init = list(S=10600000, E1=1, E2=1, E3=1, E4=1, E5=1, E6=1,
                                   I1 = 1, I2= 1, I3=0, I4=0, Iu1=0, Iu2=0, Iu3=0, Iu4=0,
                                   H=0, Ru=0, C=0),
                       nsims=15, nstep=NULL, start=as.Date("2020-03-01"))

# Matches figure on simulation page
plot.model(out8, log='y', title='Outbreak started with 8 cases on 1 March and interventions on 12 March')

# Scenario 8 in "Georgia scenarios - Sheet1.csv"
out64 <- evaluate.model(params=list(beta0=0.6584, sigma=1/6.4, z=12, b=0.143, a0=1/1.5, w=12, c=1, presymptomatic=1, dt=0.05),
                        init = list(S=10600000, E1=6, E2=6, E3=6, E4=6, E5=6, E6=6,
                                    I1 = 7, I2= 7, I3=7, I4=7, Iu1=0, Iu2=0, Iu3=0, Iu4=0,
                                    H=0, Ru=0, C=0),
                        nsims=15, nstep=NULL, start=as.Date("2020-03-01"))

# Matches figure on simulation page
plot.model(out64, log='y', title='Outbreak started with 64 cases on 1 March and interventions on 12 March')

# Scenario 5 in "Georgia scenarios - Sheet1.csv" (but presymptomatic == 0 in that sheet)
out128 <- evaluate.model(params=list(beta0=0.6584, sigma=1/6.4, z=12, b=0.143, a0=1/1.5, w=12, c=1, presymptomatic=1, dt=0.05),
                         init = list(S=10600000, E1=13, E2=13, E3=13, E4=13, E5=13, E6=13,
                                     I1 = 13, I2= 13, I3=12, I4=12, Iu1=0, Iu2=0, Iu3=0, Iu4=0,
                                     H=0, Ru=0, C=0),
                         nsims=15, nstep=NULL, start=as.Date("2020-03-01"))

# Matches figure on simulation page
plot.model(out128, log='y', title='Outbreak started with 128 cases on 1 March and interventions on 12 March')


