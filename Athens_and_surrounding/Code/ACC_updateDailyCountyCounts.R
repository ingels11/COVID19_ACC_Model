## This code is run daily to ensure that number of cases in our working dataset is properly pulled from the 
## Georgia DPH website with number of confirmed cases in each county
library(rvest)
library(lubridate)
library(dplyr)
# Tell which website to look in / scrape HTML cod 
dailyCountWebsite <- "https://d20s4vd27d0hk0.cloudfront.net/?initialWidth=663&childId=covid19dashdph&parentTitle=COVID-19%20Daily%20Status%20Report%20%7C%20Georgia%20Department%20of%20Public%20Health&parentUrl=https%3A%2F%2Fdph.georgia.gov%2Fcovid-19-daily-status-report"
tables = read_html(dailyCountWebsite)

# Telling R to grab only the TABLES from the website
tbls <- html_nodes(tables, "table")


# head(tbls)

# From looking at the website, the table we want is the 3rd table on the site
tbls_ls <- tbls %>%
  html_nodes("table") %>%
  .[9] %>%                    ## Indicating we want the 3rd table
  html_table(fill = TRUE)

# Creating a dataframe from the picked table
countyCasesDaily = as.data.frame(tbls_ls)
names(countyCasesDaily) = c("County", "Cases", "Deaths")
countyCasesDaily = countyCasesDaily[c(-1, -nrow(countyCasesDaily)), -3]


# countyCasesDaily$date = ymd(Sys.Date())


# Reading in our working (full) dataset of all counties that we want to populate 
countsToBeUpdated = read.csv("Colquitt_and_surrounding/Data/ACC Healthcare Region Simulation  - Case Counts by County GA.csv")
dat2 <- data.frame(matrix(nrow = 25, ncol = ncol(countsToBeUpdated)))
names(dat2) = names(countsToBeUpdated)
countsToBeUpdated = bind_rows(countsToBeUpdated, dat2)
countsToBeUpdated$date = seq(as.Date("2020-03-14"), as.Date("2020-03-14")+nrow(countsToBeUpdated)-1, by = "1 day")
countsToBeUpdated[is.na(countsToBeUpdated)] <- 0
names(countsToBeUpdated)[1] = "date"
countsToBeUpdated = countsToBeUpdated[1:which(countsToBeUpdated$date == as.character(Sys.Date())), ]






library(tidyr)
library(plyr)


# going through DPH website and populating the number of cases for each county in our working dataset
for (i in 1:nrow(countyCasesDaily)){
  
  if (countyCasesDaily$County[i] %in%(names(countsToBeUpdated))){
    
    countsToBeUpdated[countsToBeUpdated$date == Sys.Date(), as.character(countyCasesDaily$County)[i]] = 
      countyCasesDaily[countyCasesDaily$County == as.character(countyCasesDaily$County)[i], 2]
  }
}

# Overwriting our working (full) dataset with up to date case numbers for each county in GA
write.csv(countsToBeUpdated, "Athens_and_surrounding/Data/ACC Healthcare Region Simulation  - Case Counts by County GA.csv", row.names = F)


detach("package:plyr", unload=TRUE)





## This bit of code just goes through and populates counties in the Athens area -- primary and secondary
## I guess we could just subset the full working dataset above and indicate which we want, but 
## to minimize that work, I just made a CSV file with only the primary / secondary counties and put an 
## indicator variable to tell which kind (include in both primary AND sec or just secondary)


primSecCounties = read.csv("Athens_and_surrounding/Data/primary and secondary counties cases.csv")
dat2 <- data.frame(matrix(nrow = 25, ncol = ncol(primSecCounties)))
names(dat2) = names(primSecCounties)
primSecCounties = bind_rows(primSecCounties, dat2)
primSecCounties$date = seq(as.Date("2020-03-14"), as.Date("2020-03-14")+nrow(primSecCounties)-1, by = "1 day")
primSecCounties = primSecCounties[1:which(primSecCounties$date == as.character(Sys.Date())), ]
# primSecCounties$date = as.Date(countsToBeUpdated$date)
primSecCounties[is.na(primSecCounties)] <- 0
names(primSecCounties)[1] = "date"





library(tidyr)
library(plyr)

for (i in 1:nrow(countyCasesDaily)){
  
  if (countyCasesDaily$County[i] %in%(names(primSecCounties))){
    
    primSecCounties[primSecCounties$date == Sys.Date(), as.character(countyCasesDaily$County)[i]] = 
      countyCasesDaily[countyCasesDaily$County == as.character(countyCasesDaily$County)[i], 2]
  }
}


write.csv(primSecCounties, "Athens_and_surrounding/Data/primary and secondary counties cases.csv", row.names = F)



newCasesDaily = data.frame(matrix(0L, nrow = nrow(primSecCounties), ncol = ncol(primSecCounties)))

for(i in 2:nrow(primSecCounties)) {
  
  newCasesDaily[,1] = primSecCounties[,1]
  
  newCasesDaily[i, 2:18] = as.numeric(primSecCounties[i, 2:18]) - as.numeric(primSecCounties[i-1, 2:18])
  
  names(newCasesDaily) = names(primSecCounties)
}

names(newCasesDaily) = names(primSecCounties)

write.csv(newCasesDaily[1:which(newCasesDaily$date == as.character(Sys.Date())), ], "Athens_and_surrounding/Data/primSecNewCasesDaily.csv", row.names = F)

detach("package:plyr", unload=TRUE)





# This process will be defined in a function ----

## FOR SOURCE FXN BELOW, GETWD SHOULD BE THE "CODE" FOLDER
source("Athens_and_surrounding/Code/UpdateGitHub.R")

# TO UPDATE GITHUB FUNCTION, CHANGE REPO PATH TO GITHUB PATH. ONE BEFORE COLQUITT_AND_SURROUNDING
UpdateGitHub(repo = "/Users/ishaandave/Box/Random Help Items/COVID19 Git/COVID19_Athens_Model")

