
library(rvest)
install.packages("lubridate")
library(lubridate)
dailyCountWebsite <- read_html("https://dph.georgia.gov/covid-19-daily-status-report")

tbls <- html_nodes(dailyCountWebsite, "table")

head(tbls)
tbls_ls <- dailyCountWebsite %>%
  html_nodes("table") %>%
  .[3] %>%
  html_table(fill = TRUE)

countyCasesDaily = as.data.frame(tbls_ls)

countyCasesDaily$date = ymd(Sys.Date())

countsToBeUpdated = read.csv("Data/ACC Healthcare Region Simulation  - Case Counts by County GA.csv")
countsToBeUpdated[is.na(countsToBeUpdated)] <- 0
names(countsToBeUpdated)[1] = "date"
countsToBeUpdated$date = lubridate::ymd(countsToBeUpdated$date)






library(tidyr)
library(plyr)

for (i in 1:nrow(countyCasesDaily)){
  
  if (countyCasesDaily$County[i] %in%(names(countsToBeUpdated))){
    
    countsToBeUpdated[countsToBeUpdated$date == Sys.Date(), as.character(countyCasesDaily$County)[i]] = 
      countyCasesDaily[countyCasesDaily$County == as.character(countyCasesDaily$County)[i], 2]
  }
}




write.csv(countsToBeUpdated, "Data/ACC Healthcare Region Simulation  - Case Counts by County GA.csv", row.names = F)











primSecCounties = read.csv("Data/primary and secondary counties cases.csv")
primSecCounties[is.na(primSecCounties)] <- 0
names(primSecCounties)[1] = "date"
primSecCounties$date = lubridate::ymd(primSecCounties$date)


library(tidyr)
library(plyr)

for (i in 1:nrow(countyCasesDaily)){
  
  if (countyCasesDaily$County[i] %in%(names(primSecCounties))){
    
    primSecCounties[primSecCounties$date == Sys.Date()-1, as.character(countyCasesDaily$County)[i]] = 
      countyCasesDaily[countyCasesDaily$County == as.character(countyCasesDaily$County)[i], 2]
  }
}


write.csv(primSecCounties, "Data/primary and secondary counties cases.csv", row.names = F)





