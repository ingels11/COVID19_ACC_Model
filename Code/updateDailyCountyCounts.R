
library(rvest)


dailyCountWebsite <- read_html("https://dph.georgia.gov/covid-19-daily-status-report")

tbls <- html_nodes(dailyCountWebsite, "table")

head(tbls)
tbls_ls <- dailyCountWebsite %>%
  html_nodes("table") %>%
  .[3] %>%
  html_table(fill = TRUE)

countyCasesDaily = as.data.frame(tbls_ls)

countyCasesDaily$date = Sys.Date()

countsToBeUpdated = read.csv("Data/ACC Healthcare Region Simulation  - Case Counts by County GA.csv")
names(countsToBeUpdated)[1] <- "date"
countsToBeUpdated$date <- as.Date(as.character(countsToBeUpdated$date), format = "%m/%d/%y")




library(tidyr)
library(plyr)

for (i in 1:nrow(countyCasesDaily)){
  
  if (countyCasesDaily$County[i] %in%(names(countsToBeUpdated))){
    
    countsToBeUpdated[countsToBeUpdated$date == Sys.Date(), as.character(countyCasesDaily$County)[i]] = 
      countyCasesDaily[countyCasesDaily$County == as.character(countyCasesDaily$County)[i], 2]
  }
}


write.csv(countsToBeUpdated, "Data/ACC Healthcare Region Simulation  - Case Counts by County GA.csv")


