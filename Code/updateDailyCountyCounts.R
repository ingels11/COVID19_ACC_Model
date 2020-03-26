install.packages("rvest")
library(rvest)

webpage <- read_html("https://dph.georgia.gov/covid-19-daily-status-report")

tbls <- html_nodes(webpage, "table")

head(tbls)
tbls_ls <- webpage %>%
  html_nodes("table") %>%
  .[3] %>%
  html_table(fill = TRUE)

countyCasesDaily = as.data.frame(tbls_ls)

countyCasesDaily$date = Sys.Date()

countsToBeUpdated = read.csv("/Data/ACC Healthcare Region Simulation  - Case Counts by County GA.csv")
gaCounties$CASE.COUNT = as.Date(gaCounties$CASE.COUNT, format = "%m/%d/%y")


library(tidyr)
library(plyr)

for (i in 1:nrow(countyCasesDaily)){
  if (countyCasesDaily$County[i] %in%(names(gaCounties))){
    
    gaCounties[gaCounties$CASE.COUNT == Sys.Date(), as.character(countyCasesDaily$County)[i]] = 
      countyCasesDaily[countyCasesDaily$County == as.character(countyCasesDaily$County)[i], 2]
  }
}
