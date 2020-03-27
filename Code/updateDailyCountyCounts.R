## This code is run daily to ensure that number of cases in our working dataset is properly pulled from the 
## Georgia DPH website with number of confirmed cases in each county
library(rvest)

library(lubridate)

# Tell which website to look in / scrape HTML cod 
dailyCountWebsite <- read_html("https://dph.georgia.gov/covid-19-daily-status-report")


# Telling R to grab only the TABLES from the website
tbls <- html_nodes(dailyCountWebsite, "table")


# head(tbls)

# From looking at the website, the table we want is the 3rd table on the site
tbls_ls <- dailyCountWebsite %>%
  html_nodes("table") %>%
  .[3] %>%                    ## Indicating we want the 3rd table
  html_table(fill = TRUE)

# Creating a dataframe from the picked table
countyCasesDaily = as.data.frame(tbls_ls)


# countyCasesDaily$date = ymd(Sys.Date())


# Reading in our working (full) dataset of all counties that we want to populate 
countsToBeUpdated = read.csv("Data/ACC Healthcare Region Simulation  - Case Counts by County GA.csv")
countsToBeUpdated[is.na(countsToBeUpdated)] <- 0
names(countsToBeUpdated)[1] = "date"
countsToBeUpdated$date = lubridate::ymd(countsToBeUpdated$date)






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
write.csv(countsToBeUpdated, "Data/ACC Healthcare Region Simulation  - Case Counts by County GA.csv", row.names = F)








## This bit of code just goes through and populates counties in the Athens area -- primary and secondary
## I guess we could just subset the full working dataset above and indicate which we want, but 
## to minimize that work, I just made a CSV file with only the primary / secondary counties and put an 
## indicator variable to tell which kind (include in both primary AND sec or just secondary)


primSecCounties = read.csv("Data/primary and secondary counties cases.csv")
primSecCounties[is.na(primSecCounties)] <- 0
names(primSecCounties)[1] = "date"
primSecCounties$date = lubridate::ymd(primSecCounties$date)


library(tidyr)
library(plyr)

for (i in 1:nrow(countyCasesDaily)){
  
  if (countyCasesDaily$County[i] %in%(names(primSecCounties))){
    
    primSecCounties[primSecCounties$date == Sys.Date(), as.character(countyCasesDaily$County)[i]] = 
      countyCasesDaily[countyCasesDaily$County == as.character(countyCasesDaily$County)[i], 2]
  }
}


write.csv(primSecCounties, "Data/primary and secondary counties cases.csv", row.names = F)





