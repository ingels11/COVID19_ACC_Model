
#Modified Handel and Drake's code to get plot for ACC and surrounding counties that our hospitals serve
#Could be used to update daily based on this dataset that Meg is keeping track of...https://docs.google.com/spreadsheets/d/1C8e0vgNuaF1yMT2rxocEAeCGJ4Lqnsn8TnTuFL6qt8E/edit#gid=0

today <- Sys.Date()
start <- as.Date('12/01/2019',format='%m/%d/%Y')
today.day <- today - start + 1
#data <- read.csv('/Users/nicholasmallis/Downloads/CountyOne - Sheet1-2.csv')
data <- read.csv("Data/ACC Healthcare Region Simulation  - Case Counts by County GA.csv")
data <- data[1:10, c(1:6)]

data[,1] <- as.Date(as.character(data[,1]), format='%m-%d-%Y')
names(data)[1] <- 'date'



data.county <- data #head(data,-3)
#data.county$date <- as.Date(data.county$Date, format='%m-%d-%Y')
data.county[is.na(data.county)] <- 0
#data.county$total <- rowSums(data.county[,3:35])
data.county$total <- rowSums(data.county[,c(2:6)])
data.county$day <- data.county$date - start + 1  # Day 1 is given by "start"
data.county$cum.cases <- cumsum(data.county$total)

data.county$all <- data.county$Clarke+data.county$Oconee + data.county$Barrow + data.county$Madison + data.county$Morgan

library(ggplot2)
ggplot(data = data.county, aes(x = date, y = all)) +
  geom_bar(stat = "identity", fill = "black", width=.2) +
  labs(title = "Case Notifications for Clarke and Surrounding Counties",
       subtitle = "(Includes Clarke, Oconee, Barrow, Madison, and Morgan Counties)",
       x = "Date", y = "Case Notifications")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



#Modified Handel and Drake's code to get plot for ACC and surrounding counties that our hospitals serve
#Could be used to update daily based on this dataset that Meg is keeping track of...https://docs.google.com/spreadsheets/d/1C8e0vgNuaF1yMT2rxocEAeCGJ4Lqnsn8TnTuFL6qt8E/edit#gid=0

today <- Sys.Date()
start <- as.Date('12/01/2019',format='%m/%d/%Y')
today.day <- today - start + 1
#data <- read.csv('/Users/nicholasmallis/Downloads/CountyOne - Sheet1-2.csv')
data <- read.csv('/Users/nicholasmallis/Downloads/COVID - Sheet1-3.csv')
data[,1] <- as.Date(as.character(data[,1]), format='%m-%d-%Y')
names(data)[1] <- 'date'



data.county <- data #head(data,-3)
#data.county$date <- as.Date(data.county$Date, format='%m-%d-%Y')
data.county[is.na(data.county)] <- 0
#data.county$total <- rowSums(data.county[,3:35])
data.county$total <- rowSums(data.county[,c(2:6)])
data.county$day <- data.county$date - start + 1  # Day 1 is given by "start"
data.county$cum.cases <- cumsum(data.county$total)

data.county$all <- data.county$Clarke+data.county$Oconee + data.county$Barrow + data.county$Madison + data.county$Morgan

library(ggplot2)
ggplot(data = data.county, aes(x = date, y = all)) +
  geom_bar(stat = "identity", fill = "rosybrown", width=.5) +
  labs(title = "Case Notifications for Clarke and Surrounding Counties",
       subtitle = "(Includes Cases from Clarke, Oconee, Barrow, Madison, and Morgan Counties)",
       x = "Date", y = "Case Notifications")
