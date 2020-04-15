
library(readr)
data <- read_csv("Colquitt_and_surrounding/Data/SW_primary and secondary counties cases.csv")

datatwo <- data[1:32, ]

datatwo$total <- rowSums(datatwo[,c(2:15)])


library(lubridate)
library(scales)
library(ggplot2)

# line that makes dates look better on figure
datatwo$newdate <- as.Date(parse_date_time(datatwo$date, "%y/%m/%d"))

#using plot
plot(datatwo$newdate, datatwo$total, type='h', lwd=7, col='black',
     lend='butt', xlab='Date', ylab='Case Notifications', main='Cumulative Case Notifications: Southwest Georgia Health District')

#ggplot no grid
library(ggplot2)
ggplot(data = datatwo, aes(x = newdate, y = total)) +
  geom_bar(stat = "identity", fill = "black", width=.5) + 
  labs(title = "Cumulative Case Notifications: Southwest Georgia Health District",
       x = "Date", y = "Case Notifications") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#using ggplot with grid
library(ggplot2)
ggplot(data = datatwo, aes(x = newdate, y = total)) +
  geom_bar(stat = "identity", fill = "black", width=.5) + 
  labs(title = "Cumulative Case Notifications: Southwest Georgia Health District",
       x = "Date", y = "Case Notifications") 


