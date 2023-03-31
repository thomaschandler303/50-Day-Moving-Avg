#Program: Chandler_project2.R
#Program displays a chart of the 50-day moving average for a stock
#Author: Thomas Chandler
#Date: April 10, 2022

rm(list=ls())
install.packages("quantmod")
install.packages("ggplot2")
library(quantmod)
library(ggplot2)

#Get stock data from website
nvda <- getSymbols("NVDA", src = "yahoo", from = "2002-01-01", to = "2022-02-01", auto.assign = FALSE)

#Create subset of stock data to append new data
nvda_mm <- subset(nvda, index(nvda) >= "2008-01-01")



#Calculate the 50-day moving average (selected 6th column, perform computation on data)
nvda_mm50 <- rollmean(nvda_mm[,6], 50, fill = list(NA, NULL, NA), align = "right")
nvda_mm30 <- rollmean(nvda_mm[,6], 30, fill = list(NA, NULL, NA), align = "right")



#Append new data and rename column headers (Merging data, added columns)
nvda_new <- merge.xts(nvda_mm,nvda_mm50, join = "right")
nvda_new2 <- merge.xts(nvda_new, nvda_mm30, join = "right")

#Perform function on data
colnames(nvda_new2) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "50-Day MA", "30-Day MA")


#Data visualization
ggplot(nvda_mm, aes(x = index(nvda_mm))) +
  geom_line(aes(y = nvda_mm[,6], color = "NVDA")) + ggtitle("Nvidia prices series") +
  geom_line(aes(y = nvda_mm30, color = "MM30")) +
  geom_line(aes(y = nvda_mm50, color = "MM50")) + xlab("Date") + ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  scale_colour_manual("Series", values=c("NVDA"="gray40", "MM30"="firebrick4", "MM50"="darkcyan"))



