library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(lubridate)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(ggplot2)


####Data Processing#######

setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update')

COVID <- read.csv("May30Data.csv")
COVID
COVID <- subset(COVID, COVID$date >= "2020-01-22") #1/1/2020
COVID <- subset(COVID, COVID$date <= "2021-05-30") #5/29/2021
COVID <- subset(COVID, COVID$location == "World")
COVID$CFR <- (COVID$total_deaths / COVID$total_cases)*100







str(COVID)

COVID <- COVID %>%
  mutate(DATE = as.Date(date, format = "%Y-%m-%D"))
COVID$date
class(COVID$date)

# plot the data using ggplot2 and pipes
COVID %>%
  ggplot(aes(x = date, y = CFR)) +
  geom_point(color = "darkorchid4") +
  labs(title = "",
       y = "Reported (CFR)",
       x = "Months") + theme_bw(base_size = 15)




COVID$date

history <- data.frame(ds = seq(as.Date('2020-01-22'), as.Date('2021-05-01'), by = 'd'),
                      y = COVID$CFR)
plot(history)


#ARIMA
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)

library(zoo)

myts <- ts(COVID$CFR[1:466])

p <- autoplot(myts) + geom_line(color = "black", size = 1) + labs(y="Reported CFR (%)", x = "Days")

