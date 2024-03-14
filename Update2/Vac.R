#####################Vaccination and COVID-19 in South-Asia#########################
#                            Mohammad Nayeem Hasan                                 #
####################################################################################

library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(betareg)
library(car)
library(gapminder)
library(dplyr)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)


setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID <- read.csv("owid-covid-data_31_8.csv")

COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020
COVID <- subset(COVID, COVID$date <= "2021-08-31") #5/29/2021

#Creating CFR
COVID$CFR <- (COVID$total_deaths/COVID$total_cases)*100


######ISRAEL
COVID <- subset(COVID, COVID$location == "Israel")

library(lubridate)
COVID$date2 <- as.Date(as.character(COVID$date),format="%Y-%m-%d")
COVID$date2
COVID$Week <- week(as.Date(as.character(COVID$date2),format="%Y-%m-%d"))
COVID$Week

x <- nrow(COVID)
COVID$Week2 <- COVID$Week
for (i in 1:x) {
  if (COVID$date[i] >= "2021-01-01")
    COVID$Week2[i] = COVID$Week[i]+53

  }

print(COVID$Week2)

IS_CFR <- aggregate(COVID$CFR, by= list(COVID$Week2), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
IS_CFR

IS_Vac <- aggregate(COVID$total_vaccinations_per_hundred, by= list(COVID$Week2), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
IS_Vac


data <- data.frame(IS_CFR, IS_Vac)


p <- ggplot(data, aes(x = Group.1))
p
p <- p + geom_line(aes(y = data$x))
p
# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = data$x.1/100))
p
# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./100, name = "Total vaccinations per hundred"))
# modifying colours and theme options
#p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Reported CFR (%)",
              x = "Weeks",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.3, 0.9))
p





#######United Kingdom
setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID <- read.csv("owid-covid-data.csv")

COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020
COVID <- subset(COVID, COVID$date <= "2021-05-31") #5/29/2021
#Creating CFR
COVID$CFR <- (COVID$total_deaths/COVID$total_cases)*100

COVID <- subset(COVID, COVID$location == "United Kingdom")

library(lubridate)
COVID$date2 <- as.Date(as.character(COVID$date),format="%Y-%m-%d")
COVID$date2
COVID$Week <- week(as.Date(as.character(COVID$date2),format="%Y-%m-%d"))
COVID$Week
COVID$row_num <- seq.int(nrow(COVID))
x <- nrow(COVID)
COVID$Week2 <- COVID$Week
for (i in 1:x) {
  if (COVID$date[i] >= "2021-01-01")
  COVID$Week2[i] = COVID$Week[i]+53
  
}

print(COVID$Week2)
COVID$total_vaccinations_per_hundred

UK_CFR <- aggregate(COVID$CFR, by= list(COVID$Week2), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
UK_CFR

UK_Vac <- aggregate(COVID$total_vaccinations_per_hundred, by= list(COVID$Week2), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
UK_Vac


data <- data.frame(UK_CFR, UK_Vac)

p <- ggplot(data, aes(x = Group.1))
p
p <- p + geom_line(aes(y = data$x))
p
# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = data$x.1/15))
p
# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./15, name = "Total vaccinations per hundred"))
# modifying colours and theme options
#p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Reported CFR (%)",
              x = "Weeks",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.3, 0.9))
p




#######United States
setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID <- read.csv("owid-covid-data.csv")

COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020
COVID <- subset(COVID, COVID$date <= "2021-05-31") #5/29/2021
#Creating CFR
COVID$CFR <- (COVID$total_deaths/COVID$total_cases)*100

COVID <- subset(COVID, COVID$location == "United States")

library(lubridate)
COVID$date2 <- as.Date(as.character(COVID$date),format="%Y-%m-%d")
COVID$date2
COVID$Week <- week(as.Date(as.character(COVID$date2),format="%Y-%m-%d"))
COVID$Week
COVID$row_num <- seq.int(nrow(COVID))
x <- nrow(COVID)
COVID$Week2 <- COVID$Week
for (i in 1:x) {
  if (COVID$date[i] >= "2021-01-01")
  COVID$Week2[i] = COVID$Week[i]+53
  
}

print(COVID$Week2)
COVID$total_vaccinations_per_hundred

US_CFR <- aggregate(COVID$CFR, by= list(COVID$Week2), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
US_CFR

US_Vac <- aggregate(COVID$total_vaccinations_per_hundred, by= list(COVID$Week2), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
US_Vac


data <- data.frame(US_CFR, US_Vac)

p <- ggplot(data, aes(x = Group.1))
p
p <- p + geom_line(aes(y = data$x))
p
# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = data$x.1/20))
p
# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./20, name = "Total vaccinations per hundred"))
# modifying colours and theme options
#p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Reported CFR (%)",
              x = "Weeks",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.3, 0.9))
p


#######Top 20
setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID <- read.csv("owid-covid-data.csv")

COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020
COVID <- subset(COVID, COVID$date <= "2021-05-31") #5/29/2021
#Creating CFR
COVID$CFR <- (COVID$total_deaths/COVID$total_cases)*100

COVID <- subset(COVID, COVID$date == "2021-08-31")

COVID <- subset(COVID, COVID$population >= 1000000) #1/1/2020

sort_COVID<- COVID[order(-COVID$total_vaccinations_per_hundred),]
sort_COVID$location


setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID <- read.csv("owid-covid-data_8_31.csv")

COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020
COVID <- subset(COVID, COVID$date <= "2021-08-31") #5/29/2021
#Creating CFR
COVID$CFR <- (COVID$total_deaths/COVID$total_cases)*100

COVID_top20<- COVID[(COVID$location=="United Arab Emirates" | COVID$location=="Israel" |
                                 COVID$location=="Bahrain" | COVID$location=="Chile" |
                                 COVID$location=="Mongolia" | COVID$location=="United Kingdom" |
                                 COVID$location=="Hungary" | COVID$location=="Qatar" |
                                 COVID$location=="Uruguay" | COVID$location=="Singapore" |
                                 COVID$location=="Canada" |
                                 COVID$location=="Germany" | COVID$location=="Lithuania" |
                                 COVID$location=="Belgium" | COVID$location=="Italy" |
                                 COVID$location=="Spain" | COVID$location=="Austria" |
                                 COVID$location=="Switzerland" | COVID$location=="Portugal" |
                                 COVID$location=="France"),]

COVID_top20


library(lubridate)
COVID_top20$date2 <- as.Date(as.character(COVID_top20$date),format="%Y-%m-%d")
COVID_top20$date2
COVID_top20$Week <- week(as.Date(as.character(COVID_top20$date2),format="%Y-%m-%d"))
COVID_top20$Week
x <-  nrow(COVID_top20)
COVID_top20$Week2 <- COVID_top20$Week
for (i in 1:x) {
  if (COVID_top20$date[i] >= "2021-01-01")
    COVID_top20$Week2[i] = COVID_top20$Week[i]+53
  
}

T20_CFR <- aggregate(COVID_top20$CFR, by= list(COVID_top20$Week2), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
T20_CFR

T20_Vac <- aggregate(COVID_top20$total_vaccinations_per_hundred, by= list(COVID_top20$Week2), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
T20_Vac


data <- data.frame(T20_CFR, T20_Vac)

p <- ggplot(data, aes(x = Group.1))
p
p <- p + geom_line(aes(y = data$x))
p
# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = data$x.1/10))
p
# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./10, name = "Total vaccinations per hundred"))
# modifying colours and theme options
#p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Reported CFR (%)",
              x = "Weeks",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.3, 0.9))
p




####time series

library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(ggplot2)

# Model building
world <- read.csv("WorldData.csv")
world$date


history <- data.frame(ds = seq(as.Date('2020-01-01'), as.Date('2021-05-30'), by = 'd'),
                      y = world$CFR)
m3 <- prophet(history)
future <- make_future_dataframe(m3, periods = 10)
fcst3 <- predict(m3, future)
y <-plot(m3, fcst3, xlab="Months", ylab="Reported CFR") + ggtitle("                                         Automatic Forecasting Time-series Model")
y

#ARIMA
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)

library(zoo)

myts <- ts(world$CFR ,start=c(2020), frequency = 365.25)

auto.arima(myts)
Fit<-Arima(myts,order=c(0,2,2))
fcast <- forecast(Fit, h=10)

z <- autoplot(fcast, main=NULL)  +
  autolayer(fcast$mean, series="Forecast") +
  autolayer(fitted(Fit), series='Fitted') + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("Days") + ylab("Reported CFR") + ggtitle("                                 Auto-Regressive Integrated Moving Average Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))
z


####SES########

library(tidyverse)
library(fpp2) 

ses.goog <- ses(myts,
                h = 10) 

fcast <- forecast(ses.goog, h=10)
x <- autoplot(ses.goog, main=NULL)+
  autolayer(fcast$mean, series="Forecast") +
  autolayer(fitted(ses.goog), series='Fitted') + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("Months") + ylab("Reported CFR") +ggtitle("                                             Simple Exponential Smoothing Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))
x

