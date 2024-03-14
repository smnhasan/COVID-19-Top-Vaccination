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
library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(betareg)
library(car)


####Data Processing#######

setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update')

COVID <- read.csv("May30Data.csv")####Data Processing#######

#Subset from January to October
COVID <- subset(COVID, COVID$date >= "2020-01-01") #"2020-01-01"
COVID <- subset(COVID, COVID$date <= "2021-05-25") #"2020-08-24"

#Remove World and International information
COVID<-COVID[!(COVID$iso_code=="OWID_AFR" | COVID$iso_code=="OWID_ASI" | COVID$iso_code=="OWID_EUR" | COVID$iso_code=="OWID_INT" | 
              COVID$iso_code=="OWID_KOS" | COVID$iso_code=="OWID_NAM" | COVID$iso_code=="OWID_CYN" | COVID$iso_code=="OWID_SAM" |
                COVID$iso_code=="OWID_WRL"),]

#Reading other data
Day <- read.csv("Day.csv")

GHSI <- read.csv("GHSI.csv")
GHSI[GHSI == 0] <- NA

WGI <- read.csv("WGI.csv")
WGI[WGI == 0] <- NA

Obesity <- read.csv("Obesity.csv")

#Merge all data
finaldt1 <- merge(Day, GHSI,  by="location")

finaldt2 <- merge(finaldt1, WGI,  by="location")

finaldt3 <- merge(finaldt2, Obesity,  by="location")


####Overall#######

#Subset Peak date data
COVID_May25 <- subset(COVID, COVID$date == "2021-05-25") #"4/26/2020" "2020-04-26"

#Merge peak date data and others data
COVID_May25 <- merge(COVID_May25, finaldt3,  by="location")

#Creating CFR
COVID_May25$CFR <- (COVID_May25$total_deaths/COVID_May25$total_cases)

#Remove 0 from the data
COVID_May25$CFR[final_April26$CFR == 0] <- NA

## To disable scientific notation
options(scipen = 999)
#Beta regression model 
model_Before <- betareg(CFR ~  aged_65_older + population_density + total_tests_per_thousand + GHSI + gdp_per_capita +
                          +  WGI + Obesity_rate + total_vaccinations_per_hundred, data = COVID_May25)


#Multi-colinearity
vif(model_Before)

#Model summary
summary(model_Before)

#IRR
round(exp(model_Before$coefficients$mean),5)

#Confidence interval of IRR
round(exp(confint(model_Before)),5)




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

par(mfrow=c(3,2))



########################Israel#############################
COVID <- read.csv("Israel.csv")


COVID <- mutate(COVID, Date=as.POSIXct(date))
p <- ggplot(COVID, aes(x = Date)) + scale_x_datetime(expand=c(0,0),
                                                     date_breaks= "10 days", 
                                                     date_minor_breaks = "10 days", 
                                                     date_labels = "%d %b", 
                                                     limits = as.POSIXct(c("2020-12-19 00:00:00", "2021-02-12 00:00:00")))

p <- p + geom_line(aes(y = new_cases, colour = "Daily new cases"),size = 1.5)

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = total_vaccinations_per_hundred*200, colour = "Total vaccinations per hundred"),size = 1.5)

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./200, name = "Total vaccinations per hundred"))
# modifying colours and theme options
#p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Daily new cases",
              x = "Months",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.3, 0.9))