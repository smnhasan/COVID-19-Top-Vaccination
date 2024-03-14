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
library(psych) # for descriptive analysis
library(forecast)
library(lmtest)
library(tseries)

#Data Management

setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID <- read.csv("owid-covid-data (1).csv")

COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020
COVID <- subset(COVID, COVID$date <= "2021-08-31") #5/29/2021

#Remove World and International information
COVID<-COVID[!(COVID$iso_code=="OWID_AFR" | COVID$iso_code=="OWID_ASI" | COVID$iso_code=="OWID_EUR" | COVID$iso_code=="OWID_INT" | 
                 COVID$iso_code=="OWID_KOS" | COVID$iso_code=="OWID_NAM" | COVID$iso_code=="OWID_CYN" | COVID$iso_code=="OWID_SAM" |
                 COVID$iso_code=="OWID_WRL" | COVID$iso_code == "OWID_EUN" | COVID$iso_code == "OWID_OCE"),]
COVID$location
COVID <- COVID[ which(COVID$population >= 1000000), ]

#Creating CFR
COVID$CFR <- (COVID$total_deaths/COVID$total_cases)*100


options(scipen = 999)

#Week transformation (daily to weekly)
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
summary(COVID$Week2)


#Aggregate by weekly

world_CFR <- aggregate(COVID$CFR, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_CFR

colnames(world_CFR) <- c("Weeks", "location", "CFR")
world_CFR

world_total_cases_pm <- aggregate(COVID$total_cases_per_million, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_total_cases_pm 

colnames(world_total_cases_pm) <- c("Weeks", "location", "TotalCasepm")
world_total_cases_pm

world_new_cases <- aggregate(COVID$new_cases, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_new_cases

colnames(world_new_cases) <- c("Weeks", "location", "NewCases")
world_new_cases

world_aged_65_older <- aggregate(COVID$aged_65_older, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_aged_65_older

colnames(world_aged_65_older) <- c("Weeks", "location", "Age65Older")
world_aged_65_older

world_population_density <- aggregate(COVID$population_density, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_population_density

colnames(world_population_density) <- c("Weeks", "location", "PopulationDensity")
world_population_density

world_total_tests_per_thousand <- aggregate(COVID$total_tests_per_thousand, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_total_tests_per_thousand

colnames(world_total_tests_per_thousand) <- c("Weeks", "location", "TotalTestpt")
world_total_tests_per_thousand


world_gdp_per_capita <- aggregate(COVID$gdp_per_capita, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_gdp_per_capita

colnames(world_gdp_per_capita) <- c("Weeks", "location", "GDP")
world_gdp_per_capita

world_Vac <- aggregate(COVID$total_vaccinations_per_hundred, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_Vac

colnames(world_Vac) <- c("Weeks", "location", "Vaccinationph")
world_Vac

world_SI <- aggregate(COVID$stringency_index, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_SI

colnames(world_SI) <- c("Weeks", "location", "SI")
world_SI

#Merge all
ModelData1 <- merge(world_CFR, world_total_cases_pm, by=c("location", "Weeks"))

ModelData2 <- merge(ModelData1, world_new_cases, by=c("location", "Weeks"))

ModelData3 <- merge(ModelData2, world_aged_65_older, by=c("location", "Weeks"))

ModelData4 <- merge(ModelData3, world_population_density, by=c("location", "Weeks"))

ModelData5 <- merge(ModelData4, world_total_tests_per_thousand, by=c("location", "Weeks"))

ModelData6 <- merge(ModelData5, world_gdp_per_capita, by=c("location", "Weeks"))

ModelData7 <- merge(ModelData6, world_Vac, by=c("location", "Weeks"))

ModelData <- merge(ModelData7, world_SI, by=c("location", "Weeks"))

GHSI <- read.csv("GHSI.csv")
GHSI[GHSI == 0] <- NA

WGI <- read.csv("WGI.csv")
WGI[WGI == 0] <- NA

Obesity <- read.csv("Obesity.csv")

#Merge all data with GHSI and WGI

finaldt1 <- merge(GHSI, WGI,  by="location")

finaldt2 <- merge(finaldt1, Obesity,  by="location")

ModelData2 <- merge(ModelData, finaldt2,  by="location")

x <- nrow(ModelData2)
ModelData2$Vaccinationph2 <- ModelData2$Vaccinationph
for (i in 1:x) {
  if (ModelData2$Weeks[i] < 49)
    ModelData2$Vaccinationph2[i] = 0
  
}
print(ModelData2)
summary(COVID$Week2)

write.csv(ModelData2,"ModelData2.csv")

#Replace missing with previous observation

World <- read.csv("World88.csv")

#Top20 CFR Countries in last week
Top20 <- subset(World, World$Weeks == 88) #1/1/2020

Top20$CFRprcnt <- Top20$CFR*100

write.csv(Top20,"Weeks88Only.csv")

Top20$CFR
sort_Top20<- Top20[order(-Top20$CFRprcnt),]
sort_Top20$location

#Time series

#ARIMA
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(ggplot2)

COVID <- read.csv("owid-covid-data (1).csv")

COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020
COVID <- subset(COVID, COVID$date <= "2021-08-31") #5/29/2021

#Remove World and International information
COVID_WRL<-COVID[(COVID$iso_code=="OWID_WRL"),]
COVID_WRL$CFR

#Creating CFR
COVID_WRL$CFR <- (COVID_WRL$total_deaths/COVID_WRL$total_cases)*100

myts <- ts(COVID_WRL$CFR,frequency=365, start=c(2020,6))

auto.arima(myts)
Fit<-Arima(myts,order=c(5,2,1))
summary(Fit)
fcast <- forecast(Fit, h=10)

z <- autoplot(fcast, main=NULL)  +
  autolayer(fcast$mean, series="Forecast") +
  autolayer(fitted(Fit), series='Fitted') + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("Days") + ylab("Reported CFR (%)") + ggtitle("                  Auto-Regressive Integrated Moving Average Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))
z

#R2
SSE <- sum((resid(Fit[1:588]))^2)
SST <- sum((COVID_WRL$CFR[1:588] - mean(COVID_WRL$CFR[1:588]))^2)
R_square <- 1 - SSE / SST
R_square


#Prophet

write.csv(COVID_WRL,"COVID_WRL.csv")

World <- read.csv("COVID_WRL.csv")

history <- data.frame(ds = seq(as.Date('2020-01-22'), as.Date('2021-08-31'), by = 'd'),
                      y = World$CFR)

m3 <- prophet(history)
future <- make_future_dataframe(m3, periods = 10)
fcst3 <- predict(m3, future)
y <-plot(m3, fcst3, xlab="Days", ylab="Reported CFR (%)") + ggtitle("                                                    Automatic Forecasting Time-series Model") + theme(
  plot.title = element_text(size=12))
plot(y)

SSE <- sum((history$y[1:588] - fcst3$yhat[c(1:588)])^2)
SST <- sum((history$y[1:588] - mean(history$y[1:588]))^2)
R_square <- 1 - SSE / SST
R_square

last_fcst3 <- fcst3[588,]
rmse <- sqrt(mean((history$y - fcst3$yhat[c(1:588)])^2))
mae <- mean(abs((history$y - fcst3$yhat[c(1:588)])))
final <- cbind(last_fcst3, rmse, mae)
final



####SES########

library(tidyverse) 
library(fpp2) 

ses.goog <- ses(myts,  
                alpha = .38, 
                h = 10) 
summary(ses.goog)

fcast <- forecast(ses.goog, h=10)
x <- autoplot(ses.goog, main=NULL)+
  autolayer(fcast$mean, series="Forecast") +
  autolayer(fitted(ses.goog), series='Fitted') + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("Days") + ylab("Reported CFR (%)") +
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval")) + ggtitle("                  Simple Exponential Smoothing Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))

x

#R2
SSE <- sum((resid(ses.goog[1:588]))^2)
SST <- sum((COVID_WRL$CFR[1:588] - mean(COVID_WRL$CFR[1:588]))^2)
R_square <- 1 - SSE / SST
R_square



#Menn kendal
library(Kendall)
library(trend)
World <- read.csv("World88.csv")
world_CFR <- aggregate(ModelData2$CFR, by= list(ModelData2$Weeks), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_CFR$x <- world_CFR$x

myts <- ts(world_CFR$x[4:88])
t.test(world_CFR$x[4:88])$"conf.int"
mean(world_CFR$x[4:88])

MannKendall(myts)
sens.slope(myts, conf.level = 0.95)


#GLMM Model

library(glmmTMB)
World$Vaccinationph2
World$lw <- log(World$Weeks)
World$vml <- World$Vaccinationph2/10000
World$vml2 <- scale(World$vml)

World$TTs <- World$TotalTestpt/1000
World$TTs2 <- scale(World$TTs)
World$GDP2 <- scale(World$GDP)
World$PopulationDensity2 <- World$PopulationDensity/1000
World$PopulationDensity22 <- scale(World$PopulationDensity2)
World$GHSI2 <- scale(World$GHSI)
World$WGI2 <- scale(World$WGI)
World$Age65Older2 <- World$Age65Older/10000
World$Age65Older22 <- scale(World$Age65Older2)
World$Obesity_rate2 <- World$Obesity_rate/10000
World$Obesity_rate22 <- scale(World$Obesity_rate2)
World$CFR2 <- World$CFR/100
World$CFR22 <- scale(World$CFR2)
nrow(World)
write.csv(World,"WorldData88.csv")

library(glmmTMB)
library(DHARMa)
library(performance)
fit <- glmmTMB(World$CFR2~ vml2*lw +Age65Older22 + PopulationDensity22 + GDP2 + GHSI2 
               + WGI2 + Obesity_rate22 + scale(SI) + (1|location) + (1 | lw), na.action=na.omit, family = beta_family(link = "logit"),  data = World)

summary(fit)
round(exp(confint(fit)),3)
options(scipen = 999) 
performance::performance(fit)
