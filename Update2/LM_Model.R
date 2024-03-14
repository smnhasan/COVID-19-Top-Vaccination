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


setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID <- read.csv("owid-covid-data_31_8.csv")

COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020
COVID <- subset(COVID, COVID$date <= "2021-08-31") #5/29/2021

#Remove World and International information
COVID<-COVID[!(COVID$iso_code=="OWID_AFR" | COVID$iso_code=="OWID_ASI" | COVID$iso_code=="OWID_EUR" | COVID$iso_code=="OWID_INT" | 
                 COVID$iso_code=="OWID_KOS" | COVID$iso_code=="OWID_NAM" | COVID$iso_code=="OWID_CYN" | COVID$iso_code=="OWID_SAM" |
                 COVID$iso_code=="OWID_WRL" | COVID$iso_code == "OWID_EUN" | COVID$iso_code == "OWID_OCE"),]

COVID <- COVID[ which(COVID$population > 1000000), ]

#Creating CFR
COVID$CFR <- (COVID$total_deaths/COVID$total_cases)

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

#Merge peak date data and others data
COVID <- merge(COVID, finaldt3,  by="location")

options(scipen = 999)

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



world_CFR <- aggregate(COVID$CFR, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_CFR

#world_CFR <- subset(world_CFR, world_CFR$Group.1 >= 79) #5/29/2021
#world_CFR

colnames(world_CFR) <- c("Weeks", "Location", "CFR")
world_CFR

world_total_cases_pm <- aggregate(COVID$total_cases_per_million, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_total_cases_pm 

#world_total_cases_pm <- subset(world_total_cases_pm, world_total_cases_pm$Group.1 >= 79) #5/29/2021
#world_total_cases_pm

colnames(world_total_cases_pm) <- c("Weeks", "Location", "TotalCasepm")
world_total_cases_pm

world_new_cases <- aggregate(COVID$new_cases, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_new_cases

colnames(world_new_cases) <- c("Weeks", "Location", "NewCases")
world_new_cases

world_aged_65_older <- aggregate(COVID$aged_65_older, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_aged_65_older

#world_aged_65_older <- subset(world_aged_65_older, world_aged_65_older$Group.1 >= 79) #5/29/2021
#world_aged_65_older

colnames(world_aged_65_older) <- c("Weeks", "Location", "Age65Older")
world_aged_65_older

world_population_density <- aggregate(COVID$population_density, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_population_density

#world_population_density <- subset(world_population_density, world_population_density$Group.1 >= 79) #5/29/2021
#world_population_density

colnames(world_population_density) <- c("Weeks", "Location", "PopulationDensity")
world_population_density

world_total_tests_per_thousand <- aggregate(COVID$total_tests_per_thousand, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_total_tests_per_thousand

#world_total_tests_per_thousand <- subset(world_total_tests_per_thousand, world_total_tests_per_thousand$Group.1 >= 79) #5/29/2021
#world_total_tests_per_thousand

colnames(world_total_tests_per_thousand) <- c("Weeks", "Location", "TotalTestpt")
world_total_tests_per_thousand

world_GHSI <- aggregate(COVID$GHSI, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_GHSI

#world_GHSI <- subset(world_GHSI, world_GHSI$Group.1 >= 79) #5/29/2021
#world_GHSI

colnames(world_GHSI) <- c("Weeks", "Location", "GHSI")
world_GHSI

world_gdp_per_capita <- aggregate(COVID$gdp_per_capita, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_gdp_per_capita

#world_gdp_per_capita <- subset(world_gdp_per_capita, world_gdp_per_capita$Group.1 >= 79) #5/29/2021
#world_gdp_per_capita

colnames(world_gdp_per_capita) <- c("Weeks", "Location", "GDP")
world_gdp_per_capita



world_WGI <- aggregate(COVID$WGI, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_WGI

#world_WGI <- subset(world_WGI, world_WGI$Group.1 >= 75) #5/29/2021
#world_WGI

colnames(world_WGI) <- c("Weeks", "Location", "WGI")
world_WGI

world_Obesity_rate <- aggregate(COVID$Obesity_rate, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_Obesity_rate

#world_Obesity_rate <- subset(world_Obesity_rate, world_Obesity_rate$Group.1 >= 79) #5/29/2021
#world_Obesity_rate

colnames(world_Obesity_rate) <- c("Weeks", "Location", "Obesity")
world_Obesity_rate

world_Vac <- aggregate(COVID$total_vaccinations_per_hundred, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_Vac

#world_Vac <- subset(world_Vac, world_Vac$Group.1 >= 75) #5/29/2021
#world_Vac

colnames(world_Vac) <- c("Weeks", "Location", "Vaccinationph")
world_Vac

ModelData1 <- merge(world_CFR, world_total_cases_pm, by=c("Location", "Weeks"))

ModelData2 <- merge(ModelData1, world_new_cases, by=c("Location", "Weeks"))

ModelData3 <- merge(ModelData2, world_aged_65_older, by=c("Location", "Weeks"))

ModelData4 <- merge(ModelData3, world_population_density, by=c("Location", "Weeks"))

ModelData5 <- merge(ModelData4, world_total_tests_per_thousand, by=c("Location", "Weeks"))

ModelData6 <- merge(ModelData5, world_GHSI, by=c("Location", "Weeks"))

ModelData7 <- merge(ModelData6, world_gdp_per_capita, by=c("Location", "Weeks"))

ModelData8 <- merge(ModelData7, world_WGI, by=c("Location", "Weeks"))

ModelData9 <- merge(ModelData8, world_Obesity_rate, by=c("Location", "Weeks"))

ModelData <- merge(ModelData9, world_Vac, by=c("Location", "Weeks"))


nrow(ModelData)
ModelData$id <- c(1:11515)
library(nlme)
mmrm <- gls(CFR ~ Vaccinationph*Weeks + Age65Older + PopulationDensity + TotalTestpt + GHSI + GDP + WGI + Obesity ,
            na.action=na.omit, data= ModelData,
            correlation=nlme::corSymm(form=~1 | id),
            weights=nlme::varIdent(form=~1|Weeks))
summary(mmrm)




View(ModelData)
# model <- betareg(ModelData$CFR ~  ModelData$Age65Older + ModelData$PopulationDensity + ModelData$TotalTestpt   + ModelData$GHSI + ModelData$GDP + ModelData$WGI + ModelData$Obesity + ModelData$Vaccinationph, 
#              data = ModelData, link= "log")
# 
# vif(model)
# #Model summary
# summary(model)

#IRR
#round(exp(model$coefficients$mean),5)

#Confidence interval of IRR
#round(exp(confint(model)),5)



#ARIMAX

ModelData2 <- na.omit(ModelData)
z<- ModelData2$CFR
z
auto.arima(z)


c<-Arima(z, order=c(1,0,0)) 
checkresiduals(c)

Arima(z, order=c(1,0,0))$bic
Arima(z, order=c(2,0,0))$bic
Arima(z, order=c(1,0,1))$bic
Arima(z, order=c(1,1,0))$bic
Arima(z, order=c(1,1,1))$bic

Fit<-Arima(z, order=c(1,0,0))  ## some AR or MA terms coefficients are insignificant, so first of all select a model which all parameters are significant and pass residual test
library(lmtest)
summary(Fit)
### model diagnosis
checkresiduals(Fit) 

xreg <- cbind(ModelData2$Age65Older , ModelData2$GHSI , ModelData2$GDP 
              , ModelData2$WGI , ModelData2$Vaccinationph)
colnames(xreg) <- c("Age65Older", "GHSI", "GDP", "WGI","Vaccinationph")

modArima <- Arima(ModelData2$CFR, xreg=xreg, order=c(1,0,0))
modArima

exp(modArima$coef)
summary(modArima)
checkresiduals(modArima)


#####Top 20

setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
# COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020
# COVID <- subset(COVID, COVID$date <= "2021-06-30") #5/29/2021

COVID <- read.csv("World2.csv")
COVID$ï..Location

#Remove World and International information
ModelData<-COVID[(COVID$ï..Location=="United Arab Emirates" | COVID$ï..Location=="Israel" |
                COVID$ï..Location=="Bahrain" | COVID$ï..Location=="Chile" |
                COVID$ï..Location=="Mongolia" | COVID$ï..Location=="United Kingdom" |
                COVID$ï..Location=="Hungary" | COVID$ï..Location=="Qatar" |
                COVID$ï..Location=="Uruguay" | COVID$ï..Location=="Singapore" |
                COVID$ï..Location=="Canada" |
                COVID$ï..Location=="Germany" | COVID$ï..Location=="Lithuania" |
                COVID$ï..Location=="Belgium" | COVID$ï..Location=="Italy" |
                COVID$ï..Location=="Spain" | COVID$ï..Location=="Austria" |
                COVID$ï..Location=="Switzerland" | COVID$ï..Location=="Portugal" |
                COVID$ï..Location=="France"),]

# COVID <- COVID[ which(COVID$population > 1000000), ]
# 
# #Creating CFR
# COVID$CFR <- (COVID$total_deaths/COVID$total_cases)
# 
# #Reading other data
# Day <- read.csv("Day.csv")
# 
# GHSI <- read.csv("GHSI.csv")
# GHSI[GHSI == 0] <- NA
# 
# WGI <- read.csv("WGI.csv")
# WGI[WGI == 0] <- NA
# 
# Obesity <- read.csv("Obesity.csv")
# 
# #Merge all data
# finaldt1 <- merge(Day, GHSI,  by="location")
# 
# finaldt2 <- merge(finaldt1, WGI,  by="location")
# 
# finaldt3 <- merge(finaldt2, Obesity,  by="location")
# 
# #Merge peak date data and others data
# COVID <- merge(COVID, finaldt3,  by="location")
# 
# options(scipen = 999)
# 
# library(lubridate)
# COVID$date2 <- as.Date(as.character(COVID$date),format="%Y-%m-%d")
# COVID$date2
# COVID$Week <- week(as.Date(as.character(COVID$date2),format="%Y-%m-%d"))
# COVID$Week
# 
# x <- nrow(COVID)
# COVID$Week2 <- COVID$Week
# for (i in 1:x) {
#   if (COVID$date[i] >= "2021-01-01")
#     COVID$Week2[i] = COVID$Week[i]+53
#   
# }
# print(COVID$Week2)
# summary(COVID$Week2)
# 
# 
# 
# world_CFR <- aggregate(COVID$CFR, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) 
# world_CFR
# 
# #world_CFR <- subset(world_CFR, world_CFR$Group.1 >= 79) #5/29/2021
# #world_CFR
# 
# colnames(world_CFR) <- c("Weeks", "Location", "CFR")
# world_CFR
# 
# world_total_cases_pm <- aggregate(COVID$total_cases_per_million, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_total_cases_pm 
# 
# #world_total_cases_pm <- subset(world_total_cases_pm, world_total_cases_pm$Group.1 >= 79) #5/29/2021
# #world_total_cases_pm
# 
# colnames(world_total_cases_pm) <- c("Weeks", "Location", "TotalCasepm")
# world_total_cases_pm
# 
# world_new_cases <- aggregate(COVID$new_cases, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_new_cases
# 
# colnames(world_new_cases) <- c("Weeks", "Location", "NewCases")
# world_new_cases
# 
# world_aged_65_older <- aggregate(COVID$aged_65_older, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_aged_65_older
# 
# #world_aged_65_older <- subset(world_aged_65_older, world_aged_65_older$Group.1 >= 79) #5/29/2021
# #world_aged_65_older
# 
# colnames(world_aged_65_older) <- c("Weeks", "Location", "Age65Older")
# world_aged_65_older
# 
# world_population_density <- aggregate(COVID$population_density, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_population_density
# 
# #world_population_density <- subset(world_population_density, world_population_density$Group.1 >= 79) #5/29/2021
# #world_population_density
# 
# colnames(world_population_density) <- c("Weeks", "Location", "PopulationDensity")
# world_population_density
# 
# world_total_tests_per_thousand <- aggregate(COVID$total_tests_per_thousand, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_total_tests_per_thousand
# 
# #world_total_tests_per_thousand <- subset(world_total_tests_per_thousand, world_total_tests_per_thousand$Group.1 >= 79) #5/29/2021
# #world_total_tests_per_thousand
# 
# colnames(world_total_tests_per_thousand) <- c("Weeks", "Location", "TotalTestpt")
# world_total_tests_per_thousand
# 
# world_GHSI <- aggregate(COVID$GHSI, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_GHSI
# 
# #world_GHSI <- subset(world_GHSI, world_GHSI$Group.1 >= 79) #5/29/2021
# #world_GHSI
# 
# colnames(world_GHSI) <- c("Weeks", "Location", "GHSI")
# world_GHSI
# 
# world_gdp_per_capita <- aggregate(COVID$gdp_per_capita, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_gdp_per_capita
# 
# #world_gdp_per_capita <- subset(world_gdp_per_capita, world_gdp_per_capita$Group.1 >= 79) #5/29/2021
# #world_gdp_per_capita
# 
# colnames(world_gdp_per_capita) <- c("Weeks", "Location", "GDP")
# world_gdp_per_capita
# 
# 
# 
# world_WGI <- aggregate(COVID$WGI, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_WGI
# 
# #world_WGI <- subset(world_WGI, world_WGI$Group.1 >= 79) #5/29/2021
# #world_WGI
# 
# colnames(world_WGI) <- c("Weeks", "Location", "WGI")
# world_WGI
# 
# world_Obesity_rate <- aggregate(COVID$Obesity_rate, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_Obesity_rate
# 
# #world_Obesity_rate <- subset(world_Obesity_rate, world_Obesity_rate$Group.1 >= 79) #5/29/2021
# #world_Obesity_rate
# 
# colnames(world_Obesity_rate) <- c("Weeks", "Location", "Obesity")
# world_Obesity_rate
# 
# world_Vac <- aggregate(COVID$total_vaccinations_per_hundred, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_Vac
# 
# #world_Vac <- subset(world_Vac, world_Vac$Group.1 >= 79) #5/29/2021
# #world_Vac
# 
# colnames(world_Vac) <- c("Weeks", "Location", "Vaccinationph")
# world_Vac
# 
# ModelData1 <- merge(world_CFR, world_total_cases_pm, by=c("Location", "Weeks"))
# 
# ModelData2 <- merge(ModelData1, world_new_cases, by=c("Location", "Weeks"))
# 
# ModelData3 <- merge(ModelData2, world_aged_65_older, by=c("Location", "Weeks"))
# 
# ModelData4 <- merge(ModelData3, world_population_density, by=c("Location", "Weeks"))
# 
# ModelData5 <- merge(ModelData4, world_total_tests_per_thousand, by=c("Location", "Weeks"))
# 
# ModelData6 <- merge(ModelData5, world_GHSI, by=c("Location", "Weeks"))
# 
# ModelData7 <- merge(ModelData6, world_gdp_per_capita, by=c("Location", "Weeks"))
# 
# ModelData8 <- merge(ModelData7, world_WGI, by=c("Location", "Weeks"))
# 
# ModelData9 <- merge(ModelData8, world_Obesity_rate, by=c("Location", "Weeks"))
# 
# ModelData <- merge(ModelData9, world_Vac, by=c("Location", "Weeks"))
# 
# write.csv(ModelData,"TOP20.csv", row.names = FALSE)
# 
# 
# model <- betareg(ModelData$CFR ~  ModelData$Age65Older + ModelData$PopulationDensity + ModelData$TotalTestpt   + ModelData$GHSI + ModelData$GDP + ModelData$WGI + ModelData$Obesity + ModelData$Vaccinationph, 
#                  data = ModelData, link= "log")
# 
# vif(model)
# #Model summary
# summary(model)
# 
# #IRR
# round(exp(model$coefficients$mean),5)
# 
# #Confidence interval of IRR
# round(exp(confint(model)),5)

#ARIMAX

ModelData2 <- na.omit(ModelData)
z<- ModelData2$CFR
z
auto.arima(z)
c<-Arima(z, order=c(0,1,0)) 
checkresiduals(c)

Arima(z, order=c(0,1,0))$bic
Arima(z, order=c(1,1,0))$bic
Arima(z, order=c(1,0,1))$bic
Arima(z, order=c(1,1,1))$bic
Arima(z, order=c(1,0,0))$bic
Arima(z, order=c(1,1,1))$bic

Fit<-Arima(z, order=c(0,1,0))  ## some AR or MA terms coefficients are insignificant, so first of all select a model which all parameters are significant and pass residual test
library(lmtest)
summary(Fit)
### model diagnosis
checkresiduals(Fit) 

xreg <- cbind(ModelData2$Age65Older , ModelData2$GHSI , ModelData2$GDP 
              , ModelData2$WGI , ModelData2$Vaccinationph)
colnames(xreg) <- c("Age65Older", "GHSI", "GDP", "WGI","Vaccinationph")

modArima <- Arima(ModelData2$CFR, xreg=xreg, order=c(0,1,0))
modArima

coeftest(modArima)
summary(modArima)
checkresiduals(modArima)


#####Rest of top 20

setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')


COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020
COVID <- subset(COVID, COVID$date <= "2021-06-30") #5/29/2021

COVID <- read.csv("World2.csv")
#Remove World and International information
ModelData<-COVID[!(COVID$ï..Location=="United Arab Emirates" | COVID$ï..Location=="Israel" |
                     COVID$ï..Location=="Bahrain" | COVID$ï..Location=="Chile" |
                     COVID$ï..Location=="Mongolia" | COVID$ï..Location=="United Kingdom" |
                     COVID$ï..Location=="Hungary" | COVID$ï..Location=="Qatar" |
                     COVID$ï..Location=="Uruguay" | COVID$ï..Location=="Singapore" |
                     COVID$ï..Location=="Canada" |
                     COVID$ï..Location=="Germany" | COVID$ï..Location=="Lithuania" |
                     COVID$ï..Location=="Belgium" | COVID$ï..Location=="Italy" |
                     COVID$ï..Location=="Spain" | COVID$ï..Location=="Austria" |
                     COVID$ï..Location=="Switzerland" | COVID$ï..Location=="Portugal" |
                     COVID$ï..Location=="France"),]

# COVID <- COVID[ which(COVID$population > 1000000), ]
# 
# #Creating CFR
# COVID$CFR <- (COVID$total_deaths/COVID$total_cases)
# 
# #Reading other data
# Day <- read.csv("Day.csv")
# 
# GHSI <- read.csv("GHSI.csv")
# GHSI[GHSI == 0] <- NA
# 
# WGI <- read.csv("WGI.csv")
# WGI[WGI == 0] <- NA
# 
# Obesity <- read.csv("Obesity.csv")
# 
# #Merge all data
# finaldt1 <- merge(Day, GHSI,  by="location")
# 
# finaldt2 <- merge(finaldt1, WGI,  by="location")
# 
# finaldt3 <- merge(finaldt2, Obesity,  by="location")
# 
# #Merge peak date data and others data
# COVID <- merge(COVID, finaldt3,  by="location")
# 
# options(scipen = 999)
# 
# library(lubridate)
# COVID$date2 <- as.Date(as.character(COVID$date),format="%Y-%m-%d")
# COVID$date2
# COVID$Week <- week(as.Date(as.character(COVID$date2),format="%Y-%m-%d"))
# COVID$Week
# 
# x <- nrow(COVID)
# COVID$Week2 <- COVID$Week
# for (i in 1:x) {
#   if (COVID$date[i] >= "2021-01-01")
#     COVID$Week2[i] = COVID$Week[i]+53
#   
# }
# print(COVID$Week2)
# summary(COVID$Week2)
# 
# 
# 
# world_CFR <- aggregate(COVID$CFR, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) 
# world_CFR
# 
# #world_CFR <- subset(world_CFR, world_CFR$Group.1 >= 79) #5/29/2021
# #world_CFR
# 
# colnames(world_CFR) <- c("Weeks", "Location", "CFR")
# world_CFR
# 
# world_total_cases_pm <- aggregate(COVID$total_cases_per_million, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_total_cases_pm 
# 
# #world_total_cases_pm <- subset(world_total_cases_pm, world_total_cases_pm$Group.1 >= 79) #5/29/2021
# #world_total_cases_pm
# 
# colnames(world_total_cases_pm) <- c("Weeks", "Location", "TotalCasepm")
# world_total_cases_pm
# 
# world_new_cases <- aggregate(COVID$new_cases, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_new_cases
# 
# colnames(world_new_cases) <- c("Weeks", "Location", "NewCases")
# world_new_cases
# 
# world_aged_65_older <- aggregate(COVID$aged_65_older, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_aged_65_older
# 
# #world_aged_65_older <- subset(world_aged_65_older, world_aged_65_older$Group.1 >= 79) #5/29/2021
# #world_aged_65_older
# 
# colnames(world_aged_65_older) <- c("Weeks", "Location", "Age65Older")
# world_aged_65_older
# 
# world_population_density <- aggregate(COVID$population_density, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_population_density
# 
# #world_population_density <- subset(world_population_density, world_population_density$Group.1 >= 79) #5/29/2021
# #world_population_density
# 
# colnames(world_population_density) <- c("Weeks", "Location", "PopulationDensity")
# world_population_density
# 
# world_total_tests_per_thousand <- aggregate(COVID$total_tests_per_thousand, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_total_tests_per_thousand
# 
# #world_total_tests_per_thousand <- subset(world_total_tests_per_thousand, world_total_tests_per_thousand$Group.1 >= 79) #5/29/2021
# #world_total_tests_per_thousand
# 
# colnames(world_total_tests_per_thousand) <- c("Weeks", "Location", "TotalTestpt")
# world_total_tests_per_thousand
# 
# world_GHSI <- aggregate(COVID$GHSI, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_GHSI
# 
# #world_GHSI <- subset(world_GHSI, world_GHSI$Group.1 >= 79) #5/29/2021
# #world_GHSI
# 
# colnames(world_GHSI) <- c("Weeks", "Location", "GHSI")
# world_GHSI
# 
# world_gdp_per_capita <- aggregate(COVID$gdp_per_capita, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_gdp_per_capita
# 
# #world_gdp_per_capita <- subset(world_gdp_per_capita, world_gdp_per_capita$Group.1 >= 79) #5/29/2021
# #world_gdp_per_capita
# 
# colnames(world_gdp_per_capita) <- c("Weeks", "Location", "GDP")
# world_gdp_per_capita
# 
# 
# 
# world_WGI <- aggregate(COVID$WGI, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_WGI
# 
# #world_WGI <- subset(world_WGI, world_WGI$Group.1 >= 79) #5/29/2021
# #world_WGI
# 
# colnames(world_WGI) <- c("Weeks", "Location", "WGI")
# world_WGI
# 
# world_Obesity_rate <- aggregate(COVID$Obesity_rate, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_Obesity_rate
# 
# #world_Obesity_rate <- subset(world_Obesity_rate, world_Obesity_rate$Group.1 >= 79) #5/29/2021
# #world_Obesity_rate
# 
# colnames(world_Obesity_rate) <- c("Weeks", "Location", "Obesity")
# world_Obesity_rate
# 
# world_Vac <- aggregate(COVID$total_vaccinations_per_hundred, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_Vac
# 
# #world_Vac <- subset(world_Vac, world_Vac$Group.1 >= 79) #5/29/2021
# #world_Vac
# 
# colnames(world_Vac) <- c("Weeks", "Location", "Vaccinationph")
# world_Vac
# 
# ModelData1 <- merge(world_CFR, world_total_cases_pm, by=c("Location", "Weeks"))
# 
# ModelData2 <- merge(ModelData1, world_new_cases, by=c("Location", "Weeks"))
# 
# ModelData3 <- merge(ModelData2, world_aged_65_older, by=c("Location", "Weeks"))
# 
# ModelData4 <- merge(ModelData3, world_population_density, by=c("Location", "Weeks"))
# 
# ModelData5 <- merge(ModelData4, world_total_tests_per_thousand, by=c("Location", "Weeks"))
# 
# ModelData6 <- merge(ModelData5, world_GHSI, by=c("Location", "Weeks"))
# 
# ModelData7 <- merge(ModelData6, world_gdp_per_capita, by=c("Location", "Weeks"))
# 
# ModelData8 <- merge(ModelData7, world_WGI, by=c("Location", "Weeks"))
# 
# ModelData9 <- merge(ModelData8, world_Obesity_rate, by=c("Location", "Weeks"))
# 
# ModelData <- merge(ModelData9, world_Vac, by=c("Location", "Weeks"))
# 
# edit(ModelData)
# write.csv(ModelData,"RestofTOP20.csv", row.names = FALSE)
# 
# 
# model <- betareg(ModelData$CFR ~  ModelData$Age65Older + ModelData$PopulationDensity + ModelData$TotalTestpt   + ModelData$GHSI + ModelData$GDP + ModelData$WGI + ModelData$Obesity + ModelData$Vaccinationph, 
#                  data = ModelData, link= "log")
# 
# vif(model)
# #Model summary
# summary(model)
# 
# #IRR
# round(exp(model$coefficients$mean),5)
# 
# #Confidence interval of IRR
# round(exp(confint(model)),5)
# 
# model <- lm(ModelData$TotalCasepm ~  ModelData$Age65Older + ModelData$PopulationDensity + ModelData$TotalTestpt   + ModelData$GHSI + ModelData$GDP + ModelData$WGI + ModelData$Obesity + ModelData$Vaccinationph, 
#             data = ModelData)
# 
# vif(model)
# #Model summary
# summary(model)
# 
# 
# model <- lm(ModelData$NewCases ~  ModelData$Age65Older + ModelData$PopulationDensity + ModelData$TotalTestpt   + ModelData$GHSI + ModelData$GDP + ModelData$WGI + ModelData$Obesity + ModelData$Vaccinationph, 
#             data = ModelData)
# 
# vif(model)
# #Model summary
# summary(model)

#ARIMAX

ModelData2 <- na.omit(ModelData)
z<- ModelData2$CFR
z
auto.arima(z)
c<-Arima(z, order=c(1,0,0)) 
checkresiduals(c)

Arima(z, order=c(1,0,0))$bic
Arima(z, order=c(0,1,0))$bic
Arima(z, order=c(1,0,1))$bic
Arima(z, order=c(1,1,0))$bic
Arima(z, order=c(1,1,1))$bic

Fit<-Arima(z, order=c(1,0,0))  ## some AR or MA terms coefficients are insignificant, so first of all select a model which all parameters are significant and pass residual test
library(lmtest)
summary(Fit)
### model diagnosis
checkresiduals(Fit) 

xreg <- cbind(ModelData2$Age65Older , ModelData2$GHSI , ModelData2$GDP 
              , ModelData2$WGI , ModelData2$Vaccinationph)
colnames(xreg) <- c("Age65Older", "GHSI", "GDP", "WGI","Vaccinationph")

modArima <- Arima(ModelData2$CFR, xreg=xreg, order=c(0,1,0))
modArima

coeftest(modArima)
summary(modArima)
checkresiduals(modArima)



#WORLD WITH Above 50% Vaccine
setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID <- read.csv("World2.csv")

# COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020
# COVID <- subset(COVID, COVID$date <= "2021-06-30") #5/29/2021

#Remove World and International information
# COVID<-COVID[!(COVID$iso_code=="OWID_AFR" | COVID$iso_code=="OWID_ASI" | COVID$iso_code=="OWID_EUR" | COVID$iso_code=="OWID_INT" | 
#                  COVID$iso_code=="OWID_KOS" | COVID$iso_code=="OWID_NAM" | COVID$iso_code=="OWID_CYN" | COVID$iso_code=="OWID_SAM" |
#                  COVID$iso_code=="OWID_WRL" | COVID$iso_code == "OWID_EUN" | COVID$iso_code == "OWID_OCE"),]
# 
# COVID <- COVID[ which(COVID$population > 1000000), ]
# ModelData <- COVID[ which(COVID$Vaccinationph  > 50), ]
# 
# #Creating CFR
# COVID$CFR <- (COVID$total_deaths/COVID$total_cases)
# 
# #Reading other data
# Day <- read.csv("Day.csv")
# 
# GHSI <- read.csv("GHSI.csv")
# GHSI[GHSI == 0] <- NA
# 
# WGI <- read.csv("WGI.csv")
# WGI[WGI == 0] <- NA
# 
# Obesity <- read.csv("Obesity.csv")
# 
# #Merge all data
# finaldt1 <- merge(Day, GHSI,  by="location")
# 
# finaldt2 <- merge(finaldt1, WGI,  by="location")
# 
# finaldt3 <- merge(finaldt2, Obesity,  by="location")
# 
# #Merge peak date data and others data
# COVID <- merge(COVID, finaldt3,  by="location")
# 
# options(scipen = 999)
# 
# library(lubridate)
# COVID$date2 <- as.Date(as.character(COVID$date),format="%Y-%m-%d")
# COVID$date2
# COVID$Week <- week(as.Date(as.character(COVID$date2),format="%Y-%m-%d"))
# COVID$Week
# 
# x <- nrow(COVID)
# COVID$Week2 <- COVID$Week
# for (i in 1:x) {
#   if (COVID$date[i] >= "2021-01-01")
#     COVID$Week2[i] = COVID$Week[i]+53
#   
# }
# print(COVID$Week2)
# summary(COVID$Week2)
# 
# 
# 
# world_CFR <- aggregate(COVID$CFR, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_CFR
# 
# #world_CFR <- subset(world_CFR, world_CFR$Group.1 >= 79) #5/29/2021
# #world_CFR
# 
# colnames(world_CFR) <- c("Weeks", "Location", "CFR")
# world_CFR
# 
# world_total_cases_pm <- aggregate(COVID$total_cases_per_million, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_total_cases_pm 
# 
# #world_total_cases_pm <- subset(world_total_cases_pm, world_total_cases_pm$Group.1 >= 79) #5/29/2021
# #world_total_cases_pm
# 
# colnames(world_total_cases_pm) <- c("Weeks", "Location", "TotalCasepm")
# world_total_cases_pm
# 
# world_new_cases <- aggregate(COVID$new_cases, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_new_cases
# 
# colnames(world_new_cases) <- c("Weeks", "Location", "NewCases")
# world_new_cases
# 
# world_aged_65_older <- aggregate(COVID$aged_65_older, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_aged_65_older
# 
# #world_aged_65_older <- subset(world_aged_65_older, world_aged_65_older$Group.1 >= 79) #5/29/2021
# #world_aged_65_older
# 
# colnames(world_aged_65_older) <- c("Weeks", "Location", "Age65Older")
# world_aged_65_older
# 
# world_population_density <- aggregate(COVID$population_density, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_population_density
# 
# #world_population_density <- subset(world_population_density, world_population_density$Group.1 >= 79) #5/29/2021
# #world_population_density
# 
# colnames(world_population_density) <- c("Weeks", "Location", "PopulationDensity")
# world_population_density
# 
# world_total_tests_per_thousand <- aggregate(COVID$total_tests_per_thousand, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_total_tests_per_thousand
# 
# #world_total_tests_per_thousand <- subset(world_total_tests_per_thousand, world_total_tests_per_thousand$Group.1 >= 75) #5/29/2021
# #world_total_tests_per_thousand
# 
# colnames(world_total_tests_per_thousand) <- c("Weeks", "Location", "TotalTestpt")
# world_total_tests_per_thousand
# 
# world_GHSI <- aggregate(COVID$GHSI, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_GHSI
# 
# #world_GHSI <- subset(world_GHSI, world_GHSI$Group.1 >= 79) #5/29/2021
# #world_GHSI
# 
# colnames(world_GHSI) <- c("Weeks", "Location", "GHSI")
# world_GHSI
# 
# world_gdp_per_capita <- aggregate(COVID$gdp_per_capita, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_gdp_per_capita
# 
# #world_gdp_per_capita <- subset(world_gdp_per_capita, world_gdp_per_capita$Group.1 >= 79) #5/29/2021
# #world_gdp_per_capita
# 
# colnames(world_gdp_per_capita) <- c("Weeks", "Location", "GDP")
# world_gdp_per_capita
# 
# 
# 
# world_WGI <- aggregate(COVID$WGI, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_WGI
# 
# #world_WGI <- subset(world_WGI, world_WGI$Group.1 >= 79) #5/29/2021
# #world_WGI
# 
# colnames(world_WGI) <- c("Weeks", "Location", "WGI")
# world_WGI
# 
# world_Obesity_rate <- aggregate(COVID$Obesity_rate, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_Obesity_rate
# 
# #world_Obesity_rate <- subset(world_Obesity_rate, world_Obesity_rate$Group.1 >= 79) #5/29/2021
# #world_Obesity_rate
# 
# colnames(world_Obesity_rate) <- c("Weeks", "Location", "Obesity")
# world_Obesity_rate
# 
# world_Vac <- aggregate(COVID$total_vaccinations_per_hundred, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_Vac
# 
# #world_Vac <- subset(world_Vac, world_Vac$Group.1 >= 79) #5/29/2021
# #world_Vac
# 
# colnames(world_Vac) <- c("Weeks", "Location", "Vaccinationph")
# world_Vac
# 
# world_week <- aggregate(COVID$CovidWeek, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_week
# 
# #world_week <- subset(world_week, world_week$Group.1 >= 79) #5/29/2021
# #world_week
# 
# colnames(world_week) <- c("Weeks", "Location", "covid_week")
# world_week
# 
# ModelData1 <- merge(world_CFR, world_total_cases_pm, by=c("Location", "Weeks"))
# 
# ModelData2 <- merge(ModelData1, world_new_cases, by=c("Location", "Weeks"))
# 
# ModelData3 <- merge(ModelData2, world_aged_65_older, by=c("Location", "Weeks"))
# 
# ModelData4 <- merge(ModelData3, world_population_density, by=c("Location", "Weeks"))
# 
# ModelData5 <- merge(ModelData4, world_total_tests_per_thousand, by=c("Location", "Weeks"))
# 
# ModelData6 <- merge(ModelData5, world_GHSI, by=c("Location", "Weeks"))
# 
# ModelData7 <- merge(ModelData6, world_gdp_per_capita, by=c("Location", "Weeks"))
# 
# ModelData8 <- merge(ModelData7, world_WGI, by=c("Location", "Weeks"))
# 
# ModelData9 <- merge(ModelData8, world_Obesity_rate, by=c("Location", "Weeks"))
# 
# ModelData10 <- merge(ModelData9, world_Vac, by=c("Location", "Weeks"))
# 
# ModelData <- merge(ModelData10, world_week, by=c("Location", "Weeks"))
# 
# 
# 
# edit(ModelData)
# write.csv(ModelData,"Above50.csv", row.names = FALSE)
# 
# 
# model <- betareg(ModelData$CFR ~  ModelData$Age65Older + ModelData$PopulationDensity + ModelData$TotalTestpt   + ModelData$GHSI + ModelData$GDP + ModelData$WGI + ModelData$Obesity + ModelData$Vaccinationph, 
#                  data = ModelData, offset(ModelData$covid_week), link= "log")
# 
# vif(model)
# #Model summary
# summary(model)
# 
# #IRR
# round(exp(model$coefficients$mean),5)
# 
# #Confidence interval of IRR
# round(exp(confint(model)),5)
# 
# model <- lm(ModelData$TotalCasepm ~  ModelData$Age65Older + ModelData$PopulationDensity + ModelData$TotalTestpt   + ModelData$GHSI + ModelData$GDP + ModelData$WGI + ModelData$Obesity + ModelData$Vaccinationph, 
#             data = ModelData)
# 
# vif(model)
# #Model summary
# summary(model)
# 
# 
# model <- lm(ModelData$NewCases ~  ModelData$Age65Older + ModelData$PopulationDensity + ModelData$TotalTestpt   + ModelData$GHSI + ModelData$GDP + ModelData$WGI + ModelData$Obesity + ModelData$Vaccinationph, 
#             data = ModelData)
# 
# vif(model)
# #Model summary
# summary(model)
# 
# model <- lm(ModelData$NewCases ~  ModelData$Age65Older + ModelData$PopulationDensity + ModelData$TotalTestpt   + ModelData$GHSI + ModelData$GDP + ModelData$WGI + ModelData$Obesity + ModelData$Vaccinationph, 
#             data = ModelData)
# 
# vif(model)
# #Model summary
# summary(model)



#ARIMAX

ModelData2 <- na.omit(ModelData)
z<- ModelData2$CFR
z
auto.arima(z)
c<-Arima(z, order=c(1,0,0)) 
checkresiduals(c)

Arima(z, order=c(1,0,1))$bic
Arima(z, order=c(0,1,0))$bic
Arima(z, order=c(1,0,0))$bic
Arima(z, order=c(1,1,0))$bic
Arima(z, order=c(1,1,1))$bic

Fit<-Arima(z, order=c(1,0,0))  ## some AR or MA terms coefficients are insignificant, so first of all select a model which all parameters are significant and pass residual test
library(lmtest)
summary(Fit)
### model diagnosis
checkresiduals(Fit) 

xreg <- cbind(ModelData2$Age65Older , ModelData2$GHSI , ModelData2$GDP 
              , ModelData2$WGI , ModelData2$Vaccinationph)
colnames(xreg) <- c("Age65Older", "GHSI", "GDP", "WGI","Vaccinationph")

modArima <- Arima(ModelData2$CFR, xreg=xreg, order=c(0,1,0))
modArima

coeftest(modArima)
summary(modArima)
checkresiduals(modArima)



#WORLD WITH Below 50% Vaccine
setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID <- read.csv("World2.csv")

# COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020
# COVID <- subset(COVID, COVID$date <= "2021-06-30") #5/29/2021
# 
# #Remove World and International information
# COVID<-COVID[!(COVID$iso_code=="OWID_AFR" | COVID$iso_code=="OWID_ASI" | COVID$iso_code=="OWID_EUR" | COVID$iso_code=="OWID_INT" | 
#                  COVID$iso_code=="OWID_KOS" | COVID$iso_code=="OWID_NAM" | COVID$iso_code=="OWID_CYN" | COVID$iso_code=="OWID_SAM" |
#                  COVID$iso_code=="OWID_WRL" | COVID$iso_code == "OWID_EUN" | COVID$iso_code == "OWID_OCE"),]
# 
# COVID <- COVID[ which(COVID$population > 1000000), ]
# ModelData <- COVID[ which(COVID$Vaccinationph < 50), ]
# 
# #Creating CFR
# COVID$CFR <- (COVID$total_deaths/COVID$total_cases)
# 
# #Reading other data
# Day <- read.csv("Day.csv")
# 
# GHSI <- read.csv("GHSI.csv")
# GHSI[GHSI == 0] <- NA
# 
# WGI <- read.csv("WGI.csv")
# WGI[WGI == 0] <- NA
# 
# Obesity <- read.csv("Obesity.csv")
# 
# #Merge all data
# finaldt1 <- merge(Day, GHSI,  by="location")
# 
# finaldt2 <- merge(finaldt1, WGI,  by="location")
# 
# finaldt3 <- merge(finaldt2, Obesity,  by="location")
# 
# #Merge peak date data and others data
# COVID <- merge(COVID, finaldt3,  by="location")
# 
# options(scipen = 999)
# 
# library(lubridate)
# COVID$date2 <- as.Date(as.character(COVID$date),format="%Y-%m-%d")
# COVID$date2
# COVID$Week <- week(as.Date(as.character(COVID$date2),format="%Y-%m-%d"))
# COVID$Week
# 
# x <- nrow(COVID)
# COVID$Week2 <- COVID$Week
# for (i in 1:x) {
#   if (COVID$date[i] >= "2021-01-01")
#     COVID$Week2[i] = COVID$Week[i]+53
#   
# }
# print(COVID$Week2)
# summary(COVID$Week2)
# 
# 
# 
# world_CFR <- aggregate(COVID$CFR, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_CFR
# 
# #world_CFR <- subset(world_CFR, world_CFR$Group.1 >= 79) #5/29/2021
# #world_CFR
# 
# colnames(world_CFR) <- c("Weeks", "Location", "CFR")
# world_CFR
# 
# world_total_cases_pm <- aggregate(COVID$total_cases_per_million, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_total_cases_pm 
# 
# #world_total_cases_pm <- subset(world_total_cases_pm, world_total_cases_pm$Group.1 >= 79) #5/29/2021
# #world_total_cases_pm
# 
# colnames(world_total_cases_pm) <- c("Weeks", "Location", "TotalCasepm")
# world_total_cases_pm
# 
# world_new_cases <- aggregate(COVID$new_cases, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_new_cases
# 
# colnames(world_new_cases) <- c("Weeks", "Location", "NewCases")
# world_new_cases
# 
# world_aged_65_older <- aggregate(COVID$aged_65_older, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_aged_65_older
# 
# #world_aged_65_older <- subset(world_aged_65_older, world_aged_65_older$Group.1 >= 79) #5/29/2021
# #world_aged_65_older
# 
# colnames(world_aged_65_older) <- c("Weeks", "Location", "Age65Older")
# world_aged_65_older
# 
# world_population_density <- aggregate(COVID$population_density, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_population_density
# 
# #world_population_density <- subset(world_population_density, world_population_density$Group.1 >= 79) #5/29/2021
# #world_population_density
# 
# colnames(world_population_density) <- c("Weeks", "Location", "PopulationDensity")
# world_population_density
# 
# world_total_tests_per_thousand <- aggregate(COVID$total_tests_per_thousand, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_total_tests_per_thousand
# 
# #world_total_tests_per_thousand <- subset(world_total_tests_per_thousand, world_total_tests_per_thousand$Group.1 >= 75) #5/29/2021
# #world_total_tests_per_thousand
# 
# colnames(world_total_tests_per_thousand) <- c("Weeks", "Location", "TotalTestpt")
# world_total_tests_per_thousand
# 
# world_GHSI <- aggregate(COVID$GHSI, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_GHSI
# 
# #world_GHSI <- subset(world_GHSI, world_GHSI$Group.1 >= 79) #5/29/2021
# #world_GHSI
# 
# colnames(world_GHSI) <- c("Weeks", "Location", "GHSI")
# world_GHSI
# 
# world_gdp_per_capita <- aggregate(COVID$gdp_per_capita, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_gdp_per_capita
# 
# #world_gdp_per_capita <- subset(world_gdp_per_capita, world_gdp_per_capita$Group.1 >= 79) #5/29/2021
# #world_gdp_per_capita
# 
# colnames(world_gdp_per_capita) <- c("Weeks", "Location", "GDP")
# world_gdp_per_capita
# 
# 
# 
# world_WGI <- aggregate(COVID$WGI, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_WGI
# 
# #world_WGI <- subset(world_WGI, world_WGI$Group.1 >= 79) #5/29/2021
# #world_WGI
# 
# colnames(world_WGI) <- c("Weeks", "Location", "WGI")
# world_WGI
# 
# world_Obesity_rate <- aggregate(COVID$Obesity_rate, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_Obesity_rate
# 
# #world_Obesity_rate <- subset(world_Obesity_rate, world_Obesity_rate$Group.1 >= 79) #5/29/2021
# #world_Obesity_rate
# 
# colnames(world_Obesity_rate) <- c("Weeks", "Location", "Obesity")
# world_Obesity_rate
# 
# world_Vac <- aggregate(COVID$total_vaccinations_per_hundred, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_Vac
# 
# #world_Vac <- subset(world_Vac, world_Vac$Group.1 >= 79) #5/29/2021
# #world_Vac
# 
# colnames(world_Vac) <- c("Weeks", "Location", "Vaccinationph")
# world_Vac
# 
# world_week <- aggregate(COVID$CovidWeek, by= list(COVID$Week2, COVID$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_week
# 
# #world_week <- subset(world_week, world_week$Group.1 >= 79) #5/29/2021
# #world_week
# 
# colnames(world_week) <- c("Weeks", "Location", "covid_week")
# world_week
# 
# ModelData1 <- merge(world_CFR, world_total_cases_pm, by=c("Location", "Weeks"))
# 
# ModelData2 <- merge(ModelData1, world_new_cases, by=c("Location", "Weeks"))
# 
# ModelData3 <- merge(ModelData2, world_aged_65_older, by=c("Location", "Weeks"))
# 
# ModelData4 <- merge(ModelData3, world_population_density, by=c("Location", "Weeks"))
# 
# ModelData5 <- merge(ModelData4, world_total_tests_per_thousand, by=c("Location", "Weeks"))
# 
# ModelData6 <- merge(ModelData5, world_GHSI, by=c("Location", "Weeks"))
# 
# ModelData7 <- merge(ModelData6, world_gdp_per_capita, by=c("Location", "Weeks"))
# 
# ModelData8 <- merge(ModelData7, world_WGI, by=c("Location", "Weeks"))
# 
# ModelData9 <- merge(ModelData8, world_Obesity_rate, by=c("Location", "Weeks"))
# 
# ModelData10 <- merge(ModelData9, world_Vac, by=c("Location", "Weeks"))
# 
# ModelData <- merge(ModelData10, world_week, by=c("Location", "Weeks"))
# 
# 
# 
# edit(ModelData)
# write.csv(ModelData,"Below50.csv", row.names = FALSE)
# 
# 
# model <- betareg(ModelData$CFR ~  ModelData$Age65Older + ModelData$PopulationDensity + ModelData$TotalTestpt   + ModelData$GHSI + ModelData$GDP + ModelData$WGI + ModelData$Obesity + ModelData$Vaccinationph, 
#                  data = ModelData, offset(ModelData$covid_week), link= "log")
# 
# vif(model)
# #Model summary
# summary(model)
# 
# #IRR
# round(exp(model$coefficients$mean),5)
# 
# #Confidence interval of IRR
# round(exp(confint(model)),5)
# 
# model <- lm(ModelData$TotalCasepm ~  ModelData$Age65Older + ModelData$PopulationDensity + ModelData$TotalTestpt   + ModelData$GHSI + ModelData$GDP + ModelData$WGI + ModelData$Obesity + ModelData$Vaccinationph, 
#             data = ModelData)
# 
# vif(model)
# #Model summary
# summary(model)
# 
# 
# model <- lm(ModelData$NewCases ~  ModelData$Age65Older + ModelData$PopulationDensity + ModelData$TotalTestpt   + ModelData$GHSI + ModelData$GDP + ModelData$WGI + ModelData$Obesity + ModelData$Vaccinationph, 
#             data = ModelData)
# 
# vif(model)
# #Model summary
# summary(model)
# 
# model <- lm(ModelData$NewCases ~  ModelData$Age65Older + ModelData$PopulationDensity + ModelData$TotalTestpt   + ModelData$GHSI + ModelData$GDP + ModelData$WGI + ModelData$Obesity + ModelData$Vaccinationph, 
#             data = ModelData)
# 
# vif(model)
# #Model summary
# summary(model)


#ARIMAX

ModelData2 <- na.omit(ModelData)
z<- ModelData2$CFR
z
auto.arima(z)
c<-Arima(z, order=c(1,0,0)) 
checkresiduals(c)

Arima(z, order=c(1,0,0))$bic
Arima(z, order=c(0,1,0))$bic
Arima(z, order=c(1,0,1))$bic
Arima(z, order=c(1,1,0))$bic
Arima(z, order=c(1,1,1))$bic

Fit<-Arima(z, order=c(1,0,0))  ## some AR or MA terms coefficients are insignificant, so first of all select a model which all parameters are significant and pass residual test
library(lmtest)
summary(Fit)
### model diagnosis
checkresiduals(Fit) 

xreg <- cbind(ModelData2$Age65Older , ModelData2$GHSI , ModelData2$GDP 
              , ModelData2$WGI , ModelData2$Vaccinationph)
colnames(xreg) <- c("Age65Older", "GHSI", "GDP", "WGI","Vaccinationph")

modArima <- Arima(ModelData2$CFR, xreg=xreg, order=c(0,1,0))
modArima

coeftest(modArima)
summary(modArima)
checkresiduals(modArima)


