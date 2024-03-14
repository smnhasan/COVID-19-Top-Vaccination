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



##Descriptive
##WORLD SUMMARY

#2022
setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID2022 <- read.csv("owid-covid-data.csv")

COVID2022 <- subset(COVID2022, COVID2022$date == "1/5/2022") #5/29/2021 2/24/2020

#Remove World and International information
COVID2022<-COVID2022[!(COVID2022$iso_code=="OWID_AFR" | COVID2022$iso_code=="OWID_ASI" | COVID2022$iso_code=="OWID_EUR" | COVID2022$iso_code=="OWID_INT" | 
                         COVID2022$iso_code=="OWID_KOS" | COVID2022$iso_code=="OWID_NAM" | COVID2022$iso_code=="OWID_CYN" | COVID2022$iso_code=="OWID_SAM" |
                         COVID2022$iso_code=="OWID_WRL" | COVID2022$iso_code == "OWID_EUN" | COVID2022$iso_code == "OWID_OCE"),]

COVID2022 <- COVID2022[ which(COVID2022$population >= 1000000), ]

#Creating CFR
COVID2022$CFR <- (COVID2022$total_deaths/COVID2022$total_cases)*100
COVID2022$CFR2022 <- (COVID2022$total_deaths/COVID2022$total_cases)*100
t.test(COVID2022$CFR)
t.test(COVID2022$total_vaccinations_per_hundred)

#2021
setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID <- read.csv("owid-covid-data.csv")

COVID <- subset(COVID, COVID$date == "1/5/2021") #5/29/2021

#Remove World and International information
COVID<-COVID[!(COVID$iso_code=="OWID_AFR" | COVID$iso_code=="OWID_ASI" | COVID$iso_code=="OWID_EUR" | COVID$iso_code=="OWID_INT" | 
                 COVID$iso_code=="OWID_KOS" | COVID$iso_code=="OWID_NAM" | COVID$iso_code=="OWID_CYN" | COVID$iso_code=="OWID_SAM" |
                 COVID$iso_code=="OWID_WRL" | COVID$iso_code == "OWID_EUN" | COVID$iso_code == "OWID_OCE"),]

COVID <- COVID[ which(COVID$population >= 1000000), ]

#Creating CFR
COVID$CFR <- (COVID$total_deaths/COVID$total_cases)*100
COVID$CFR2021 <- (COVID$total_deaths/COVID$total_cases)*100
t.test(COVID$CFR)

t.test(COVID$CFR2021, COVID2022$CFR2022, paired = TRUE, alternative = "two.sided")


##TOP20 SUMMARY
#2022
setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID <- read.csv("owid-covid-data.csv")


COVID <- subset(COVID, COVID$date == "1/5/2022") #5/29/2021
COVID <- COVID[ which(COVID$population >= 1000000), ]

#Creating CFR
COVID$CFR <- (COVID$total_deaths/COVID$total_cases)*100
data <- COVID[with(COVID,order(-CFR)),]
data <- COVID[with(COVID,order(-total_vaccinations_per_hundred)),]
data$location

COVID<-COVID[(COVID$location=="Cuba" | COVID$location=="Chile" |
                COVID$location=="United Arab Emirates" | COVID$location=="Singapore" |
                COVID$location=="Denmark" | COVID$location=="South Korea" |
                COVID$location=="Uruguay" | COVID$location=="China" |
                COVID$location=="Ireland" | COVID$location=="United Kingdom" |
                COVID$location=="Portugal" |  COVID$location=="Belgium" | 
                COVID$location=="Italy" | COVID$location=="France" | 
                COVID$location=="Bahrain" | COVID$location=="Austria" | 
                COVID$location=="Canada" | COVID$location=="Israel" | 
                COVID$location=="Norway" | COVID$location=="Germany"),]

t.test(COVID$CFR)
t.test(COVID$total_vaccinations_per_hundred)

#2021
##TOP20 SUMMARY
setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID2021 <- read.csv("owid-covid-data.csv")

COVID2021 <- subset(COVID2021, COVID2021$date == "1/5/2021") #5/29/2021
COVID2021 <- COVID2021[ which(COVID2021$population >= 1000000), ]

#Creating CFR
COVID2021$CFR <- (COVID2021$total_deaths/COVID2021$total_cases)*100
data <- COVID2021[with(COVID2021,order(-CFR)),]
data <- COVID2021[with(COVID2021,order(-total_vaccinations_per_hundred)),]

COVID2021<-COVID2021[(COVID2021$location=="Cuba" | COVID2021$location=="Chile" |
                        COVID2021$location=="United Arab Emirates" | COVID2021$location=="Singapore" |
                        COVID2021$location=="Denmark" | COVID2021$location=="South Korea" |
                        COVID2021$location=="Uruguay" | COVID2021$location=="China" |
                        COVID2021$location=="Ireland" | COVID2021$location=="United Kingdom" |
                        COVID2021$location=="Portugal" |  COVID2021$location=="Belgium" | 
                        COVID2021$location=="Italy" | COVID2021$location=="France" | 
                        COVID2021$location=="Bahrain" | COVID2021$location=="Austria" | 
                        COVID2021$location=="Canada" | COVID2021$location=="Israel" | 
                        COVID2021$location=="Norway" | COVID2021$location=="Germany"),]

t.test(COVID2021$CFR)
t.test(COVID2021$total_vaccinations_per_hundred)
t.test(COVID2021$CFR, COVID$CFR, paired = TRUE, alternative = "two.sided")

#2022
##Rest World SUMMARY
setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID <- read.csv("owid-covid-data.csv")


COVID <- subset(COVID, COVID$date == "1/5/2022") #5/29/2021
COVID <- COVID[ which(COVID$population >= 1000000), ]

#Remove World and International information
COVID<-COVID[!(COVID$iso_code=="OWID_AFR" | COVID$iso_code=="OWID_ASI" | COVID$iso_code=="OWID_EUR" | COVID$iso_code=="OWID_INT" | 
                 COVID$iso_code=="OWID_KOS" | COVID$iso_code=="OWID_NAM" | COVID$iso_code=="OWID_CYN" | COVID$iso_code=="OWID_SAM" |
                 COVID$iso_code=="OWID_WRL" | COVID$iso_code == "OWID_EUN" | COVID$iso_code == "OWID_OCE"),]


COVID<-COVID[!(COVID$location=="Cuba" | COVID$location=="Chile" |
                COVID$location=="United Arab Emirates" | COVID$location=="Singapore" |
                COVID$location=="Denmark" | COVID$location=="South Korea" |
                COVID$location=="Uruguay" | COVID$location=="China" |
                COVID$location=="Ireland" | COVID$location=="United Kingdom" |
                COVID$location=="Portugal" |  COVID$location=="Belgium" | 
                COVID$location=="Italy" | COVID$location=="France" | 
                COVID$location=="Bahrain" | COVID$location=="Austria" | 
                COVID$location=="Canada" | COVID$location=="Israel" | 
                COVID$location=="Norway" | COVID$location=="Germany"),]


#Creating CFR
COVID$CFR <- (COVID$total_deaths/COVID$total_cases)*100
t.test(COVID$CFR)
t.test(COVID$total_vaccinations_per_hundred)

#2021
##Rest World SUMMARY
setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID2021 <- read.csv("owid-covid-data.csv")

COVID2021 <- subset(COVID2021, COVID2021$date == "1/5/2021") #5/29/2021
COVID2021 <- COVID2021[ which(COVID2021$population >= 1000000), ]

#Remove World and International information
COVID2021<-COVID2021[!(COVID2021$iso_code=="OWID_AFR" | COVID2021$iso_code=="OWID_ASI" | COVID2021$iso_code=="OWID_EUR" | COVID2021$iso_code=="OWID_INT" | 
                         COVID2021$iso_code=="OWID_KOS" | COVID2021$iso_code=="OWID_NAM" | COVID2021$iso_code=="OWID_CYN" | COVID2021$iso_code=="OWID_SAM" |
                         COVID2021$iso_code=="OWID_WRL" | COVID2021$iso_code == "OWID_EUN" | COVID2021$iso_code == "OWID_OCE"),]


COVID2021<-COVID2021[!(COVID2021$location=="Cuba" | COVID2021$location=="Chile" |
                        COVID2021$location=="United Arab Emirates" | COVID2021$location=="Singapore" |
                        COVID2021$location=="Denmark" | COVID2021$location=="South Korea" |
                        COVID2021$location=="Uruguay" | COVID2021$location=="China" |
                        COVID2021$location=="Ireland" | COVID2021$location=="United Kingdom" |
                        COVID2021$location=="Portugal" |  COVID2021$location=="Belgium" | 
                        COVID2021$location=="Italy" | COVID2021$location=="France" | 
                        COVID2021$location=="Bahrain" | COVID2021$location=="Austria" | 
                        COVID2021$location=="Canada" | COVID2021$location=="Israel" | 
                        COVID2021$location=="Norway" | COVID2021$location=="Germany"),]

#Creating CFR
COVID2021$CFR <- (COVID2021$total_deaths/COVID2021$total_cases)*100
 
t.test(COVID2021$CFR)

t.test(COVID2021$CFR, COVID$CFR, paired = TRUE, alternative = "two.sided")


#2022
#SSA
##Descriptive
setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID <- read.csv("owid-covid-data.csv")

COVID <- subset(COVID, COVID$date == "1/5/2022") #5/29/2021
#Remove World and International information
COVID<-COVID[(COVID$location=="Angola" | COVID$location=="Benin" | COVID$location=="Botswana" | COVID$location=="Burkina Faso" | 
                COVID$location=="Burundi" | COVID$location=="Cameroon" | COVID$location=="Central African Republic" | COVID$location=="Chad" |
                COVID$location=="Democratic Republic of Congo" | COVID$location == "Congo" | COVID$location == "Cote d'Ivoire" | COVID$location=="Djibouti" | 
                COVID$location=="Equatorial Guinea" | COVID$location=="Eritrea" | COVID$location=="Ethiopia" | 
                COVID$location=="Gabon" | COVID$location=="Gambia" | COVID$location=="Ghana" | COVID$location=="Guinea" |
                COVID$location=="Guinea-Bissau" | COVID$location == "Kenya" | COVID$location == "Lesotho" | COVID$location=="Liberia" | 
                COVID$location=="Madagascar" | COVID$location=="Malawi" | COVID$location=="Mali" | 
                COVID$location=="Mauritania" | COVID$location=="Mauritius" | COVID$location=="Mozambique" | COVID$location=="Namibia" |
                COVID$location=="Niger" | COVID$location == "Nigeria" | COVID$location == "Rwanda" | COVID$location=="Senegal" | 
                COVID$location=="Sierra Leone" | COVID$location=="Somalia" | COVID$location=="South Africa" | 
                COVID$location=="South Sudan" | COVID$location=="Sudan" | COVID$location=="Tanzania" | COVID$location=="Togo" |
                COVID$location=="Uganda" | COVID$location == "Zambia" | COVID$location == "Zimbabwe"),]

COVID <- COVID[ which(COVID$population >= 1000000), ]

#write.csv(COVID,"COVID_SSA.csv")
#COVID <- read.csv("COVID_SSAU.csv")

#Creating CFR
COVID$CFR <- (COVID$total_deaths/COVID$total_cases)*100

t.test(COVID$CFR)
t.test(COVID$total_vaccinations_per_hundred)

#2021
setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID2021 <- read.csv("owid-covid-data.csv")

COVID2021 <- subset(COVID2021, COVID2021$date == "1/5/2021") #5/29/2021
#Remove World and International information
COVID2021<-COVID2021[(COVID2021$location=="Angola" | COVID2021$location=="Benin" | COVID2021$location=="Botswana" | COVID2021$location=="Burkina Faso" | 
                        COVID2021$location=="Burundi" | COVID2021$location=="Cameroon" | COVID2021$location=="Central African Republic" | COVID2021$location=="Chad" |
                        COVID2021$location=="Democratic Republic of Congo" | COVID2021$location == "Congo" | COVID2021$location == "Cote d'Ivoire" | COVID2021$location=="Djibouti" | 
                        COVID2021$location=="Equatorial Guinea" | COVID2021$location=="Eritrea" | COVID2021$location=="Ethiopia" | 
                        COVID2021$location=="Gabon" | COVID2021$location=="Gambia" | COVID2021$location=="Ghana" | COVID2021$location=="Guinea" |
                        COVID2021$location=="Guinea-Bissau" | COVID2021$location == "Kenya" | COVID2021$location == "Lesotho" | COVID2021$location=="Liberia" | 
                        COVID2021$location=="Madagascar" | COVID2021$location=="Malawi" | COVID2021$location=="Mali" | 
                        COVID2021$location=="Mauritania" | COVID2021$location=="Mauritius" | COVID2021$location=="Mozambique" | COVID2021$location=="Namibia" |
                        COVID2021$location=="Niger" | COVID2021$location == "Nigeria" | COVID2021$location == "Rwanda" | COVID2021$location=="Senegal" | 
                        COVID2021$location=="Sierra Leone" | COVID2021$location=="Somalia" | COVID2021$location=="South Africa" | 
                        COVID2021$location=="South Sudan" | COVID2021$location=="Sudan" | COVID2021$location=="Tanzania" | COVID2021$location=="Togo" |
                        COVID2021$location=="Uganda" | COVID2021$location == "Zambia" | COVID2021$location == "Zimbabwe"),]

COVID2021 <- COVID2021[ which(COVID2021$population >= 1000000), ]
#Creating CFR
COVID2021$CFR <- (COVID2021$total_deaths/COVID2021$total_cases)*100
t.test(COVID2021$CFR)
t.test(COVID2021$CFR, COVID$CFR, paired = TRUE, alternative = "two.sided")



#GLMM Excess mortality
#Data Management
setwd('F:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID <- read.csv("excess-deaths.csv")

COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020 2020-08-08
COVID <- subset(COVID, COVID$date <= "2022-04-10") #5/29/2021

COVID2 <- read.csv("owid-covid-data2.csv")

COVID <- merge(COVID, COVID2, by=c("location", "date"))


#Remove World and International information
COVID<-COVID[!(COVID$iso_code=="OWID_AFR" | COVID$iso_code=="OWID_ASI" | COVID$iso_code=="OWID_EUR" | COVID$iso_code=="OWID_INT" | 
                 COVID$iso_code=="OWID_KOS" | COVID$iso_code=="OWID_NAM" | COVID$iso_code=="OWID_CYN" | COVID$iso_code=="OWID_SAM" |
                 COVID$iso_code=="OWID_WRL" | COVID$iso_code == "OWID_EUN" | COVID$iso_code == "OWID_OCE"),]
COVID <- COVID[ which(COVID$population >= 1000000), ]
#Creating CFR
COVID$CFR <- (COVID$estimated_daily_excess_deaths/COVID$new_cases)


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

for (i in 1:x) {
  if (COVID$date[i] >= "2022-01-01")
    COVID$Week2[i] = COVID$Week[i]+106
  
}

print(COVID$Week2)
summary(COVID$Week2)


#Aggregate by weekly

world_em <- aggregate(COVID$estimated_daily_excess_deaths, by= list(COVID$Week2, COVID$location), FUN=sum, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_em

colnames(world_em) <- c("Weeks", "location", "EM")
world_em

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
ModelData1 <- merge(world_em, world_total_cases_pm, by=c("location", "Weeks"))

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

write.csv(ModelData2,"ModelDataem.csv")

#Replace missing with previous observation

World <- read.csv("WorldData121.csv")

World <- merge(ModelData2, World, by=c("location", "Weeks"))
#GLMM Model

library(glmmTMB)
World$Vaccinationph2.y
World$vml <- World$Vaccinationph2.y
World$vml2 <- scale(World$Vaccinationph2.y)

World$lw <- log(World$Weeks)

World$TTs <- World$TotalTestpt.y
World$TTs2 <- scale(World$TTs)

World$GDP2 <- scale(World$GDP.y)

World$PopulationDensity2 <- World$PopulationDensity.y
World$PopulationDensity22 <- scale(World$PopulationDensity2)

World$GHSI2 <- scale(World$GHSI.y)

World$WGI2 <- scale(World$WGI.y)

World$Age65Older2 <- World$Age65Older.y
World$Age65Older22 <- scale(World$Age65Older2)

World$Obesity_rate2 <- World$Obesity_rate.y
World$Obesity_rate22 <- scale(World$Obesity_rate2)

World$EM2 <- scale(World$EM)

World$SI2 <- scale(World$SI.y)

World$location <- as.factor(World$location)
nrow(World)

library(glmmTMB)
library(DHARMa)
library(performance)

summary(World$EM)


World$EM <- (World$EM-min(World$EM))/(max(World$EM)-min(World$EM))
World$EM
World$EM[World$EM == 0] <- NA
World$EM[World$EM == 1] <- NA
fit <- glmmTMB(EM~ vml2*lw +Age65Older22 + PopulationDensity22 
               + TTs2 + GDP2 + GHSI2 
               + WGI2 + Obesity_rate22 + SI2 + (1|location) + (1| lw), na.action=na.omit, family = beta_family(link = "logit"), data = World)
library(car)
summary(fit)
round(exp(confint(fit)),3)
options(scipen = 999) 
performance::performance(fit)

#Time series
tiff("CFR.tiff", units="in", width=6, height=5, res=300)


setwd('F:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID <- read.csv("excess-deaths.csv")

COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020 2020-08-08
COVID <- subset(COVID, COVID$date <= "2022-04-10") #5/29/2021

COVID2 <- read.csv("owid-covid-data2.csv")

COVID <- merge(COVID, COVID2, by=c("location", "date"))

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

for (i in 1:x) {
  if (COVID$date[i] >= "2022-01-01")
    COVID$Week2[i] = COVID$Week[i]+106
  
}

print(COVID$Week2)
summary(COVID$Week2)


#Aggregate by weekly

world_em <- aggregate(COVID$estimated_daily_excess_deaths, by= list(COVID$Week2, COVID$location), FUN=sum, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_em

colnames(world_em) <- c("Weeks", "location", "EM")
world_em


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

#Remove World and International information
COVID_WRL<-world_em[(world_em$location=="World"),]
str(COVID_WRL)

myts <- ts(COVID_WRL$EM)

auto.arima(myts)
Fit<-Arima(myts,order=c(0,1,0))
summary(Fit)
fcast <- forecast(Fit, h=10)

z <- autoplot(fcast, size = 2,geom = "point")  +
  autolayer(fcast$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(Fit), series='Fitted', lwd = 0.6) + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("Weeks") + ylab(" ") +ggtitle("ARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom",
                                                                
                                                                ) +
  theme( legend.title = element_text(color = "Black", size = 8),
         legend.text = element_text(color = "Black", size = 8))
z
summary(Fit)

#R2
SSE <- sum((resid(Fit[1:118]))^2)
SST <- sum((COVID_WRL$EM[1:118] - mean(COVID_WRL$EM[1:118]))^2)
R_square <- 1 - SSE / SST
R_square


#Prophet

history <- data.frame(ds = seq(as.Date('2021-12-14'), as.Date('2022-04-10'), by = 'd'),
                      y = COVID_WRL$EM)

m3 <- prophet(history)
future <- make_future_dataframe(m3, periods = 10)
fcst3 <- predict(m3, future)
y <-plot(m3, fcst3, xlab="Months", ylab="",axes=FALSE,labels = FALSE, xaxt = F) + ggtitle("Prophet Model") + theme(
  plot.title = element_text(size=12), axis.title.x=element_blank(),
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank())
plot(y)



SSE <- sum((history$y[1:118] - fcst3$yhat[c(1:118)])^2)
SST <- sum((history$y[1:118] - mean(history$y[1:118]))^2)
R_square <- 1 - SSE / SST
R_square

last_fcst3 <- fcst3[118,]
rmse <- sqrt(mean((history$y - fcst3$yhat[c(1:118)])^2))
mae <- mean(abs((history$y - fcst3$yhat[c(1:118)])))
final <- cbind(last_fcst3, rmse, mae)
final



####SES########

library(tidyverse) 
library(fpp2) 
library(ggfortify)
ses.goog <- ses(myts,  
                h = 10) 
summary(ses.goog)

fcast <- forecast(ses.goog, h=10)

x <- autoplot(ses.goog, size = 2,geom = "point")  +
  autolayer(fcast$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(Fit), series='Fitted', lwd = 0.6) + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("") + ylab("Reported CFR (%)") + ggtitle("SES Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom",
                                                                
                                                                axis.text.x=element_blank(),
                                                                axis.ticks.x=element_blank()) +
  theme( legend.title = element_text(color = "Black", size = 8),
         legend.text = element_text(color = "Black", size = 8))
x

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
figure <- ggarrange(y, x, z,
                    labels = c("B", "", ""),
                    ncol = 1, nrow = 3)
figure
#R2
SSE <- sum((resid(ses.goog[1:118]))^2)
SST <- sum((COVID_WRL$EM[1:118] - mean(COVID_WRL$EM[1:118]))^2)
R_square <- 1 - SSE / SST
R_square

gridExtra::grid.arrange(x,z,y)
dev.off()


#Menn kendal
library(Kendall)
library(trend)
world_CFR <- aggregate(COVID_WRL$EM, by= list(COVID_WRL$Weeks), FUN=mean, na.rm=TRUE)

myts <- ts(world_CFR$x[4:118])
t.test(world_CFR$x[4:118])$"conf.int"
mean(world_CFR$x[4:118])

MannKendall(myts)
sens.slope(myts, conf.level = 0.95)



#MAP CFR 
tiff("Map.tiff", units="in", width=6, height=4, res=300)

setwd('F:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID <- read.csv("excess-deaths.csv")
COVID2022$date
COVID2022 <- subset(COVID2022, COVID2022$date == "4/3/2022") #5/29/2021
#Remove World and International information
COVID2022<-COVID2022[!(COVID2022$iso_code=="OWID_AFR" | COVID2022$iso_code=="OWID_ASI" | COVID2022$iso_code=="OWID_EUR" | COVID2022$iso_code=="OWID_INT" | 
                         COVID2022$iso_code=="OWID_KOS" | COVID2022$iso_code=="OWID_NAM" | COVID2022$iso_code=="OWID_CYN" | COVID2022$iso_code=="OWID_SAM" |
                         COVID2022$iso_code=="OWID_WRL" | COVID2022$iso_code == "OWID_EUN" | COVID2022$iso_code == "OWID_OCE"),]


#Creating CFR
COVID2022$CFR2022 <- (COVID2022$total_deaths/COVID2022$total_cases)*100
COVID2022$cfrlog <- log(COVID2022$CFR2022+1)
COVID2022$cfrlog[COVID2022$cfrlog == -Inf] <- NA

library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log

COVID2022$cfrlog <- log10(COVID2022$CFR2022*100) +1

worldgovt <- dplyr::select(COVID2022, region = location, cfrlog = cfrlog, "CC" =  iso_code)
head(worldgovt)

diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
worldgovt <- worldgovt %>% 
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Korea (Rep.)" = "South Korea",
                         "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                         "Congo (Rep.)" = "Republic of Congo")) %>%
  ## Editing the "North Korea" entry is a little trickier for some reason
  mutate(region = case_when((CC == "PRK") ~ "North Korea",
                            TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$CFR <- as.numeric(as.character(worldgovt$cfrlog))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldCFR <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = CFR)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("") + labs(fill = "Reported CFR (%)") +
  plain
x <- plot(worldCFR)
x




####MAP Vaccination

setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID22 <- read.csv("owid-covid-data - Copy.csv")
COVID22$date

COVID22 <- subset(COVID22, COVID22$date == "1/5/2022") #5/29/2021
#Remove World and International information
COVID22<-COVID22[!(COVID22$iso_code=="OWID_AFR" | COVID22$iso_code=="OWID_ASI" | COVID22$iso_code=="OWID_EUR" | COVID22$iso_code=="OWID_INT" | 
                     COVID22$iso_code=="OWID_KOS" | COVID22$iso_code=="OWID_NAM" | COVID22$iso_code=="OWID_CYN" | COVID22$iso_code=="OWID_SAM" |
                     COVID22$iso_code=="OWID_WRL" | COVID22$iso_code == "OWID_EUN" | COVID22$iso_code == "OWID_OCE"),]



library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log

COVID22$cfrlog <- COVID22$total_vaccinations_per_hundred

worldgovt <- dplyr::select(COVID22, region = location, cfrlog = cfrlog, "CC" =  iso_code)
head(worldgovt)

diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
worldgovt <- worldgovt %>% 
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Korea (Rep.)" = "South Korea",
                         "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                         "Congo (Rep.)" = "Republic of Congo")) %>%
  ## Editing the "North Korea" entry is a little trickier for some reason
  mutate(region = case_when((CC == "PRK") ~ "North Korea",
                            TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$CFR <- as.numeric(as.character(worldgovt$cfrlog))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldCFR <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = CFR)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("") + labs(fill = "Vaccination \n(doses/100 people)") +
  plain
y <- plot(worldCFR)
y
gridExtra::grid.arrange(y,x)
dev.off()

cor.test(COVID2022$CFR2022 , COVID22$total_vaccinations_per_hundred, method = c("pearson"), use = "complete.obs")


##Corr 2021

setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID2022 <- read.csv("owid-covid-data.csv")

COVID2022 <- subset(COVID2022, COVID2022$date == "1/5/2021") #5/29/2021
#Remove World and International information
COVID2022<-COVID2022[!(COVID2022$iso_code=="OWID_AFR" | COVID2022$iso_code=="OWID_ASI" | COVID2022$iso_code=="OWID_EUR" | COVID2022$iso_code=="OWID_INT" | 
                         COVID2022$iso_code=="OWID_KOS" | COVID2022$iso_code=="OWID_NAM" | COVID2022$iso_code=="OWID_CYN" | COVID2022$iso_code=="OWID_SAM" |
                         COVID2022$iso_code=="OWID_WRL" | COVID2022$iso_code == "OWID_EUN" | COVID2022$iso_code == "OWID_OCE"),]


#Creating CFR
COVID2022$CFR2022 <- (COVID2022$total_deaths/COVID2022$total_cases)*100



setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID22 <- read.csv("owid-covid-data - Copy.csv")
COVID22$date

COVID22 <- subset(COVID22, COVID22$date == "1/5/2021") #5/29/2021
#Remove World and International information
COVID22<-COVID22[!(COVID22$iso_code=="OWID_AFR" | COVID22$iso_code=="OWID_ASI" | COVID22$iso_code=="OWID_EUR" | COVID22$iso_code=="OWID_INT" | 
                     COVID22$iso_code=="OWID_KOS" | COVID22$iso_code=="OWID_NAM" | COVID22$iso_code=="OWID_CYN" | COVID22$iso_code=="OWID_SAM" |
                     COVID22$iso_code=="OWID_WRL" | COVID22$iso_code == "OWID_EUN" | COVID22$iso_code == "OWID_OCE"),]


COVID22$total_vaccinations_per_hundred

cor (COVID2022$CFR2022 , COVID22$total_vaccinations_per_hundred, method = c("pearson"), use = "complete.obs")
cor.test(COVID2022$CFR2022 , COVID22$total_vaccinations_per_hundred, method = c("pearson"), use = "complete.obs")












#GLMM
#Data Management
setwd('F:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID <- read.csv("owid-covid-data2.csv")

COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020 2020-08-08
COVID <- subset(COVID, COVID$date <= "2022-04-10") #5/29/2021

#Remove World and International information
COVID<-COVID[!(COVID$iso_code=="OWID_AFR" | COVID$iso_code=="OWID_ASI" | COVID$iso_code=="OWID_EUR" | COVID$iso_code=="OWID_INT" | 
                 COVID$iso_code=="OWID_KOS" | COVID$iso_code=="OWID_NAM" | COVID$iso_code=="OWID_CYN" | COVID$iso_code=="OWID_SAM" |
                 COVID$iso_code=="OWID_WRL" | COVID$iso_code == "OWID_EUN" | COVID$iso_code == "OWID_OCE"),]
COVID <- COVID[ which(COVID$population >= 1000000), ]

#Creating CFR
COVID$CFR <- (COVID$total_deaths/COVID$total_cases)


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

for (i in 1:x) {
  if (COVID$date[i] >= "2022-01-01")
    COVID$Week2[i] = COVID$Week[i]+106
  
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

write.csv(ModelData2,"ModelData22.csv")

#Replace missing with previous observation

World <- read.csv("WorldData121.csv")
#GLMM Model

library(glmmTMB)
World$Vaccinationph2
World$vml <- World$Vaccinationph2
World$vml2 <- scale(World$Vaccinationph2)

World$lw <- log(World$Weeks)

World$TTs <- World$TotalTestpt
World$TTs2 <- scale(World$TTs)

World$GDP2 <- scale(World$GDP)

World$PopulationDensity2 <- World$PopulationDensity
World$PopulationDensity22 <- scale(World$PopulationDensity2)

World$GHSI2 <- scale(World$GHSI)

World$WGI2 <- scale(World$WGI)

World$Age65Older2 <- World$Age65Older
World$Age65Older22 <- scale(World$Age65Older2)

World$Obesity_rate2 <- World$Obesity_rate
World$Obesity_rate22 <- scale(World$Obesity_rate2)

World$CFR2 <- World$CFR/100
World$CFR22 <- scale(World$CFR2)

World$SI2 <- scale(World$SI)

World$location <- as.factor(World$location)
nrow(World)

library(glmmTMB)
library(DHARMa)
library(performance)

summary(World$CFR2)
World$CFR2[World$CFR2 == 1] <- NA

fit <- glmmTMB(CFR2~ vml2*lw +Age65Older22 + PopulationDensity22 
               + scale(TotalTestpt) + GDP2 + GHSI2 
               + WGI2 + Obesity_rate22 + SI2 + (1|location) + (1| lw), na.action=na.omit, family = beta_family(link = "logit"),  data = World)
library(car)
summary(fit)
round(exp(confint(fit)),3)
options(scipen = 999) 
performance::performance(fit)




#Top20 CFR Countries in last week
Top20 <- subset(World, World$Weeks == 103) #1/1/2020

Top20$CFRprcnt <- Top20$CFR*100

write.csv(Top20,"Weeks103Only.csv")

Top20$CFR
sort_Top20<- Top20[order(-Top20$CFRprcnt),]
sort_Top20$location

#Time series
tiff("CFR.tiff", units="in", width=6, height=5, res=300)

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

COVID <- read.csv("owid-covid-data2.csv")

COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020
COVID <- subset(COVID, COVID$date <= "2022-04-10") #5/29/2021

#Remove World and International information
COVID_WRL<-COVID[(COVID$iso_code=="OWID_WRL"),]
str(COVID_WRL)
#Creating CFR
COVID_WRL$CFR <- (COVID_WRL$total_deaths/COVID_WRL$total_cases)*100
COVID_WRL$CFR

myts <- ts(COVID_WRL$CFR,frequency=365, start=c(2020,6))

auto.arima(myts)
Fit<-Arima(myts,order=c(4,1,3))
summary(Fit)
fcast <- forecast(Fit, h=10)

z <- autoplot(fcast, size = 2,geom = "point")  +
  autolayer(fcast$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(Fit), series='Fitted', lwd = 0.6) + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("Months") + ylab(" ") +ggtitle("ARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.title = element_text(color = "Black", size = 8),
         legend.text = element_text(color = "Black", size = 8))
z
summary(Fit)

#R2
SSE <- sum((resid(Fit[1:810]))^2)
SST <- sum((COVID_WRL$CFR[1:810] - mean(COVID_WRL$CFR[1:810]))^2)
R_square <- 1 - SSE / SST
R_square


#Prophet

World <- read.csv("COVID_WRL.csv")

history <- data.frame(ds = seq(as.Date('2020-01-22'), as.Date('2022-04-10'), by = 'd'),
                      y = COVID_WRL$CFR)

m3 <- prophet(history)
future <- make_future_dataframe(m3, periods = 10)
fcst3 <- predict(m3, future)
y <-plot(m3, fcst3, xlab="Months", ylab="",axes=FALSE,labels = FALSE, xaxt = F) + ggtitle("Prophet Model") + theme(
  plot.title = element_text(size=12), axis.title.x=element_blank(),
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank())
plot(y)



SSE <- sum((history$y[1:810] - fcst3$yhat[c(1:810)])^2)
SST <- sum((history$y[1:810] - mean(history$y[1:810]))^2)
R_square <- 1 - SSE / SST
R_square

last_fcst3 <- fcst3[810,]
rmse <- sqrt(mean((history$y - fcst3$yhat[c(1:810)])^2))
mae <- mean(abs((history$y - fcst3$yhat[c(1:810)])))
final <- cbind(last_fcst3, rmse, mae)
final



####SES########

library(tidyverse) 
library(fpp2) 
library(ggfortify)
ses.goog <- ses(myts,  
                h = 10) 
summary(ses.goog)

fcast <- forecast(ses.goog, h=10)

x <- autoplot(ses.goog, size = 2,geom = "point")  +
  autolayer(fcast$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(Fit), series='Fitted', lwd = 0.6) + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("") + ylab("Reported CFR (%)") + ggtitle("SES Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom",
                                                                
                                                                axis.text.x=element_blank(),
                                                                axis.ticks.x=element_blank()) +
  theme( legend.title = element_text(color = "Black", size = 8),
         legend.text = element_text(color = "Black", size = 8))
x

library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
figure <- ggarrange(y, x, z,
                    labels = c("A", "", ""),
                    ncol = 1, nrow = 3)
figure
#R2
SSE <- sum((resid(ses.goog[1:810]))^2)
SST <- sum((COVID_WRL$CFR[1:810] - mean(COVID_WRL$CFR[1:810]))^2)
R_square <- 1 - SSE / SST
R_square

gridExtra::grid.arrange(x,z,y)
dev.off()

#Menn kendal
library(Kendall)
library(trend)
world_CFR <- aggregate(ModelData2$CFR, by= list(ModelData2$Weeks), FUN=mean, na.rm=TRUE)

myts <- ts(world_CFR$x[4:121])
t.test(world_CFR$x[4:121])$"conf.int"
mean(world_CFR$x[4:121])

MannKendall(myts)
sens.slope(myts, conf.level = 0.95)



#MAP CFR 
tiff("Map.tiff", units="in", width=6, height=4, res=300)

setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID2022 <- read.csv("owid-covid-data.csv")
COVID2022$date
COVID2022 <- subset(COVID2022, COVID2022$date == "1/5/2022") #5/29/2021
#Remove World and International information
COVID2022<-COVID2022[!(COVID2022$iso_code=="OWID_AFR" | COVID2022$iso_code=="OWID_ASI" | COVID2022$iso_code=="OWID_EUR" | COVID2022$iso_code=="OWID_INT" | 
                         COVID2022$iso_code=="OWID_KOS" | COVID2022$iso_code=="OWID_NAM" | COVID2022$iso_code=="OWID_CYN" | COVID2022$iso_code=="OWID_SAM" |
                         COVID2022$iso_code=="OWID_WRL" | COVID2022$iso_code == "OWID_EUN" | COVID2022$iso_code == "OWID_OCE"),]


#Creating CFR
COVID2022$CFR2022 <- (COVID2022$total_deaths/COVID2022$total_cases)*100
COVID2022$cfrlog <- log(COVID2022$CFR2022+1)
COVID2022$cfrlog[COVID2022$cfrlog == -Inf] <- NA

library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log

COVID2022$cfrlog <- log10(COVID2022$CFR2022*100) +1

worldgovt <- dplyr::select(COVID2022, region = location, cfrlog = cfrlog, "CC" =  iso_code)
head(worldgovt)

diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
worldgovt <- worldgovt %>% 
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Korea (Rep.)" = "South Korea",
                         "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                         "Congo (Rep.)" = "Republic of Congo")) %>%
  ## Editing the "North Korea" entry is a little trickier for some reason
  mutate(region = case_when((CC == "PRK") ~ "North Korea",
                            TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$CFR <- as.numeric(as.character(worldgovt$cfrlog))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldCFR <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = CFR)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("") + labs(fill = "Reported CFR (%)") +
  plain
x <- plot(worldCFR)
x




####MAP Vaccination

setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID22 <- read.csv("owid-covid-data - Copy.csv")
COVID22$date

COVID22 <- subset(COVID22, COVID22$date == "1/5/2022") #5/29/2021
#Remove World and International information
COVID22<-COVID22[!(COVID22$iso_code=="OWID_AFR" | COVID22$iso_code=="OWID_ASI" | COVID22$iso_code=="OWID_EUR" | COVID22$iso_code=="OWID_INT" | 
                         COVID22$iso_code=="OWID_KOS" | COVID22$iso_code=="OWID_NAM" | COVID22$iso_code=="OWID_CYN" | COVID22$iso_code=="OWID_SAM" |
                         COVID22$iso_code=="OWID_WRL" | COVID22$iso_code == "OWID_EUN" | COVID22$iso_code == "OWID_OCE"),]



library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log

COVID22$cfrlog <- COVID22$total_vaccinations_per_hundred

worldgovt <- dplyr::select(COVID22, region = location, cfrlog = cfrlog, "CC" =  iso_code)
head(worldgovt)

diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
worldgovt <- worldgovt %>% 
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Korea (Rep.)" = "South Korea",
                         "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                         "Congo (Rep.)" = "Republic of Congo")) %>%
  ## Editing the "North Korea" entry is a little trickier for some reason
  mutate(region = case_when((CC == "PRK") ~ "North Korea",
                            TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$CFR <- as.numeric(as.character(worldgovt$cfrlog))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldCFR <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = CFR)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("") + labs(fill = "Vaccination \n(doses/100 people)") +
  plain
y <- plot(worldCFR)
y
gridExtra::grid.arrange(y,x)
dev.off()

cor.test(COVID2022$CFR2022 , COVID22$total_vaccinations_per_hundred, method = c("pearson"), use = "complete.obs")


##Corr 2021

setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID2022 <- read.csv("owid-covid-data.csv")

COVID2022 <- subset(COVID2022, COVID2022$date == "1/5/2021") #5/29/2021
#Remove World and International information
COVID2022<-COVID2022[!(COVID2022$iso_code=="OWID_AFR" | COVID2022$iso_code=="OWID_ASI" | COVID2022$iso_code=="OWID_EUR" | COVID2022$iso_code=="OWID_INT" | 
                         COVID2022$iso_code=="OWID_KOS" | COVID2022$iso_code=="OWID_NAM" | COVID2022$iso_code=="OWID_CYN" | COVID2022$iso_code=="OWID_SAM" |
                         COVID2022$iso_code=="OWID_WRL" | COVID2022$iso_code == "OWID_EUN" | COVID2022$iso_code == "OWID_OCE"),]


#Creating CFR
COVID2022$CFR2022 <- (COVID2022$total_deaths/COVID2022$total_cases)*100



setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID22 <- read.csv("owid-covid-data - Copy.csv")
COVID22$date

COVID22 <- subset(COVID22, COVID22$date == "1/5/2021") #5/29/2021
#Remove World and International information
COVID22<-COVID22[!(COVID22$iso_code=="OWID_AFR" | COVID22$iso_code=="OWID_ASI" | COVID22$iso_code=="OWID_EUR" | COVID22$iso_code=="OWID_INT" | 
                     COVID22$iso_code=="OWID_KOS" | COVID22$iso_code=="OWID_NAM" | COVID22$iso_code=="OWID_CYN" | COVID22$iso_code=="OWID_SAM" |
                     COVID22$iso_code=="OWID_WRL" | COVID22$iso_code == "OWID_EUN" | COVID22$iso_code == "OWID_OCE"),]


COVID22$total_vaccinations_per_hundred

cor (COVID2022$CFR2022 , COVID22$total_vaccinations_per_hundred, method = c("pearson"), use = "complete.obs")
cor.test(COVID2022$CFR2022 , COVID22$total_vaccinations_per_hundred, method = c("pearson"), use = "complete.obs")

