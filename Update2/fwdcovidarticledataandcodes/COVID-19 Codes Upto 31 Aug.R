###########  Afghanishtan #######################
#require(ggplot2)
#require(gridExtra)
library(psych) # for descriptive analysis
library(forecast)
library(lmtest)
library(tseries)

df<- read.csv("F:/Research Work New/Covid 19/Data/Important Data/Final Data/Fresh_Data_Upto_Aug.csv", header = T, sep = ",")
id<- c(1:988)

c<-cbind(df,id)
covid<-c[c$Country=="Afghanistan",] 

z<- covid$Confirmed
# z[z=="-Inf"]<-0

plot.ts(z, xlab="Days after first confirmed case", ylab="Confimed Cases")
library(MASS)
z2<- z+1
boxcox(z2~1) # suggests log transformation
z1<- log(z)
z1[z1=="-Inf"]<-0
plot.ts(z1)
adf.test(z1) ## not trend stationary (>0.05)
kpss.test(z1) # notlevel/mean stationary coz p<.05

ft<- diff(z1)
plot.ts(ft)
adf.test(ft)    ## trend stationary (<.05)
kpss.test(ft)  ## level/ mean stationary (>.05)

acf(ft)  # cuts of after lag 2
pacf(ft)  # tails off, probable model ARIMA (0,1,2)
### model selection

auto.arima(z) # 1,1,2 # in residuals it presents autocorrelation. So avoid auto arima
c<-Arima(z, order=c(0,1,1)) 
library(lmtest)
coeftest(c)
summary(c)
checkresiduals(c)  ### >.05 no autocorrelation


Arima(z, order=c(0,1,2))$bic #  
Arima(z, order=c(0,1,1))$bic #  2nd but auto
Arima(z, order=c(0,1,3))$bic # 3rd, para insig but no auto
Arima(z, order=c(1,1,1))$bic # parametrs insig
Arima(z, order=c(1,1,2))$bic  ## 1st, para sig but autocorrelation exists
Arima(z, order=c(2,1,1))$bic   ### 
Arima(z, order=c(2,1,2))$bic # 4th, para sig and no auto
Arima(z, order=c(3,1,2))$bic   ### 
Arima(z, order=c(2,2,3))$bic
### model fitting
Fit<-Arima(z, order=c(2,1,2))  ## some AR or MA terms coefficients are insignificant, so first of all select a model which all parameters are significant and pass residual test
library(lmtest)
coeftest(Fit)
summary(Fit)
### model diagnosis
checkresiduals(Fit)   ### autocorrelation present in Ljug box test

## fitted vs observed
library(graphics)
plot.ts(z) 
plot.ts(z, col=8, ylab=expression(hat(Confirmed)), main="Observed vs Fitted Plot (Afghanistan)") 
lines(fitted(Fit), col=2)
legend("topleft", c("Observed", "Fitted"), 
       col = c("gray", "red"),
       pch = c(15, 15), cex = 0.8, 
       box.col = "black"
)



# prewetened factors by fitted arima model
die<- residuals(Arima(z, model = Fit))
maxt<-residuals(Arima(covid$Max.Temp, model = Fit)) 
mint<-residuals(Arima(covid$Min.Temp, model = Fit)) 
hum<-residuals(Arima(covid$Relative.humidity, model = Fit)) 
php<-residuals(Arima(covid$Surface.Pressure, model = Fit)) 
rain<-residuals(Arima(covid$Rainfall, model = Fit))
ws<-residuals(Arima(covid$Max_Windspeed_10meters, model = Fit))
pm<- residuals(Arima(covid$pm25_max, model = Fit))

## Cross correlation Function
ccf(die, maxt, 12, main="AFG Confirmed vs Tmax", ylab="CCF") #  12 (considering positive lag only)
ccf(die, mint, 12, main="AFG Confirmed  vs Tmin", ylab="CCF") # 5
ccf(die, hum, 12, main="AFG Confirmed  vs RH", ylab="CCF") # no
ccf(die, php, 12, main="AFG Confirmed vs pressure", ylab="CCF")  # no
ccf(die, rain, 12, main="AFG Confirmed  vs rainfall", ylab="CCF") # no
ccf(die, ws, 12, main="AFG Confirmed vs Wspeed", ylab="CCF")  #
ccf(die, pm, 12, main="AFG Confirmed vs PM 2.5", ylab="CCF") # 

## create a matrix of independent variable

rf <- c(NA, NA,NA,NA,NA,NA, NA,NA,NA,NA, NA,NA, covid$Max.Temp)
rf_tmax<- embed(rf,13) 
Tmax_12<- rf_tmax[,13]

mn<- c(NA,NA,NA,NA,NA, covid$Min.Temp)
rf_tmin<- embed(mn,6)
Tmin_5<- rf_tmin[,6]

vars_matrix <- cbind(Tmax_12,Tmin_5)

require(forecast)
model_f = arima(covid$Confirmed, order = c(2,1,2), xreg = vars_matrix[,1:2]) # positive impact 5.18

coeftest(model_f)
summary(model_f)
checkresiduals(model_f)

########################### Bangladesh #################

covid<-df[df$Country=="Bangladesh",] 

#c<-covid[c(-1,-2)]
#cor(c)
## fit arima model for dependent variable confirmed cases

plot.ts(covid$Confirmed, xlab="Days after first confirmed case", ylab="Daily Confirmed Cases of Bangladesh")

library(tseries)
z<- covid$Confirmed
z1<- z+1
boxcox(z1~1)

z2<- log(z)
z2[z2=="-Inf"]<-0
plot.ts(z2,xlab="Days after first confirmed case", ylab="log(Confirmed Cases)")
adf.test(z2) ## not trend stationary
kpss.test(z2) # not level/mean stationary coz p<.05
ys<- diff(z2)
plot.ts(ys)
adf.test(ys)  ##trend stationary
kpss.test(ys) ##  mean 
acf(ys)
pacf(ys)

### model selection

auto.arima(z)  # (1,1,1) # auto correlation present but this is best
d<- Arima(z, order=c(1,1,1)) # goood
coeftest(d)
summary(d)
checkresiduals(d)

Arima(z, order=c(1,1,1))$bic #                   
Arima(z, order=c(2,1,0))$bic # 
Arima(z, order=c(2,1,1))$bic # 
Arima(z, order=c(2,1,2))$bic # 
Arima(z, order=c(2,1,3))$bic # 
Arima(z, order=c(3,1,0))$bic #  
Arima(z, order=c(3,1,1))$bic #  
Arima(z, order=c(3,1,2))$bic # 1st sig & no autocorrelation

Arima(z, order=c(0,1,2))$bic #  
Arima(z, order=c(0,1,1))$bic #  
Arima(z, order=c(0,1,3))$bic # 
Arima(z, order=c(1,1,1))$bic # 
Arima(z, order=c(1,1,2))$bic 
Arima(z, order=c(2,1,1))$bic  
Arima(z, order=c(2,1,2))$bic # 
Arima(z, order=c(3,1,2))$bic   
Arima(z, order=c(2,2,3))$bic

### model fitting
Fit<-Arima(z, order=c(3,1,2))
library(lmtest)
coeftest(Fit)

### model diagnosis
checkresiduals(Fit)   

## fitted vs observed
plot.ts(z) 
plot.ts(z, col=8, ylab=expression(hat(Confirmed)),main="Observed vs Fitted Plot (Bangladesh)") 
lines(fitted(Fit), col=2)
legend("topleft", c("Observed", "Fitted"), 
       col = c("gray", "red"),
       pch = c(15, 15), cex = 0.8, 
       box.col = "black"
)
# prewetened factors by fitted arima model
die<- residuals(Arima(z, model = Fit))
maxt<-residuals(Arima(covid$Max.Temp, model = Fit)) 
mint<-residuals(Arima(covid$Min.Temp, model = Fit)) 
hum<-residuals(Arima(covid$Relative.humidity, model = Fit)) 
php<-residuals(Arima(covid$Surface.Pressure, model = Fit)) 
rain<-residuals(Arima(covid$Rainfall, model = Fit))
ws<-residuals(Arima(covid$Max_Windspeed_10meters, model = Fit))
pm<- residuals(Arima(covid$pm25_max, model = Fit))

## use both outcome and indep var prewhitened

ccf(die, maxt, 12, main="BD Confirmed vs Tmax", ylab="CCF") # 3, (considering positive lag only)
ccf(die, mint, 12, main="BD Confirmed  vs Tmin", ylab="CCF") # no
ccf(die, hum, 12, main="BD Confirmed  vs RH", ylab="CCF")         # 
ccf(die, php, 12, main="BD Confirmed vs Pressure", ylab="CCF")  # no
ccf(die, rain, 12, main="BD Confirmed  vs Rainfall", ylab="CCF") # 6
ccf(die, ws, 12, main="BD Confirmed vs Wspeed", ylab="CCF")      # 6
ccf(die, pm, 12, main="BD Confirmed vs PM 2.5", ylab="CCF") # no

## create a matrix of independ variabe
mx<- c(NA,NA,NA,covid$Max.Temp)
rf_mx<- embed(mx,4)
Tmax_3<- rf_mx[,4]


rn<- c(NA,NA,NA,NA,NA,NA,covid$Rainfall)
rf_rn<- embed(rn,7)
Rain_6<- rf_rn[,7]

wn<- c(NA,NA,NA,NA,NA,NA,covid$Max_Windspeed_10meters)
rf_wn<- embed(wn,7)
Wind_6<- rf_wn[,7]

vars_matrix<- cbind(Tmax_3,Rain_6,Wind_6)

model_f = arima(covid$Confirmed, order = c(3,1,2), xreg = vars_matrix[,1:3])
library(lmtest)
coeftest(model_f)

summary(model_f)
checkresiduals(model_f)


########################### INDIA #################

covid<-df[df$Country=="India",] 

## fit arima model for dependent variable confirmed cases
plot.ts(covid$Confirmed, xlab="Days after first confirmed case", ylab="Daily Confirmed Cases of India")
library(tseries)
library(MASS)
z<- covid$Confirmed
as.ts(z)
z1<- z+1
boxcox(z1~1)  # lamda is closely 0 so use log transformation

z2<-log(z)
z2[z2=="-Inf"]<-0
plot.ts(z2)

adf.test(z) ## not trend stationary
kpss.test(z) # not level/mean stationary coz p<.05

ys<- diff(z)
adf.test(ys)    ## trend stationary
kpss.test(ys)  ## level stationary
yt<- diff(ys)
acf(yt)  # cuts after lag 2
pacf(yt)  ## tails off, (0,2,2)


### model selection

auto.arima(z)  # (0,1,2)  parameter insig and auto correlation

Arima(z, order=c(0,2,2))$bic # 2nd sig but greater auto
Arima(z, order=c(0,2,1))$bic  # 4th sig but auto
Arima(z, order=c(0,2,3))$bic  # 6th
Arima(z, order=c(1,2,1))$bic  # 3rd, sig but auto. 
Arima(z, order=c(1,2,2))$bic
Arima(z, order=c(1,2,3))$bic 
Arima(z, order=c(2,2,1))$bic # 5th
Arima(z, order=c(2,2,2))$bic 
Arima(z, order=c(2,2,3))$bic #  1st  #  sig but auto. BIC lowest but parameter sig nd have autocorrelation. 
#Since we dont have any model which free from auto correaltion so we choose this one
Arima(z, order=c(0,2,3))$bic
Arima(z, order=c(3,1,2))$bic
Arima(z, order=c(2,1,1))$bic
Arima(z, order=c(2,1,2))$bic
Arima(z, order=c(0,1,3))$bic

### model fitting
Fit<-Arima(z, order=c(2,2,3))  # good
coeftest(Fit)
summary(Fit)
### model diagnosis
checkresiduals(Fit)   

## fitted vs observed
plot.ts(z) 
plot.ts(z, col=8, ylab=expression(hat(Confirmed)), main="Observed vs Fitted Plot (India)") 
lines(fitted(Fit), col=2)
legend("topleft", c("Observed", "Fitted"), 
       col = c("gray", "red"),
       pch = c(15, 15), cex = 0.8, 
       box.col = "black"
)
# prewetened factors by fitted arima model
die<- residuals(Arima(z, model = Fit))
maxt<-residuals(Arima(covid$Max.Temp, model = Fit)) 
mint<-residuals(Arima(covid$Min.Temp, model = Fit)) 
hum<-residuals(Arima(covid$Relative.humidity, model = Fit)) 
php<-residuals(Arima(covid$Surface.Pressure, model = Fit)) 
rain<-residuals(Arima(covid$Rainfall, model = Fit))
ws<-residuals(Arima(covid$Max_Windspeed_10meters, model = Fit))
pm<- residuals(Arima(covid$pm25_max, model = Fit))

## use both outcome and indep var prewhitened

ccf(die, maxt, 12, main="India Confirmed vs Tmax", ylab="CCF") # (considering positive lag only)
ccf(die, mint, 12, main="India Confirmed vs Tmin", ylab="CCF") # 
ccf(die, hum, 12, main="India Confirmed  vs RH", ylab="CCF") #  
ccf(die, php, 12, main="India Confirmed vs pressure", ylab="CCF")  # 
ccf(die, rain, 12, main="India Confirmed vs rainfall", ylab="CCF") # 6
ccf(die, ws, 12, main="India Confirmed vs wspeed", ylab="CCF")# 8
ccf(die, pm, 12, main="India Confirmed vs PM 2.5", ylab="CCF") # 2

## create a matrix of independ variable

rn<-c(NA,NA,NA,NA,NA,NA,covid$Rainfall)
rf_rn<- embed(rn,7)
Rain_6<- rf_rn[,7]

wn<-c(NA,NA,NA,NA,NA,NA,NA,NA,covid$Max_Windspeed_10meters)
rf_wn<- embed(wn,9)
Wind_8<- rf_wn[,9]

pm<-c(NA,NA,covid$pm25_max)
rf_pm<- embed(pm,3)
PM_2<- rf_pm[,3]

vars_matrix <- cbind(Rain_6,Wind_8,PM_2)


model_f = arima(covid$Confirmed, order = c(2,2,3), xreg =vars_matrix[,1:3])
coeftest(model_f)
summary(model_f)
checkresiduals(model_f)



###############  Pakistan

covid<-df[df$Country=="Pakistan",] 

## fit arima model for dependent variable confirmed cases
p1 <- ggplot(covid, aes(x =covid$id, y = covid$Confirmed)) +
  ylab("Daily Confimed Cases for Sri Lanka") +
  xlab("Days after first confirmed case") +
  geom_line() +
  expand_limits(x = 0, y = 0)
grid.arrange(p1, ncol=1, nrow=1)
## fit arima model for dependent variable confirmed cases
plot.ts(covid$Confirmed, xlab="Days after first confirmed case", ylab="Daily Confirmed Cases of Pakistan")
library(tseries)

z<- covid$Confirmed
as.ts(z)
z1<- z+1
boxcox(z1~1)  # lamda is closely 0 so use log transformation

z2<-log(z)
z2[z2=="-Inf"]<-0
plot.ts(z2)

adf.test(z2) ## not trend stationary
kpss.test(z2) # not level/mean stationary coz p<.05

ys<- diff(z2)
adf.test(ys)    ## trend stationary
kpss.test(ys)  ## level stationary
acf(ys)  # cuts after lag 2
pacf(ys)  ## tails off, (0,1,2)

# mean stationary

### model selection
library(zoo)

ftfinal.aic <- Inf
ftfinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
  ftcurrent.aic <- AIC(arima(ft, order=c(p, d, q)))
  if (ftcurrent.aic < ftfinal.aic) {
    ftfinal.aic <- ftcurrent.aic
    ftfinal.order <- c(p, d, q)
    ftfinal.arima <- arima(ft, order=ftfinal.order)
  }
}
ftfinal.order ## 1,1,4

auto.arima(z) # 0,1,4     2nd 
Arima(z, order=c(0,1,1))$bic
Arima(z, order=c(0,1,2))$bic  

Arima(z, order=c(0,1,3))$bic         ## 1st,best  sig & no auto
Arima(z, order=c(1,1,0))$bic  #
Arima(z, order=c(1,1,1))$bic  # 

Arima(z, order=c(1,1,2))$bic
Arima(z, order=c(1,1,3))$bic   ## 3rd
Arima(z, order=c(1,1,4))$bic   # 

Arima(z, order=c(2,1,0))$bic  ##
Arima(z, order=c(2,1,1))$bic  # 3343

Arima(z, order=c(2,1,2))$bic
Arima(z, order=c(2,1,3))$bic 
Arima(z, order=c(3,1,1))$bic   
Arima(z, order=c(3,1,2))$bic 
Arima(z, order=c(2,2,3))$bic
### model fitting
Fit<-Arima(z, order=c(0,1,3)) ## good para sig and no correlation)
coeftest(Fit)
summary(Fit)
### model diagnosis
checkresiduals(Fit)   

## fitted vs observed
plot.ts(z) 
plot.ts(z, col=8, ylab=expression(hat(Confirmed)), main="Observed vs Fitted (Pakistan)") 
lines(fitted(Fit), col=2)
legend("topleft", c("Observed", "Fitted"), 
       col = c("gray", "red"),
       pch = c(15, 15), cex = 0.8, 
       box.col = "black"
)
# prewetened factors by fitted arima model
die<- residuals(Arima(z, model = Fit))
maxt<-residuals(Arima(covid$Max.Temp, model = Fit)) 
mint<-residuals(Arima(covid$Min.Temp, model = Fit)) 
hum<-residuals(Arima(covid$Relative.humidity, model = Fit)) 
php<-residuals(Arima(covid$Surface.Pressure, model = Fit)) 
rain<-residuals(Arima(covid$Rainfall, model = Fit))
ws<-residuals(Arima(covid$Max_Windspeed_10meters, model = Fit))
pm<- residuals(Arima(covid$pm25_max, model = Fit))

## use both outcome and indep var prewhitened

ccf(die, maxt, 12, main="Pakistan Confirmed vs Tmax", ylab="CCF") #  (considering positive lag only)
ccf(die, mint, 12, main="Pakistan Confirmed  vs Tmin", ylab="CCF") # 3, 10
ccf(die, hum, 12, main="Pakistan Confirmed  vs RH", ylab="CCF") # 
ccf(die, php, 12, main="Pakistan Confirmed vs Pressure", ylab="CCF")  # 
ccf(die, rain, 12, main="Pakistan Confirmed  vs Rainfall", ylab="CCF") # 10,12 
ccf(die, ws, 12, main="Pakistan Confirmed vs wspeed", ylab="CCF")      # lag 8
ccf(die, pm, 12, main="Pakistan Confirmed vs PM 2.5", ylab="CCF") # 10

## create a matrix of independ variable

mn<- c(NA,NA,NA,NA, NA,NA,NA,NA,NA,NA,covid$Min.Temp)
rf_min<- embed(mn, 11)
Tmin_3<- rf_min[,4]
Tmin_10<- rf_min[,11]

rn<- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, covid$Rainfall)
rf_rn<- embed(rn, 13)
Rain_10<- rf_rn[,11]
Rain_12<- rf_rn[,13]

wn<- c(NA,NA,NA,NA, NA,NA,NA,NA,covid$Max_Windspeed_10meters)
rf_wn<- embed(wn,9)
Wind_8<- rf_wn[,9]

pm<- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,covid$pm25_max)
rf_pm<- embed(pm,11)
pm_10<- rf_pm[,11]


vars_matrix <- cbind(Tmin_3, Tmin_10, Rain_10, Rain_12,Wind_8,pm_10)

require(forecast)

model_f = arima(z, order = c(0,1,3), xreg = vars_matrix[,c(1:6)])
coeftest(model_f)

summary(model_f)
checkresiduals(model_f)

########################  Srilanka

covid<-df[df$Country=="Sri Lanka",] 

## fit arima model for dependent variable confirmed cases
p1 <- ggplot(covid, aes(x =id, y = covid$Confirmed)) +
  ylab("Confirmed") +
  xlab("Days after first confirmed case") +
  geom_line() +
  expand_limits(x = 0, y = 0)
grid.arrange(p1, ncol=1, nrow=1)


## fit arima model for dependent variable confirmed cases

z<- covid$Confirmed
as.ts(z)
z1<- z+1
boxcox(z1~1)  # lamda is exactly 0 so use log transformation

z2<-log(z)
z2[z2=="-Inf"]<-0
plot.ts(z2)

adf.test(z2) ## not trend stationary
kpss.test(z2) # not level/mean stationary coz p<.05

ys<- diff(z2)
adf.test(ys)    ## trend stationary
kpss.test(ys)  ## level stationary
acf(ys)  # cuts after lag 2
pacf(ys)  ## tails off, (0,1,2)



### model selection

auto.arima(z)  # (2,1,1) 

Arima(z, order=c(0,1,1))$bic
Arima(z, order=c(0,1,2))$bic  

Arima(z, order=c(0,1,3))$bic  
Arima(z, order=c(1,1,0))$bic  #
Arima(z, order=c(1,1,1))$bic  # 

Arima(z, order=c(1,1,2))$bic
Arima(z, order=c(1,1,3))$bic   ## 3rd
Arima(z, order=c(1,1,4))$bic   # 

Arima(z, order=c(2,1,0))$bic  ##
Arima(z, order=c(2,1,1))$bic  # 1st sig and no auto

Arima(z, order=c(2,1,2))$bic
Arima(z, order=c(2,1,3))$bic 
Arima(z, order=c(3,1,1))$bic   
Arima(z, order=c(3,1,2))$bic 
Arima(z, order=c(2,2,3))$bic


### model fitting
Fit<-Arima(z, order=c(2,1,1))  ## good
library(lmtest)
coeftest(Fit)
summary(Fit)
### model diagnosis
checkresiduals(Fit)   

## fitted vs observed
plot.ts(z) 
plot.ts(z, col=8, ylab=expression(hat(Confirmed)), main="Observed vs Fitted Plot (Sri Lanka)") 
lines(fitted(Fit), col=2)
legend("topleft", c("Observed", "Fitted"), 
       col = c("gray", "red"),
       pch = c(15, 15), cex = 0.8, 
       box.col = "black"
)
# prewetened factors by fitted arima model
die<- residuals(Arima(z, model = Fit))
maxt<-residuals(Arima(covid$Max.Temp, model = Fit)) 
mint<-residuals(Arima(covid$Min.Temp, model = Fit)) 
hum<-residuals(Arima(covid$Relative.humidity, model = Fit)) 
php<-residuals(Arima(covid$Surface.Pressure, model = Fit)) 
rain<-residuals(Arima(covid$Rainfall, model = Fit))
ws<-residuals(Arima(covid$Max_Windspeed_10meters, model = Fit))
pm<- residuals(Arima(covid$pm25_max, model = Fit))

#explantory<- cbind(maxt,mint,hum,php,rain,ws,pm)
#cor(explantory)  # Tmax has corr with Humidity (r=-0.77)

## use both outcome and indep var prewhitened

ccf(die, maxt, 12, main="Sri Lanka Confirmed vs Tmax", ylab="CCF") #  3 (considering positive lag only)
ccf(die, mint, 12, main="Sri Lanka Confirmed  vs Tmin", ylab="CCF") # 10
ccf(die, hum, 12, main="Sri Lanka Confirmed  vs RH", ylab="CCF") # 
ccf(die, php, 12, main="Sri Lanka Confirmed vs Pressure", ylab="CCF")  # 
ccf(die, rain, 12, main="Sri Lanka Confirmed  vs Rainfall", ylab="CCF") # 
ccf(die, ws, 12, main="Sri Lanka Confirmed vs Wspeed", ylab="CCF")     # lag 2
ccf(die, pm, 12, main="Sri Lanka Confirmed vs PM 2.5", ylab="CCF") # lag 2

## create a matrix of independ variable
mx<- c(NA,NA,NA, covid$Max.Temp)
rf_max<- embed(mx, 4)
Tmax_3<- rf_max[,4]

mn<- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, covid$Min.Temp)
rf_mn<- embed(mn,11)
Tmin_10<- rf_mn[,11]

speeed<- c(NA,NA,covid$Max_Windspeed_10meters)
rf_w<- embed(speeed, 3)
Wind_2<- rf_w[,3]


pm<- c(NA,NA, covid$pm25_max)
rf_pm<- embed(pm,3)
pm_2<- rf_pm[,3]

vars_matrix<- cbind(Tmax_3,Tmin_10,Wind_2, pm_2)

model_f = arima(z, order = c(2,1,1), xreg = vars_matrix[,1:4]) 
coeftest(model_f)
summary(model_f)
checkresiduals(model_f)



library(faraway)
faraway::vif(model_f)
library(car)
car::vif(model_f)
vif(model_f)
d<- covid[,c(-1,-2)]
cor(d)
