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


setwd("F:\\ResearchProject\\Jamal Sir\\Vaccination")

par(mfrow=c(3,2))
########################Israel#############################
COVID <- read.csv("Israel.csv")


COVID <- mutate(COVID, Date=as.POSIXct(date))
p <- ggplot(COVID, aes(x = Date)) + scale_x_datetime(expand=c(0,0),
                                                     date_breaks= "10 days", 
                                                     date_minor_breaks = "10 days", 
                                                     date_labels = "%d %b", 
                                                     limits = as.POSIXct(c("2020-12-19 00:00:00", "2021-02-12 00:00:00")))

p
p <- p + geom_line(aes(y = new_cases, colour = "Daily new cases"),size = 1.5)
p
# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = total_vaccinations_per_hundred*200, colour = "Total vaccinations per hundred"),size = 1.5)
p
# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./200, name = "Total vaccinations per hundred"))
p
# modifying colours and theme options
#p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Daily new cases",
              x = "Months",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.3, 0.9))
p
########################UAE#############################
COVID <- read.csv("UAE.csv")
COVID <- mutate(COVID, Date=as.POSIXct(date))
p <- ggplot(COVID, aes(x = Date)) + scale_x_datetime(expand=c(0,0),
                                                     date_breaks= "10 days", 
                                                     date_minor_breaks = "10 days", 
                                                     date_labels = "%d %b", 
                                                     limits = as.POSIXct(c("2021-01-05 00:00:00", "2021-02-12 00:00:00")))

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
p1 <- p + theme(legend.position = c(0.3, 0.9))
p1



########################UK#############################
COVID <- read.csv("UK.csv")


COVID <- mutate(COVID, Date=as.POSIXct(date))
p <- ggplot(COVID, aes(x = Date)) + scale_x_datetime(expand=c(0,0),
                                                     date_breaks= "10 days", 
                                                     date_minor_breaks = "10 days", 
                                                     date_labels = "%d %b", 
                                                     limits = as.POSIXct(c("2021-01-09 00:00:00", "2021-02-12 00:00:00")))

p <- p + geom_line(aes(y = new_cases, colour = "Daily new cases"),size = 1.5)

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = total_vaccinations_per_hundred*2700, colour = "Total vaccinations per hundred"),size = 1.5)

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./2700, name = "Total vaccinations per hundred"))
# modifying colours and theme options
#p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Daily new cases",
              x = "Months",
              colour = "Parameter")
p2 <- p + theme(legend.position = c(0.3, 0.9))
p2


########################US#############################
COVID <- read.csv("US.csv")
options(scipen = 999)
COVID <- mutate(COVID, Date=as.POSIXct(date))
p <- ggplot(COVID, aes(x = Date)) + scale_x_datetime(expand=c(0,0),
                                                     date_breaks= "10 days", 
                                                     date_minor_breaks = "10 days", 
                                                     date_labels = "%d %b", 
                                                     limits = as.POSIXct(c("2020-12-20 00:00:00", "2021-02-12 00:00:00")))

p <- p + geom_line(aes(y = new_cases, colour = "Daily new cases"),size = 1.5)

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = total_vaccinations_per_hundred*18000, colour = "Total vaccinations per hundred"),size = 1.5)

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./18000, name = "Total vaccinations per hundred"))
# modifying colours and theme options
#p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Daily new cases",
              x = "Months",
              colour = "Parameter")
p3 <- p + theme(legend.position = c(0.7, 0.9))
p3


########################Chile#############################
COVID <- read.csv("Chile.csv")
options(scipen = 999)
COVID <- mutate(COVID, Date=as.POSIXct(date))
p <- ggplot(COVID, aes(x = Date)) + scale_x_datetime(expand=c(0,0),
                                                     date_breaks= "10 days", 
                                                     date_minor_breaks = "10 days", 
                                                     date_labels = "%d %b", 
                                                     limits = as.POSIXct(c("2021-01-13 00:00:00", "2021-02-12 00:00:00")))

p <- p + geom_line(aes(y = new_cases, colour = "Daily new cases"),size = 1.5)

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = total_vaccinations_per_hundred*1000, colour = "Total vaccinations per hundred"),size = 1.5)

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Total vaccinations per hundred"))
# modifying colours and theme options
#p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Daily new cases",
              x = "Months",
              colour = "Parameter")
p4 <- p + theme(legend.position = c(0.7, 0.9))
p4
