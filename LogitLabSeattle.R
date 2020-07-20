library(readxl)
library(plyr)
library(dplyr)
library(tidyverse)
library(car)
library(zoo)

setwd("C:/Users/shydhevi/Documents/R")
SeattleRain2018 <- read_excel("C:/Users/shydhevi/Documents/R/SeattleRain.xlsx")


dim(SeattleRain2018)    #returns the dimensions of an object
str(SeattleRain2018)    #returns the structure of an object
sum(is.na(SeattleRain2018)) #returns how many observations have "na"
SeattleRain2018[is.na(SeattleRain2018)] <- '0' #replaces "na" with 0. This is a choice, statistically, but you can't run the regression without it
sum(is.na(SeattleRain2018))
View(SeattleRain2018)

select(SeattleRain2018,-c(PGTM,SNOW,SNWD,WDF2))

View(SeattleRain2018)

#add a Season Variable#


yearquarter <- as.yearqtr(as.yearmon(SeattleRain2018$DATE, "%Y/%m/%d") + 1/12)
SeattleRain2018$Season <- factor(format(yearquarter, "%q"), levels = 1:4, 
                    labels = c("winter", "spring", "summer", "fall"))
View(SeattleRain2018)
glimpse(SeattleRain2018)

#create a Wind Direction factor variable#

SeattleRain2018$NewWindDir<-SeattleRain2018$WDF5
SeattleRain2018$WindCat<-cut(SeattleRain2018$NewWindDir, c(0,22,67,112,157,202,247,292,337,360))
SeattleRain2018$NewWindCat1<-revalue(SeattleRain2018$WindCat, c("(0,22]"="N","(337,360]"="N","(22,67]"="NE","(67,112]"="E","(112,157]"="SE","(157,202]"="S","(202,247]"="SW","(247,292]"="W","(292,337]"="NW" ))
 
view(SeattleRain2018)


#Create Factor variable indicating presence of Rain
SeattleRain2018$RainFac <- ifelse(SeattleRain2018$PRCP > 0, 1, 0)
rain<-factor(SeattleRain2018$RainFac)
list(SeattleRain2018$RainFac)
view(SeattleRain2018)
#create sequential logit models

PredictRain<-glm(rain~SeattleRain2018$AWND, data=SeattleRain2018, family=binomial)
summary(PredictRain)
exp(cbind(Odds_Ratio_RainVNoRain=coef(PredictRain), confint(PredictRain)))

PredictRain<-glm(rain~SeattleRain2018$AWND + SeattleRain2018$TAVG, data=SeattleRain2018, family=binomial)
summary(PredictRain)
exp(cbind(Odds_Ratio_RainVNoRain=coef(PredictRain), confint(PredictRain)))

PredictRain<-glm(rain~SeattleRain2018$AWND + SeattleRain2018$TAVG + SeattleRain2018$TMAX, data=SeattleRain2018, family=binomial)
summary(PredictRain)
exp(cbind(Odds_Ratio_RainVNoRain=coef(PredictRain), confint(PredictRain)))

PredictRain<-glm(rain~SeattleRain2018$AWND + SeattleRain2018$TAVG + SeattleRain2018$TMAX + SeattleRain2018$TMIN, data=SeattleRain2018, family=binomial)
summary(PredictRain)
exp(cbind(Odds_Ratio_RainVNoRain=coef(PredictRain), confint(PredictRain)))

PredictRain<-glm(rain~SeattleRain2018$AWND + SeattleRain2018$TAVG + SeattleRain2018$TMAX + SeattleRain2018$TMIN + SeattleRain2018$NewWindCat1, data=SeattleRain2018, family=binomial)
summary(PredictRain)
exp(cbind(Odds_Ratio_RainVNoRain=coef(PredictRain), confint(PredictRain)))

PredictRain<-glm(rain~SeattleRain2018$AWND + SeattleRain2018$TAVG + SeattleRain2018$TMAX + SeattleRain2018$TMIN + SeattleRain2018$NewWindCat1 + SeattleRain2018$WSF5, data=SeattleRain2018, family=binomial)
summary(PredictRain)
exp(cbind(Odds_Ratio_RainVNoRain=coef(PredictRain), confint(PredictRain)))
?glm

PredictRain<-glm(rain~SeattleRain2018$AWND + SeattleRain2018$TAVG + SeattleRain2018$TMAX + SeattleRain2018$TMIN + SeattleRain2018$NewWindCat1 + SeattleRain2018$WSF5 + SeattleRain2018$Season, data=SeattleRain2018, family=binomial)
summary(PredictRain)
exp(cbind(Odds_Ratio_RainVNoRain=coef(PredictRain), confint(PredictRain)))

PredictRain1<-glm(rain~SeattleRain2018$AWND + SeattleRain2018$TMAX + SeattleRain2018$TMIN + SeattleRain2018$WSF5, data=SeattleRain2018, family=binomial)
summary(PredictRain1)
PredictRain1

predict(PredictRain,type="response")


rain.res<-residuals.glm(PredictRain)
ggplot(data=SeattleRain2018, aes(x=SeattleRain2018$AWND, y=rain.res))+geom_point()+geom_smooth()
ggplot(data=SeattleRain2018, aes(x=SeattleRain2018$TAVG, y=rain.res))+geom_point()+geom_smooth()
ggplot(data=SeattleRain2018, aes(x=SeattleRain2018$TMAX, y=rain.res))+geom_point()+geom_smooth()
ggplot(data=SeattleRain2018, aes(x=SeattleRain2018$TMIN, y=rain.res))+geom_point()+geom_smooth()
ggplot(data=SeattleRain2018, aes(x=SeattleRain2018$WSF5, y=rain.res))+geom_point()+geom_smooth()
ggplot(data=SeattleRain2018, aes(x=SeattleRain2018$NewWindCat1, y=rain.res))+geom_point()
ggplot(data=SeattleRain2018, aes(x=SeattleRain2018$Season, y=rain.res))+geom_point()+geom_smooth()

