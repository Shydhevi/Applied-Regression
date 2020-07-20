library(readxl)
library(plyr)
library(dplyr)
library(tidyverse)
library(car)
library(zoo)
library(ggplot2)


getwd()
setwd("C:/Users/shydhevi/Documents/R")
ABNYC2019 <- read_excel("C:/Users/shydhevi/Documents/R/AB_NYC_2019.xlsx")

view(ABNYC2019)              #LETS YOU SEE THE DATA
#Descriptive Analysis
summary(ABNYC2019)           # RUNS A SUMMARY ON THE DATA

#NOW WE WANT TO DELETE THE DUPLICATE ID 

mydata <- ABNYC2019 #RENAME DATA TO MYDATA FOR EASIER USE 

# Remove duplicate rows of the dataframe using THE host_id variable and we rename it 
ABNY2019NODUPLICATES <- distinct(mydata,host_id, .keep_all= TRUE)

view(ABNY2019NODUPLICATES)

#NOW WE PULL THE VARIABLES WE NEED FROM THE DATASET 

AIRBNBNYC2019 <- ABNY2019NODUPLICATES %>% select(neighbourhood_group,room_type,price,minimum_nights,number_of_reviews,calculated_host_listings_count,availability_365)
View(AIRBNBNYC2019)
glimpse(AIRBNBNYC2019)

sum(is.na(AIRBNBNYC2019)) 
count(AIRBNBNYC2019)
#Descriptive Analysis
summary(AIRBNBNYC2019)
summary(AIRBNBNYC2019$neighbourhood_group)
#Create multiple scatterplots (5) of each explanatory variables versus price (y)

ggplot(data=AIRBNBNYC2019, aes(x=AIRBNBNYC2019$neighbourhood_group, y=AIRBNBNYC2019$price)) + geom_point(size=2)
ggplot(data=AIRBNBNYC2019, aes(x=AIRBNBNYC2019$room_type, y=AIRBNBNYC2019$price)) + geom_point(size=2)
ggplot(data=AIRBNBNYC2019, aes(x=AIRBNBNYC2019$minimum_nights, y=AIRBNBNYC2019$price)) + geom_point(size=2)
ggplot(data=AIRBNBNYC2019, aes(x=AIRBNBNYC2019$number_of_reviews, y=AIRBNBNYC2019$price)) + geom_point(size=2)
ggplot(data=AIRBNBNYC2019, aes(x=AIRBNBNYC2019$calculated_host_listings_count, y=AIRBNBNYC2019$price)) + geom_point(size=2)
ggplot(data=AIRBNBNYC2019, aes(x=AIRBNBNYC2019$availability_365 , y=AIRBNBNYC2019$price)) + geom_point(size=2)

#Histograms

hist(AIRBNBNYC2019$minimum_nights, main = "Normal DIstribution")
hist(AIRBNBNYC2019$number_of_reviews, main = "Normal DIstribution")
hist(AIRBNBNYC2019$availability_365, main = "Normal DIstribution")

#Regression with price as the dependent variable and all other variables(Before Transformation)

Airbnbnyclm<-lm(price~`neighbourhood_group`+`room_type`+`minimum_nights`+`number_of_reviews`+`calculated_host_listings_count`+`availability_365`, data=AIRBNBNYC2019)
summary(Airbnbnyclm)

#Regression without CAlculated host listings
Airbnbnyclm<-lm(price~`neighbourhood_group`+`room_type`+`minimum_nights`+`number_of_reviews`+`availability_365`, data=AIRBNBNYC2019)
summary(Airbnbnyclm)

#Create a linear model for neighbourhood_group
LinearModelNG <- lm(AIRBNBNYC2019$'price'~ AIRBNBNYC2019$'neighbourhood_group')
summary(LinearModelNG)
ggplot(AIRBNBNYC2019, aes(x =AIRBNBNYC2019$'neighbourhood_group', y =AIRBNBNYC2019$'price')) +geom_point() +stat_smooth(method = "lm", col = "blue")

#Create a linear model for roomtype
LinearModelRT <- lm(AIRBNBNYC2019$'price'~ AIRBNBNYC2019$'room_type')
summary(LinearModelRT)
ggplot(AIRBNBNYC2019, aes(x =AIRBNBNYC2019$'room_type', y =AIRBNBNYC2019$'price')) +geom_point() +stat_smooth(method = "lm", col = "blue")

#Create a linear model for availability_365
LinearModelAV <- lm(AIRBNBNYC2019$'price'~ AIRBNBNYC2019$'availability_365')
summary(LinearModelAV)
ggplot(AIRBNBNYC2019, aes(x =AIRBNBNYC2019$'availability_365', y =AIRBNBNYC2019$'price')) +geom_point() +stat_smooth(method = "lm", col = "blue")

#Create a linear model for minimum_nights
LinearModelMN <- lm(AIRBNBNYC2019$'price'~ AIRBNBNYC2019$'minimum_nights')
summary(LinearModelMN)
ggplot(AIRBNBNYC2019, aes(x =AIRBNBNYC2019$'minimum_nights', y =AIRBNBNYC2019$'price')) +geom_point() +stat_smooth(method = "lm", col = "blue")


# Histogram of Availability_365
ggplot(data=AIRBNBNYC2019,aes(x = AIRBNBNYC2019$availability_365), y=AIRBNBNYC2019$price) +
  geom_histogram(binwidth=1, color='black', fill = "#F79420") +
   xlab("Availability") +
  ylab("Price")

###PLOTS FOR DATA ANALYSIS###

#Histogram for Number of property in neigghborhood.
ggplot(x=AIRBNBNYC2019$neighbourhood_group) + geom_histogram(aes(AIRBNBNYC2019$neighbourhood_group, fill = AIRBNBNYC2019$neighbourhood_group), stat = "count",alpha = 0.85) + 
  theme_minimal(base_size=13) + xlab("") + ylab("") +theme(legend.position="none") +
  ggtitle("The Number of Property in Each Area")

#Proportion of RoomType in Area

ggplot(x=AIRBNBNYC2019$room_type) + geom_histogram(aes(AIRBNBNYC2019$neighbourhood_group, fill = AIRBNBNYC2019$room_type), stat = "count",alpha = 0.85, position = 'fill') + 
  theme_minimal(base_size=13) + xlab("") + ylab("")  + 
  ggtitle("The Proportion of Room Type in Each Area")

# room_type / price
AIRBNBNYC2019 %>% 
  ggplot(aes(room_type, price, fill = room_type)) + 
  geom_boxplot(alpha = 0.5) + 
  labs(x = 'Room_Type', y = 'Price') + 
  guides(fill = F) + 
  scale_y_log10()

# neighnorhood/ price
AIRBNBNYC2019 %>% 
  ggplot(aes(neighbourhood_group, price, fill = neighbourhood_group)) + 
  geom_boxplot(alpha = 0.5) + 
  labs(x = 'Neighbourhood_group', y = 'Price') + 
  guides(fill = F) + 
  scale_y_log10()

# minimum_nights / roomtype
AIRBNBNYC2019 %>% 
  ggplot(aes(room_type, minimum_nights, fill = room_type)) + 
  geom_boxplot(alpha = 0.5) + 
  labs(x = 'room_type', y = 'Minimum_Nights') + 
  guides(fill = F) + 
  scale_y_log10()


# Reviews / neighborhood
AIRBNBNYC2019 %>% 
  ggplot(aes(neighbourhood_group,number_of_reviews,  fill = neighbourhood_group)) + 
  geom_boxplot(alpha = 0.5) + 
  labs(x = 'Neighbourhood_group', y = 'Number_of_Reviews') + 
  guides(fill = F) + 
  scale_y_log10()


# neighbourhood_group / price / room_type
AIRBNBNYC2019 %>% 
  group_by(room_type, neighbourhood_group) %>% 
  summarise(median_price = median(price)) %>% 
  ggplot(aes(neighbourhood_group, median_price)) + 
  geom_bar(aes(fill = neighbourhood_group), stat = 'identity', colour = 'black') + 
  geom_text(aes(label = median_price), vjust = -0.5) + 
  guides(fill = F) + 
  labs(x = 'Neighbourhood_Group', y = 'Median of Price') + 
  facet_wrap(~ room_type)



#Categorizing Neighborhood_Group

AIRBNBNYC2019$NeighborhoodCat<-factor(AIRBNBNYC2019$neighbourhood_group,levels=c('Manhattan','Brooklyn','Queens','Staten Island','Bronx'))

summary(AIRBNBNYC2019$NeighborhoodCat)  

#count the values in the each category#

LocationCount <- as.data.frame(table(AIRBNBNYC2019$neighbourhood_group))
View(LocationCount)



#Interpret the coefficients, including the categorical variables

CatFit1<-lm(price~NeighborhoodCat+`room_type`+`minimum_nights`+`number_of_reviews`+`calculated_host_listings_count`+`availability_365`, data=AIRBNBNYC2019)
summary(CatFit1)
#Run and report on a VIF regression
vif(CatFit1)

###Swarnima-edits###
#Regression with price as the dependent variable and all other variables(Before Transformation)
Airbnbnyclm<-lm(price~`neighbourhood_group`+`room_type`+`minimum_nights`+`number_of_reviews`+`calculated_host_listings_count`+`availability_365`, data=AIRBNBNYC2019)
summary(Airbnbnyclm)

#histogram
hist(AIRBNBNYC2019$`number_of_reviews`)
hist(AIRBNBNYC2019$`availability_365`)

#categorizing Number_of_reviews
AIRBNBNYC2019$NumberOfReviews<-cut(AIRBNBNYC2019$`number_of_reviews`, c(0,100,200,300,400,500,600,Inf))
View(AIRBNBNYC2019)

#categorizing room_type
AIRBNBNYC2019$RoomType<-factor(AIRBNBNYC2019$`room_type`)
RoomTypeCount <- as.data.frame(table(AIRBNBNYC2019$RoomType))
summary(AIRBNBNYC2019$RoomType)
summary(AIRBNBNYC2019$price)
aggregate(price ~ RoomType, AIRBNBNYC2019, mean)
AIRBNBNYC2019$RoomType<-factor(AIRBNBNYC2019$RoomType,levels=c('Entire home/apt','Private room','Shared room'))
RoomTypeCount <- as.data.frame(table(AIRBNBNYC2019$RoomType))
View(RoomTypeCount)
View(AIRBNBNYC2019)

#categorizing neighbourhood_group
AIRBNBNYC2019$NeighGrp<-factor(AIRBNBNYC2019$`neighbourhood_group`)
NeighGrpCount <- as.data.frame(table(AIRBNBNYC2019$NeighGrp))
summary(AIRBNBNYC2019$NeighGrp)
summary(AIRBNBNYC2019$price)
aggregate(price ~ NeighGrp, AIRBNBNYC2019, mean)
AIRBNBNYC2019$NeighGrp<-factor(AIRBNBNYC2019$NeighGrp,levels=c('Manhattan','Staten Island','Brooklyn', 'Queens','Bronx'))
NeighGrpCount <- as.data.frame(table(AIRBNBNYC2019$NeighGrp))
View(NeighGrpCount)
View(AIRBNBNYC2019)

#Regression with price as the dependent variable and all other variables(after categorization)
Airbnbnyclm1<-lm(price~NeighGrp+RoomType+`minimum_nights`+NumberOfReviews+`calculated_host_listings_count`+`availability_365`, data=AIRBNBNYC2019)
summary(Airbnbnyclm)

AirbnbNYC.res<-residuals.lm(Airbnbnyclm1)
View(AirbnbNYC.res)
ggplot(data=AIRBNBNYC2019, aes(x=AIRBNBNYC2019$neighbourhood_group, y=AirbnbNYC.res)) + geom_point()
