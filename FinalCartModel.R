library(tidyverse)
library(leaps)
library(MASS)
library(corrgram)
library(glmnet)
library(boot)
library(rpart)
library(rpart.plot)
library(reshape2) # To reshape data

BostonHousing <- read.csv("C:/Users/shydhevi/Documents/R/AdvancedRegression/BostonHousing.csv")
#Glimpse of the dataset
glimpse(BostonHousing)
summary(BostonHousing)
#Exploratory Analysis
ggplot(gather(BostonHousing), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')+
  theme_gray()+
  ggtitle("Histogram of all variables")

BostonHousing %>% gather(key, val, -MEDV) %>%
  ggplot(aes(x = val, y = MEDV)) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE, col = "blue") +
  facet_wrap(~key, scales = "free") +
  theme_gray() +
  ggtitle("Scatter plot of dependent variables vs Median Value (MEDV)")

#Box Plot
boxplot(BostonHousing, col = "red")

#Correlation Plots
corrgram(BostonHousing, upper.panel = NULL, lower.panel = panel.cor)

#Density plot of Median Value of house prices

ggplot(BostonHousing, aes(x = MEDV)) +
  stat_density() +
  labs(x = "Median Value x $1000", y = "Density", title = "Density Plot of Median Value House Price") +
  theme_minimal()
#Attributes vs. Median house pricing
#We will now plot a few attributes against the house price.

selected.attributes = dplyr::select(BostonHousing, c(CRIM, RM, AGE, RAD, TAX, LSTAT, MEDV))
melted = melt(selected.attributes,id="MEDV")
ggplot(melted, aes(x = value, y = MEDV, colour = variable)) +
  geom_point(alpha = 0.7) +
  geom_smooth(color='black') +
  facet_wrap(~variable, scales = "free", ncol = 2) +  # Divide the plots based on variable in 2 columns with a free scale that adjusts
  labs(x = "Variable Value", y = "Median House Price x $1000") +
  theme_minimal()
# Splitting the data 

sapply(BostonHousing, function(x)sum(is.na(x)))
smp_siz = floor(0.50*nrow(BostonHousing))
smp_siz
train.index <- sample(seq_len(nrow(BostonHousing)),size = smp_siz)
BostonHousing_train <- BostonHousing[train.index, ]
nrow(BostonHousing_train)
BostonHousing_test <- BostonHousing[-train.index, ]
nrow(BostonHousing_test)
#Linear Regression
boston.lm <- lm(MEDV ~. , data = BostonHousing_train)
mean(boston.lm$residuals^2)
summary(boston.lm)

bostonhousing.rpart1 <- rpart(formula = MEDV~. , data = BostonHousing_train)

rpart.plot(bostonhousing.rpart1,type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE,varlen = 0, cex = 0.6)
#prp(bostonhousing.rpart1, type = 5, extra = 101, leaf.round = 1, fallen.leaves = TRUE,
#    varlen = 0, cex = 0.6)
prp(bostonhousing.rpart1, type = 5, extra = 1, split.font = 1, varlen = -15,cex=0.6)

library(rattle)
fancyRpartPlot(bostonhousing.rpart1, cex=0.6)

plotcp(bostonhousing.rpart1)

predict.tree1 <- predict(bostonhousing.rpart1, BostonHousing_test)
library(forecast)
accuracy(predict.tree1, BostonHousing_test$MEDV)

mse.tree <- mean((predict.tree1- BostonHousing_test$MEDV)^2)
mse.tree

#cp = 0.013

bostonhousing.rpart2 <- rpart(formula = MEDV ~. , data = BostonHousing_train, cp= 0.013)
rpart.plot(bostonhousing.rpart2,type = 3, box.palette = c("red", "grey"), fallen.leaves = TRUE,varlen = 0, cex = 0.6)


plotcp(bostonhousing.rpart2)

printcp(bostonhousing.rpart2)
predict.tree2 <- predict(bostonhousing.rpart2, BostonHousing_test)
library(forecast)
accuracy(predict.tree2, BostonHousing_test$MEDV)

mse.tree2 <- mean((predict.tree2- BostonHousing_test$MEDV)^2)
mse.tree2

#cp = 0.015

bostonhousing.rpart3<- rpart(formula = MEDV ~. , data = BostonHousing_train, cp= 0.015)
rpart.plot(bostonhousing.rpart3,type = 3, box.palette = c("red", "grey"), fallen.leaves = TRUE,varlen = 0, cex = 0.5)


plotcp(bostonhousing.rpart3)

printcp(bostonhousing.rpart3)
predict.tree3 <- predict(bostonhousing.rpart3, BostonHousing_test)
library(forecast)
accuracy(predict.tree3, BostonHousing_test$MEDV)

mse.tree3 <- mean((predict.tree3- BostonHousing_test$MEDV)^2)
mse.tree3

