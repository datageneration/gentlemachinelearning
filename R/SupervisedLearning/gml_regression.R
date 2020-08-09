## Gentle Machine Learning
## Linear regression
## Adapted from examples in James, G., Witten, D., Hastie, T., and Tibshirani, R. 2013.
## An Introduction to Statistical Learning with applications in R, New York: Springer

# Boston dataset: 
# crim: per capita crime rate by town.
# zn: proportion of residential land zoned for lots over 25,000 sq.ft.
# indus: proportion of non-retail business acres per town.
# chas: Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# nox: nitrogen oxides concentration (parts per 10 million).
# rm: average number of rooms per dwelling.
# age: proportion of owner-occupied units built prior to 1940.
# dis: weighted mean of distances to five Boston employment centres.
# rad: index of accessibility to radial highways.
# tax: full-value property-tax rate per \$10,000.
# ptratio: pupil-teacher ratio by town.
# black: 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
# lstat: lower status of the population (percent).
# medv: median value of owner-occupied homes in \$1000s.

library(MASS)
library(ISLR)
library(tidyverse)

# Simple Linear Regression
data(Boston)
names(Boston)
lm.fit=lm(medv~lstat,data=Boston)
attach(Boston)
lm.fit=lm(medv~lstat)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

# Prediction intervals will always be wider than confidence intervals 
# because they account for the uncertainty associated with epsilon, the irreducible error.
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")
plot(lstat,medv,  col = "forestgreen")
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv, pch = 20, col="red")
par(mfrow=c(2,2))
plot(lm.fit, pch=20)
plot(predict(lm.fit), residuals(lm.fit), pch=20, col = "red")
plot(predict(lm.fit), rstudent(lm.fit), pch=20, col = "forestgreen")
plot(hatvalues(lm.fit), pch=20, col = "darkblue")
which.max(hatvalues(lm.fit))

# Multiple Linear Regression

lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)
library(car)
vif(lm.fit)
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-age)

# Interaction Terms

summary(lm(medv~lstat*age,data=Boston))

# Non-linear Transformations of the Predictors
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)
summary(lm(medv~log(rm),data=Boston))

# Advertising example: Model fit
# Advertising dataset: Advertising on TV, Radio and newspaper and sales on a product.
# All units in thousands

advertising=read_csv("https://raw.githubusercontent.com/datageneration/gentlemachinelearning/master/data/Advertising.csv") %>%
  select(c(TV, radio, newspaper,sales))
lm.adv1=lm(sales~.,data=advertising)
lm.adv2=lm(sales~.-newspaper,data=advertising)
summary(lm.adv1)
summary(lm.adv2)
anova(lm.adv1,lm.adv2)


# Carseats example: interaction terms
# Carseats datasets: simulated data set containing sales of child car seats at 400 different stores.
# Sales: Unit sales (in thousands) at each location
# CompPrice: Price charged by competitor at each location: 
# Income: Community income level (in thousands of dollars)
# Advertising: Local advertising budget for company at each location (in thousands of dollars): 
# Population: Population size in region (in thousands)
# Price: Price company charges for car seats at each site: 
# ShelveLoc: A factor with levels Bad, Good and Medium indicating the quality of the shelving location for the car seats at each site
# Age: Average age of the local population
# Education: Education level at each location
# Urban: A factor with levels No and Yes to indicate whether the store is in an urban or rural location
# US: A factor with levels No and Yes to indicate whether the store is in the US or not# 

names(Carseats)

# Regression with all variables plus interaction terms
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc)


