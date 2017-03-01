#### Linear Regression Blog Post ####

#This code is an attempt by a novice R user at performing 
#simple linear regressions and multiple regression using the 
#USArrests dataset from R. The goal here is to see if murder rate
#and assult rate are good predictors of urban population.

#Packages and Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(datasets) # load library with R datasets
library(ez)
library(lmSupport)
library(MASS)
library(ISLR)


#Set Working Directory

("E:/Blogpost1")
getwd()

#### Exploring the Dataset ####

#look at data
View (USArrests)

#descriptive statistics
summary(USArrests)

#shows plots of comparisons between the different columns
require(graphics)
pairs(USArrests, panel = panel.smooth, main = "USArrests data")#

#### This dataset apparently has an error. the following code fixes it####

## Difference between 'USArrests' and its correction
USArrests["Maryland", "UrbanPop"] # 67 -- the transcription error
UA.C <- USArrests
UA.C["Maryland", "UrbanPop"] <- 76.6

## also +/- 0.5 to restore the original  <n>.5  percentages
s5u <- c("Colorado", "Florida", "Mississippi", "Wyoming")
s5d <- c("Nebraska", "Pennsylvania")
UA.C[s5u, "UrbanPop"] <- UA.C[s5u, "UrbanPop"] + 0.5
UA.C[s5d, "UrbanPop"] <- UA.C[s5d, "UrbanPop"] - 0.5

## ==> UA.C  is now a *C*orrected version of  USArrests

####Murder as a predictor for %urban population Simple Linear Regression####

#Following the error, the new dataset assigned object name UA.C

#View Corrected
View(UA.C)

#plot Murder as input, urbanpop as output
#this does not look like a linear relationship
attach(UA.C)
scatterplot(Murder,UrbanPop)
scatterplot(Assault,UrbanPop)


#this fits a simple linear regression model using Murder as a predictor
#for Urban Population percentage. 
#Note: the following 2 lines might be redundant
lm.fit <- lm(UrbanPop~Murder, data = UA.C) 
attach(UA.C)
lm.fit <- lm(UrbanPop~Murder) 

#this calls up the formula "lm.fit" with basic coefficients.
lm.fit

#This gives more detailed information about the function lm.fit
summary(lm.fit)

#here I decided to repeat the regression with Assault and found it might have
#a more linear relationship with Urban population %
lm.Assualt <- lm(UrbanPop~Assault)

lm.Assualt
summary(lm.Assualt)

#see what else is stored in lm.Assault
names(lm.Assualt)

#see the coefficients 
coef(lm.Assualt)

#see the confidence intervals
confint(lm.Assualt)

#shows predicted values for the urban pop for different numbers of Assault
predict(lm.Assualt, data.frame(Murder = c(20,30,40)), interval = "confidence")

#Shows the line of best fit explained by the regression
plot(Assault, UrbanPop)
abline(lm.Assualt, lwd=3, col = "red") #makes it red and 3 width

#plots predicted values by residuals
plot(predict(lm.Assualt), residuals(lm.Assualt))

####Multiple Linear Regression####

#Uses multiple regression with Rape, Murder, Assault as predictors for Urbanpop

#performs multiple regression
lm.MultReg <- lm(UrbanPop~., Murder, data = UA.C)

#shows coefficients for each
lm.MultReg

#shows the results of the regression
summary(lm.MultReg)
