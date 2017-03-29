####Setting up R####

#packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(datasets) # load library with R datasets
library(ez)
library(lmSupport)
library(MASS)
library(ISLR)
library(foreign)
library(haven)
library(Hmisc)
library(XML)
library(grDevices)

#clear workspace
rm(list=ls())


#set working directory
setwd("C:/Users/Richard/Desktop/R_Thesis")
getwd()

list.files()

####Read in CSV and clean up obs types####

data <- read.csv("COMBINED.csv")

str(data)

data$RT_Adj <- as.numeric(as.character(data$RT_Adj))

data$Cond <- as.character(data$Cond)

data$PID <- as.numeric(as.character(data$PID))

data$Trial_Type <- as.factor(as.character(data$Trial_Type))

AdjData <- data %>% filter(RT_Adj<2500)

SPSSData <- data %>% filter(RT_Adj<9000)

reg_data$Cond <- as.factor(as.character(reg_data$Cond))
reg_data$Trial_Type <- as.factor(as.character(reg_data$Trial_Type))

####Plots####


ggplot(data = AdjData, aes(RT_Adj)) + geom_histogram(binwidth = 10)+
  facet_grid(.~Cond)

ggplot(data = AdjData, aes(x = PID, y = RT_Adj))+
  geom_boxplot()+
  facet_grid(.~Cond)


#boxplots
ggplot(data = AdjData, aes(x = Cond, y = RT_Adj, color = Trial_Type)) +
  geom_boxplot ()+
  scale_y_continuous(name = "Reaction Time (Milliseconds)",
                     breaks = seq(0, 20, 5),
                     limits=c(0, 20))+
  scale_x_discrete(name = "Visual Angle Distance")+
  theme_bw() +
  theme(panel.grid.minor = element_blank())+
  ggtitle("Task Relatedness and Visual Angle Distance on Reaction Time ")+
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom")+
  labs(fill = "Task Relatedness")+
  theme(plot.title = element_text(hjust = .5))#centered title!!!



####Linear Regression####


lm.fit <- lm(RT_Adj~Cond, data = AdjData) 
summary(lm.fit)


#Shows the line of best fit explained by the regression
attach(AdjData)
plot(Cond, RT_Adj)
abline(nlm.fit, lwd=5, col = "red")
?abline


####  Multiple Regression ####
#In order to make this section a little simpler to handle, I'm going to
#create a subset dataframe with just the columns I want (PID, RT_Adj, Cond, Trial_Type,
#Trial)

#subset of columns
reg_data <- SPSSData[,c("PID", "RT_Adj", "Cond", "Trial_Type", "Trial")]
print(head(reg_data))

#I got an error "contrasts can be applied only to factors with 2 or more levels" so
#I wrote this code to fix the dependent variables and make sure they are factors.

#make them factors
reg_data$Cond <- as.factor(reg_data$Cond)
reg_data$Trial_Type <- as.factor(reg_data$Trial_Type)
reg_data$RT_Adj <- as.numeric(reg_data$RT_Adj)

#I'm still getting that error so I checked stack overflow for somebody having a similar problem
#     "Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
#     contrasts can be applied only to factors with 2 or more levels"

#Here is the answer I found
#     http://stackoverflow.com/questions/18171246/error-in-contrasts-when-defining-a-linear-model-in-r

#the answer contained some diagnostic code to figure out where the problem lies.

#see whether variables are factors
l <- sapply(reg_data, function(x) is.factor(x))

#put all factors in a dataframe "m"
m <- reg_data[,1]

#check factors to see if they have only 1 level
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")

#ok, so that didn't work. Let me try assigning levels myself to the variables...

AdjData$Cond <- factor(AdjData$Cond,
                    levels = c(1,2,3,4,5),
                    labels = c("22.5", "45", "67.5","90","112.5"))
AdjData$Trial_Type <- factor(AdjData$Trial_Type,
                        levels = c(1,2),
                        labels = c("Related", "Independent"))


#nope, still getting the error message

####The Actual Regression####

#create the regression model

#code for regression equation
RT_Reg <- lm(RT_Adj ~ Trial_Type + Cond, data = AdjData)

#show the regression model
print(RT_Reg)

a <-coef(RT_Reg)
print(a)

# Show the model.
print(RT_Reg)

# Get the Intercept and coefficients as vector elements.
cat("584.31","290.51670","36.83306")

#Makes coefficient labels and columns for the different types of coefficients
Intercept <- coef(RT_Reg)[1]
print(a)

XTrial_Type <- coef(RT_Reg)[2]
XCondition <- coef(RT_Reg)[3]


print(Xdisp)
print(Xhp)

#ok, so let's test it out on a few different combinations to see how it works...

#I know that from the test data, the mean Reaction Time when inputs were independent(1)
#and distance condition(2) was 1111.4 milliseconds with Std Dev = 235.9
Y = 584.31 + (1) * 290.52 + (2) * 36.8336
#Y = 948.4972
#Not too bad, definitely within one standard deviation of the predicted value.

#I know that from the test data, the mean Reaction Time when inputs were Related (0)
#and distance condition was farther (5) was 786.7 milliseconds with Std Dev = 325.7
Y = 584.31 + (0) * 290.52+(5) * 36.8336
#Y = 768.478
#Wow, it's actually doing pretty well, let's try one more.

#I know that from the test data, the mean Reaction Time when inputs were Related (0)
#and distance condition was farther (4) was 819.2 milliseconds with Std Dev = 284.0
Y = 584.31 + (0) * 290.52 + (4) * 36.8336
#Y = 731.6
#Not too bad, definitely within one standard deviation of the predicted value.

####Plot Means####

read.csv("Adjusted.csv")

AdjData$Cond < as.integer(AdjData$Cond)


ggplot(data = AdjData, aes(x= Cond, y = RT_Adj, color = Trial_Type))+
  geom_smooth(method = lm) + geom_boxplot()+
scale_y_continuous(name = "Reaction Time (Milliseconds)",
                   breaks = seq(0, 20, 5),
                   limits=c(0, 20))+
  scale_x_discrete(name = "Visual Angle Distance")+
  theme_bw() +
  theme(panel.grid.minor = element_blank())+
  ggtitle("Task Relatedness and Visual Angle Distance on Reaction Time ")+
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom")+
  labs(fill = "Task Relatedness")+
  theme(plot.title = element_text(hjust = .5))#centered title!!!
       