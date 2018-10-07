setwd("~/Titanic")
library(tidyverse)
library(data.table)


# train Import -------------------------------------------------------------

train <- read.csv("train.csv")
train$Age %>%  head(10)

train$Survived %>% sum() #Number of survivors 
nrow(train) - (train$Survived %>% sum()) #Number of unlucky bastards


# Handling NA's -----------------------------------------------------------
#There are some missing values for Age. We will do a dirty train replacement by replacing those values with the median Age
train$Age %>% na.omit() %>%  median() # NA's are like an STD one NA in an entire vector will result in all the values being NA
train$Age <- train$Age %>% replace_na(28)

# Wrangling ---------------------------------------------------------------
#train <- train %>% filter(Age <= 110) #some 400 year old on this boat. 

#Creating age categories
Agebreaks <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,500)
Agelabels <- c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
               "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
               "70-74","75-79","80-84","85+")

setDT(train)[ , Agegroups := cut(Age, 
                                breaks = Agebreaks, 
                                right = FALSE, 
                                labels = Agelabels)]
remove(Agebreaks)
remove(Agelabels)
# EDA ---------------------------------------------------------------------

train %>% filter(Survived == 1) %>% ggplot() + geom_bar(aes(Agegroups)) + ggtitle("Age of Survivors") # Survivors by Age
train %>% filter(Survived == 0) %>% ggplot() + geom_bar(aes(Agegroups)) + ggtitle("Age of Victims") # Victims by Age
train %>% ggplot(aes(Embarked, Pclass, fill = Survived, alpha=Survived)) + geom_tile() #Embarked and Pclass on survivorship

