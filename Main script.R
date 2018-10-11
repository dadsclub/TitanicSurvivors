

# Libraries ---------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(mice)
library(readr)
library(scales)
library(ggthemes)
library(caret)
library(classifierplots)

# Importing data ----------------------------------------------------------
testset <- read_csv("all/test.csv")
trainingset <- read_csv("all/train.csv")

##Note that both sets are being binded so that feature engineering can be done to both
fullset <- bind_rows(testset, trainingset)


# Feature Engineering  ----------------------------------------------------

##grabbing the title from passenger names using Regexps
fullset$Title <- gsub('(.*, )|(\\..*)', '', fullset$Name)

##here is a table that will display the passengers titles as well as their Sex
table(fullset$Sex, fullset$Title)

##some titles are too rare to give them their own columns
rare_titles <- c('Capt', 'Col', 'Don', 'Dona', 'Jonkheer', 'Dr', 'Lady', 
                 'Major', 'Sir', 'the Countess', 'Rev')

##taking the more extravagant versions of "Miss" and "Mrs" and converting them
fullset$Title[fullset$Title == 'Mlle']  <- 'Miss'
fullset$Title[fullset$Title == 'Mme']   <- 'Mrs'
fullset$Title[fullset$Title == 'Ms']    <- 'Miss'

##establishing a new list using the rare titles listed above
fullset$Title[fullset$Title %in% rare_titles] <- 'Rare Title'

table(fullset$Sex, fullset$Title)



# Feature Engineering Pt. 2 -----------------------------------------------

###I am going to create a different designation/column in the data based on 
###whether or not a passenger is a child or not

##there are NA values for age so lets change that first
fullset$Age %>% 
  na.omit() %>% 
  median()
#results in the value 28 so we are going to replace NAs with that
fullset$Age <- fullset$Age %>% 
  replace_na(28)

fullset$Child[fullset$Age < 18] <- 'Child'
fullset$Child[fullset$Age >= 18] <- 'Adult'

table(fullset$Child, fullset$Survived)

##I am going to do the same with mothers in the data
fullset$Mother <-  'Not Mother'
fullset$Mother[fullset$Sex == "female" & fullset$Parch > 0 & fullset$Age > 18 & fullset$Title != "Miss"] <- 'Mother'


table(fullset$Mother, fullset$Survived)

##setting them to 0s and 1s for later
fullset$Mother <-  0
fullset$Mother[fullset$Sex == "female" & fullset$Parch > 0 & fullset$Age > 18 & fullset$Title != "Miss"] <- 1
# Graphics/Visualisations -------------------------------------------------

##What is the relationship between embark location and survival?
embark_survive <- trainingset %>%
  filter(PassengerId != 62 & PassengerId != 830)

ggplot(embark_survive, aes(x = Embarked, fill = factor(Survived))) +
  geom_bar()+
  ggtitle("Embarked count factoring survival")

##How about embarkment fare and survival? 
embark_fares <- trainingset %>% 
  filter(PassengerId != 62 & PassengerId != 830 & Fare < 300)

ggplot(embark_fares, aes(x = Embarked, y = Fare, fill = factor(Survived))) + 
  geom_boxplot() +
  ggtitle("Survivor Embarkment/Fare boxplot")
  

##median travelling fare
mean(trainingset$Fare)

# Fun Statistics stuff ----------------------------------------------------
trainingset <- fullset[892:1309,]
testset <- testset[1:891,]

##how about some correlations?
  ##Is there a correlation between being a mother and surviving?


