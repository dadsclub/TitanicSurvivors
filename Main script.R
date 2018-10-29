

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readr)
library(caret)

# Importing data ----------------------------------------------------------
testset <- read_csv("all/test.csv")
trainingset <- read_csv("all/train.csv")

validation <- read_csv("all/gender_submission.csv")
##Note that both sets are being binded so that feature engineering can be done to both
fullset <- bind_rows(trainingset, testset)

set.seed(555)
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


##setting them to 0s and 1s for later
fullset$Mother <-  0
fullset$Mother[fullset$Sex == "female" & fullset$Parch > 0 & fullset$Age > 18 & fullset$Title != "Miss"] <- 1

##I am going to do the same with mothers in the data
table(fullset$Mother, fullset$Survived)


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
testset <- fullset[892:1309,]
trainingset <- fullset[1:891,]


##how about some correlations?
  ##Is there a correlation between being a mother and surviving?

# Model Training/Testing --------------------------------------------------

##for the first model fitting, I am going to use a Multiple Logistic Regression model
##removed unneeded variables 
glm.fit = glm(Survived ~ Pclass + Age + Sex, 
              family = binomial,
              data = trainingset)
summary(glm.fit)

##So we find that some variables are not significant to the data: Fare, Mother, Parch, and Child. So we will remove
##them and retrain the model


##finding the probabilities that people survived
glm.probs = predict(glm.fit, type ="response")

##Create vectors of class predictions based on whether the predicted probability of survival is 
##greater than or less than 0.5
glm.pred = rep("Died", 891)
glm.pred[glm.probs>.5] = "Survived"

##Generating a table showing what our model predicted versus the actual results of the TRAINING SET
table(glm.pred, trainingset$Survived)



##im going to use the caret package to build a random forest model
 # <- train(Survived ~Pclass + Age + Sex, 
 #                  data = trainingset, 
 #                  method = "lm")


# Test Data modeling ------------------------------------------------------

## testing the model on the test set 
glm.probs_test = predict(glm.fit, testset, type ="response")

glm.pred_test = rep("Died", 418)
glm.pred_test[glm.probs2>.5] = "Survived"


