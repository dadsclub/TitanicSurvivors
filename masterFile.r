library(tidyverse)
library(caret)


set.seed(420)
train <- read.csv("train.csv", stringsAsFactors=FALSE)
test  <- read.csv("test.csv",  stringsAsFactors=FALSE)


# EDA ---------------------------------------------------------------------
ggplot(train, aes(Age, fill = factor(Survived))) + 
  geom_histogram(bins=30) + 
  xlab("Age") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Age vs Survived")

ggplot(train, aes(Sex, fill = factor(Survived))) + 
  geom_bar(stat = "count", position = 'dodge')+
  xlab("Sex") +
  ylab("Count") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Sex vs Survived")

ggplot(train, aes(Age, fill = factor(Survived))) + 
  geom_histogram(bins=30) + 
  xlab("Age") +
  ylab("Count") +
  facet_grid(.~Sex)+
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Age vs Sex vs Survived")

ggplot(train, aes(Pclass, fill = factor(Survived))) + 
  geom_bar(stat = "count")+
  xlab("Pclass") +
  facet_grid(.~Sex)+
  ylab("Count") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Pclass vs Sex vs Survived")

ggplot(train, aes(x = Age, y = Sex)) + 
  geom_jitter(aes(colour = factor(Survived))) + 
  facet_wrap(~Pclass) + 
  labs(x = "Age", y = "Sex", title = "Pclass vs Sex vs Age vs Survived")+
  scale_fill_discrete(name = "Survived") + 
  scale_x_continuous(name="Age",limits=c(0, 81))

ggplot(train, aes(x = Fare, y = Pclass)) + 
  geom_jitter(aes(colour = factor(Survived))) + 
  labs(x = "Age", y = "Pclass", title = "Fare vs Pclass")+
  scale_fill_discrete(name = "Survived") + 
  scale_x_continuous(name="Fare", limits=c(0, 270), breaks=c(0, 40, 80, 120, 160, 200, 240, 280))

ggplot(train, aes(Pclass, fill = factor(Survived))) + 
  geom_bar(stat = "count")+
  xlab("Pclass") +
  ylab("Count") +
  facet_wrap(~Embarked) + 
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Embarked vs Pclass vs Survived")


# Function for Wrangling Data ---------------------------------------------
features <- c("Pclass",
              "Age",
              "Sex",
              "Parch",
              "SibSp",
              "Fare",
              "Embarked",
              "Survived")
extractfeatures <- function(data) {
  fea <- data[,features]
  fea$Age[is.na(fea$Age)] <- 28
  fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
  fea$Embarked[fea$Embarked==""] = "S"
  fea$Sex      <- as.factor(fea$Sex)
  fea$Embarked <- as.factor(fea$Embarked)
  fea$Survived <- as.factor(fea$Survived)
  return(fea)
}
features2 <- c("Pclass",
              "Age",
              "Sex",
              "Parch",
              "SibSp",
              "Fare",
              "Embarked")
extractfeatures2 <- function(data) {
  fea <- data[,features2]
  fea$Age[is.na(fea$Age)] <- 28
  fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
  fea$Embarked[fea$Embarked==""] = "S"
  fea$Sex      <- as.factor(fea$Sex)
  fea$Embarked <- as.factor(fea$Embarked)
  return(fea)
}


# Building Model ----------------------------------------------------------

rf <- train(Survived ~ ., data = extractfeatures(train), method = "rf", ntree=1000)
#rf <- randomForest(extractfeatures(train), as.factor(train$Survived), ntree=100, importance=TRUE)

predictions <- data.frame(PassengerId = test$PassengerId)
predictions$Survived <- predict(rf, extractfeatures2(test))


# Attaining Accuracy ------------------------------------------------------

key <- read_csv("gender_submission.csv")
key$pred <- predictions$Survived
key$answer <- as.factor(case_when(key$Survived == key$pred ~ "Right",key$Survived != key$pred ~ "Wrong"))
summary(key$answer)

