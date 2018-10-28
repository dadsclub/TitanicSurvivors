# Clear Console and Previous Plots
cat("\014")
dev.off(dev.list()["RStudioGD"])


# Import libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)



# Import datasets
assign("train", read.csv("data/train.csv"))


############################## Feature Engineering #################################

# Fixing NA's
train$Age <- replace_na(train$Age, median(na.omit(train$Age)))

# Grabbing Last Name and inserting into new column
train$LastName = gsub(",.+", "", train$Name)

# Grabbing Titles from Names and inserting in new column
train$Title = as.factor(gsub(".*,\\s|\\..*", "", train$Name, perl=T))

# Creating Common Titles List
CommonTitles = c("Miss", "Mrs", "Mr", "Dr", "Master", "Rev")
train$CommonTitle = train$Title
train$CommonTitle[train$Title == "Ms" | train$Title == "Mlle"] = "Miss"
train$CommonTitle[train$Title == "Mme"] = "Mrs"
train$CommonTitle[train$Title == "Sir" | train$Title == "Don"] = "Mr"
train$CommonTitle[(train$Title == "Capt" | train$Title == "Col" |
                    train$Title == "Jonkheer" | train$Title == "Major") & train$Sex == "male"] = "Mr"

train$CommonTitle = factor(x = train$CommonTitle, levels = c(levels(train$CommonTitle), "Miss/Mrs"))
train$CommonTitle[(
  train$Title == "Capt" | train$Title == "Col" | train$Title == "Jonkheer" | train$Title == "Major" |
    train$Title == "Lady" | train$Title == "the Countess") & train$Sex == "female" & train$SibSp == 0] = "Miss"

train$CommonTitle[(
  train$Title == "Capt" | train$Title == "Col" | train$Title == "Jonkheer" | train$Title == "Major" |
    train$Title == "Lady" | train$Title == "the Countess") & train$Sex == "female" & train$SibSp > 0] = "Miss/Mrs"


# Combine SibSp and Parch to compute total family size
train$FamilySize = train$SibSp + train$Parch + 1


# Convert Integer Values (1,0) to Boolean
train$Survived = as.logical(train$Survived)


# Creating Age Categories
AgeBreaks = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 500)
AgeLabels = c("0-1","2-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
              "45-49", "50-54", "55,59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")

# Add a new column that specifies the age group of the person
setDT(train)[, AgeGroups := cut(x=Age, breaks=AgeBreaks, right=TRUE, labels=AgeLabels)]

# Free up memory
remove(AgeBreaks)
remove(AgeLabels)





########################### Plotting Data #######################################


# Stacked Bar Chart of Survival by Gender
ggplot(data = train) +
  geom_bar(mapping = aes(x=Sex, fill=factor(Survived))) +
  labs(title="Survivorship Based on Gender",
       x="Sex",
       y="Number of People")
             
# Boxplot of Fare Split by Survival
ggplot(data = train) +
  geom_boxplot(mapping = aes(x=factor(Survived), y=Fare)) +
  labs(title="Fare Distribution Based on Survival",
       x="Survivorship",
       y="Fare")

# Stacked Bar Graph of Survival based on Age Group
ggplot(data = train) +
  geom_bar(mapping = aes(x=AgeGroups, fill=Survived)) +
  labs(
    title="Survival Based on Age Group",
    x="Age Groups",
    y="Number of People")

# Stacked Bar Graph of Survival based on Age Group, Faceted by Gender
ggplot(data = train) +
  geom_bar(mapping = aes(x=AgeGroups, fill=Survived)) +
  coord_flip() +
  facet_wrap(~Sex) +
  labs(
    title="Survival Based on Age Group and Faceted by Gender",
    y="Number of People",
    x="Age Groups")

# Proportional Stacked Bar Graph of Survival based on Age Group, Faceted by Gender
ggplot(data = train) +
  geom_bar(mapping = aes(x=AgeGroups, fill=Survived), position="fill") +
  coord_flip() +
  facet_wrap(~Sex) +
  labs(
    title="Proportional Survival Based on Age Group and Faceted by Gender",
    y="Proportion of People",
    x="Age Groups")


# Stacked Bar Graph of Survival Based on Embarkment
ggplot(data = train) + 
  geom_bar(mapping=aes(x=Embarked, fill=Survived)) +
  labs(
    title="Survival Based on Embarkment",
    x="Embarkment",
    y="Number of People")


# Stacked Bar Graph of Survival Based on Common Title
ggplot(data = train) +
  geom_bar(mapping=aes(x=CommonTitle, fill=Survived)) +
  labs(
    title="Survival Based on Title",
    x="Title",
    y="Number of People"
  )




# Bar Graph of Survival Based on Family Size
group_by(train, LastName) %>%
  summarise(FamilySize = n(), NumberSurvived = sum(Survived), NumberDied = n() - sum(Survived)) %>%
  group_by(FamilySize) %>%
  summarise(NumberOfFamilies = n(), NumberOfSurvivors = sum(NumberSurvived), 
            NumberDead = sum(NumberDied), PercentageSurvived = 100*sum(NumberSurvived)/(sum(NumberSurvived) + sum(NumberDead))) %>%
    ggplot() +
  geom_bar(mapping=aes(x=FamilySize, y=PercentageSurvived), stat="identity") +
  labs(
    title="Survival Based on Family Size",
    x="Family Size",
    y="Percentage of Family Survived"
  )



