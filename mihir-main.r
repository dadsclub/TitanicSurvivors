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




# Fixing NA's
train$Age <- replace_na(train$Age, median(na.omit(train$Age)))


# Convert Integer Values (1,0) to Boolean
train$Survived = as.logical(train$Survived)


# Creating Age Categories
AgeBreaks = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)
AgeLabels = c("0-1","2-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
              "45-49", "50-54", "55,59", "60-64", "65-69", "70-74", "75-79")

# Add a new column that specifies the age group of the person
setDT(train)[, AgeGroups := cut(x=Age, breaks=AgeBreaks, right=TRUE, labels=AgeLabels)]

# Free up memory
remove(AgeBreaks)
remove(AgeLabels)






# Stacked Bar Chart of Survival by Gender
ggplot(data = train) +
  geom_bar(mapping = aes(x=Sex, fill=factor(Survived)),
           main="Survival Based on Sex",
           xlab="Sex")

# Boxplot of Fare Split by Survival
ggplot(data = train) +
  geom_boxplot(mapping = aes(x=factor(Survived), y=Fare))

# Stacked Bar Graph of Survival based on Age Group
ggplot(data = train) +
  geom_bar(mapping = aes(x=AgeGroups, fill=Survived)) +
  ggtitle("Survival Based on Age Group")
