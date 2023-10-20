
# ------------------- DATA PREPROCESSING, VISUALIZATION, EXPLORATION -------------------


# Installing needed packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("skimr")

# Importing needed libraries
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(skimr)

# ********* DATA PREPROCESSING *********


# Setting the working directory
setwd("C:\\Users\\joewe\\Desktop\\LAU\\Semesters\\Fall 2023\\CSC463\\Projects\\CSC463\\Phase1")
# Loading the dataset into a variable called 'dataset'
dataset <- read.csv("ky_louisville_2023_01_26.csv")

# Checking for missing values in each column
sapply(dataset, function(x) sum(is.na(x)))
# Removing rows that contain missing values
dataset <- na.omit(dataset)
print(nrow(dataset)) # We now have 3585 rows with no missing values

# Removing duplicate rows if there are any.
dataset <- unique(dataset) 
print(nrow(dataset)) # No duplicate rows, we still have 3585

# Scaling numeric variables (lat and lng)
dataset$lat <- scale(dataset$lat)
dataset$lng <- scale(dataset$lng)


# ********* DATA Exploration *********

# Summary statistics
summary(data)

# Gaining insight to the structure of the dataset.
str(dataset) #There are 146562 rows. Only subject_age is of type int, lat lng are nums, and the rest chars

# Data profiling using dplyr and skimr
skim(dataset)

# ********* DATA Visualization *********


# Histogram for subject_age
ggplot(dataset, aes(x = subject_age)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Subject Age", x = "Age", y = "Frequency") +
  theme_minimal()

# Bar chart for subject_race
ggplot(dataset, aes(x = subject_race, fill = subject_race)) +
  geom_bar() +
  labs(title = "Distribution by Subject Race", x = "Race", y = "Frequency") +
  theme_minimal()

# Bar chart for subject_sex
ggplot(dataset, aes(x = subject_sex, fill = subject_sex)) +
  geom_bar() +
  labs(title = "Distribution by Subject Sex", x = "Sex", y = "Frequency") +
  theme_minimal() 

# Sex + age
ggplot(data = dataset) + 
  geom_jitter(mapping = aes(x = subject_age, y = subject_sex), width = 0.4, height = 0.2) +
  labs(title = "Distribution of Age vs. Sex", x = "Age", y = "Sex") +
  theme_minimal()

# Race + age
ggplot(data = dataset) + 
  geom_jitter(mapping = aes(x = subject_age, y = subject_race), width = 0.4, height = 0.2) +
  labs(title = "Distribution of Age vs. Race", x = "Age", y = "Race") +
  theme_minimal()

# Officer race + age
ggplot(data = dataset) + 
  geom_jitter(mapping = aes(x = subject_age, y = officer_race), width = 0.4, height = 0.2) +
  labs(title = "Distribution of Age of Stopped Person vs. Officer Race", 
       x = "Age of Stopped Person", 
       y = "Officer Race") +
  theme_minimal()

# Officer gender + age
ggplot(data = dataset) + 
  geom_jitter(mapping = aes(x = subject_age, y = officer_sex), width = 0.4, height = 0.2) +
  labs(title = "Distribution of Age of Stopped Person by Officer's Gender", 
       x = "Age of Stopped Person", 
       y = "Officer's Gender") +
  theme_minimal()

# Outcome + age
ggplot(data = dataset) + 
  geom_jitter(mapping = aes(x = subject_age, y = outcome), width = 0.4, height = 0.2) +
  labs(title = "Distribution of Age of Stopped Person by Outcome", 
       x = "Age of Stopped Person", 
       y = "Outcome") +
  theme_minimal()

# For example, for "plain view"
#filtered_data <- subset(dataset, reason_for_search == "plain view")
#ggplot(data = filtered_data) + 
#  geom_histogram(aes(x = subject_age)) + 
#  labs(title = "Age Distribution for 'plain view'", 
#       x = "Age", 
#       y = "Count") + 
#  theme_minimal()


#                       ------------------- Linear Regression -------------------



















