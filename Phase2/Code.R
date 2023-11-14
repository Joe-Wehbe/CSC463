
# Joe Wehbe 202000908 & Roula Ghaleb 202001243

# Qualitative response: subject_sex

# ------------------- DATA PREPROCESSING -------------------


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

# Setting the working directory
setwd("C:\\Users\\joewe\\Desktop\\LAU\\Semesters\\Fall 2023\\CSC463\\Projects\\CSC463\\Phase2")
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

# Encoding 'male' and 'female' in subject_sex to numerical values (male: 0, female: 1)
dataset$subject_sex <- as.numeric(dataset$subject_sex == "female")


# ------------------- Logistic Regression -------------------


# ------------------- Linear Discriminant Analysis-------------------


# ------------------- Quadratic Discriminant Analysis -------------------


# ------------------- Resampling Techniques -------------------


# ------------------- Subset Selection -------------------




















