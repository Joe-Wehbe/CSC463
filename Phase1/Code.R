
# Joe Wehbe & Roula Ghaleb

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


# ********* DATA PREPROCESSING ********


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
summary(dataset)
# We learn we have 3585 entries with several columns
# Numeric columns include lat, lng, subject_age
# Logical columns include citation_issued, citation_issued,  frisk_performed, search_conducted

# Gaining insight to the structure of the dataset.
str(dataset) #There are 146562 rows. Only subject_age is of type int, lat lng are nums, and the rest chars

# Data profiling using skimr to get a clearer view of our dataset
skim(dataset)


# ********* DATA Visualization *********


# Here, we are visualizing some of our data in order to better grasp
# the content and groups of the dataset that we chose
# Histogram for subject_ag
ggplot(dataset, aes(x = subject_age)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Subject Age", x = "Age", y = "Frequency")
# Here, we can tell that most of the stopped people are from their between twenty to
# almost fifty

# Bar chart for subject_race
ggplot(dataset, aes(x = subject_race, fill = subject_race)) +
  geom_bar() +
  labs(title = "Distribution by Subject Race", x = "Race", y = "Frequency")
# Most are white in this area that are stopped

# Bar chart for subject_sex
ggplot(dataset, aes(x = subject_sex, fill = subject_sex)) +
  geom_bar() +
  labs(title = "Distribution by Subject Sex", x = "Sex", y = "Frequency")

#  Search basis Consent and age
specific_data <- subset(dataset, reason_for_search == "CONSENT")
ggplot(data = specific_data) +
  geom_histogram(aes(x = subject_age), color = "black", fill = "red") +
  labs(title = "Age Distribution for 'CONSENT'",
       x = "Age",
       y = "Count")
# We notice a trend of this search for the younger ages

# Sex + age
ggplot(data = dataset) +
  geom_jitter(mapping = aes(x = subject_age, y = subject_sex), width = 0.4, height = 0.2) +
  labs(title = "Distribution of Age vs. Sex", x = "Age", y = "Sex")
# Dominance here for males under 45 and females under 30

# Race + age
ggplot(data = dataset) +
  geom_jitter(mapping = aes(x = subject_age, y = subject_race), width = 0.4, height = 0.2) +
  labs(title = "Distribution of Age vs. Race", x = "Age", y = "Race")
# For example, there is a wide distribution for black and white people
# Hispanic and others may be outliers
# We notice a lack of older people
# The "asian/pacific islander" and other groups have a majority of its data points clustered in the younger age range

# Officer race + age
ggplot(data = dataset) +
  geom_jitter(mapping = aes(x = subject_age, y = officer_race), width = 0.4, height = 0.2) +
  labs(title = "Distribution of Age of Stopped Person vs. Officer Race",
       x = "Age of Stopped Person",
       y = "Officer Race")
# We notice Hispanics, Blacks, and Asian/Pacific Islanders tend to stop younger people
# Whites have a wider distribution

# Officer sex + age
ggplot(data = dataset) +
  geom_jitter(mapping = aes(x = subject_age, y = officer_sex), width = 0.4, height = 0.2) +
  labs(title = "Distribution of Age of Stopped Person by Officer's Gender",
       x = "Age of Stopped Person",
       y = "Officer's Gender")
# We notice a big part of females arrests are under 40

# Outcome + age
ggplot(data = dataset) +
  geom_jitter(mapping = aes(x = subject_age, y = outcome), width = 0.4, height = 0.2) +
  labs(title = "Distribution of Age of Stopped Person by Outcome",
       x = "Age of Stopped Person",
       y = "Outcome")
# Citations are more frequent than warnings
# Younger people more likely to receive warnings 
# We notice the data for warnings is sparse, so maybe not many warnings are issued

#Age + search basis
ggplot(data = dataset) +
  geom_jitter(mapping = aes(x = subject_age, y = search_basis), width = 0.4, height = 0.2) +
  labs(title = "Distribution of Age of Stopped Person by search basis",
       x = "Age of Stopped Person",
       y = "Search basis")
# Dense middle age range for all
# K9 is focused on younger ages
# Probable cause and plain view have wide distributions
# Other searches are broad but are sparse in older ages
# Consent dense in young and middle range


# ------------------------------ Linear Regression ------------------------------


# Performing linear regression techniques
model1 <- lm(subject_age ~ subject_sex + subject_race + officer_sex + officer_race + outcome + search_basis  + frisk_performed, data = dataset)
summary(model1) # R-squared: 0.03302, F-statistic: 9.159, P-value < 0.05

model2 <- lm(subject_age ~ subject_sex + subject_race, data = dataset)
summary(model2) # R-squared: 0.005139, F-statistic: 4.703, P-value < 0.05

model3 <- lm(subject_age ~ officer_sex + officer_race + outcome, data = dataset)
summary(model3) # R-squared: 0.0003315, F-statistic: 1.238, P-value < 0.05

model4 <- lm(subject_age ~ outcome + search_basis + frisk_performed, data = dataset)
summary(model4) # R-squared: 0.02851, F-statistic: 18.53, P-value < 0.05
















