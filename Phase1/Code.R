
# ------------------- DATA PREPROCESSING, VISUALIZATION, EXPLORATION -------------------

# ***** DATA PREPROCESSING *****

# Setting the working directory
setwd("C:\\Users\\joewe\\Desktop\\LAU\\Semesters\\Fall 2023\\CSC463\\Projects\\CSC463\\Phase1")
# Loading the dataset into a variable called 'dataset'
dataset <- read.csv("ky_louisville_2023_01_26.csv")

# Gaining insight to the structure of the dataset.
str(dataset) #There are 146562 rows. Only subject_age is of type int, lat lng are nums, and the rest chars

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

# ***** DATA Visualization *****

# ***** DATA Exploration *****

summary(dataset)








