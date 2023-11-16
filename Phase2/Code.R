
# Joe Wehbe 202000908 & Roula Ghaleb 202001243

# Qualitative response: subject_sex


# ------------------- DATA PREPROCESSING -------------------


# Installing needed packages
install.packages("caTools")
install.packages("caret")
install.packages("MASS")

# Importing needed libraries
library(caTools)
library(caret)
library(MASS)

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


# Selecting the features and the target variable
features <- c("subject_age", "subject_race")
target <- "subject_sex"

# Splitting the data into features (X) and target variable (y)
X <- dataset[, features]
y <- dataset[, target]

# Splitting the data into training and testing sets
split <- sample.split(y, SplitRatio = 0.8)
X_train <- subset(X, split == TRUE)
X_test <- subset(X, split == FALSE)
y_train <- subset(y, split == TRUE)
y_test <- subset(y, split == FALSE)

# Creating a logistic regression model
model <- glm(y_train ~ ., data = cbind(X_train, y_train), family = "binomial")

# Making predictions on the test set
predictions <- predict(model, newdata = cbind(X_test, y_test), type = "response")

# Converting predicted probabilities to binary predictions (0 or 1)
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Evaluating the model
confusion_matrix <- table(predicted_classes, y_test)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Displaying results
print(confusion_matrix)
print(paste("Accuracy:", accuracy))


# ------------------- Linear Discriminant Analysis-------------------


# Creating an lda model 
lda_model <- lda(subject_sex ~ subject_age + subject_race, data = dataset)

# Making predictions
predictions <- predict(lda_model, newdata = dataset)

# Displaying the results
conf_matrix <- table(predictions$class, dataset$subject_sex)
print(conf_matrix)


# ------------------- Quadratic Discriminant Analysis -------------------


# Creating a qda model
qda_model <- qda(subject_sex ~ subject_age + subject_race, data = dataset)

# Making predictions
predictions_qda <- predict(qda_model, newdata = dataset)

# Displaying the results
conf_matrix_qda <- table(predictions_qda$class, dataset$subject_sex)
print(conf_matrix_qda)


# ------------------- Resampling Techniques -------------------


# ------------------- Subset Selection -------------------
# Assuming 'dataset' is your data frame
# Assuming 'dataset' is your data frame
# Assuming 'dataset' is your data frame
# Assuming 'dataset' is your data frame
p <- ncol(dataset) - 1  # Assuming the response variable is included in the dataset

model_forward <- glm(subject_sex ~ 1, family = binomial, data = dataset)  # Start with an intercept-only model

while (length(coefficients(model_forward)) <= p) {
  # Fit models with each additional predictor
  models <- lapply(setdiff(names(dataset), names(model_forward$coef)), function(var) {
    glm(subject_sex ~ . + get(var), family = binomial, data = dataset)
  })
  
  # Select the model with the lowest AIC or BIC
  model_forward <- models[[which.min(sapply(models, AIC))]]
}

# Display the final model
summary(model_forward)



# Check the levels of subject_race
levels(dataset$subject_race)























