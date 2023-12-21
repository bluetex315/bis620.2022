## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bis620.2022)
library(readr)
library(dplyr)
library(tidyr)

data <- adsl %>%
  full_join(biomark, by = 'SUBJID')

data1 = data

my_func <- function(df) {
    
  # Function to convert a categorical column to numeric
  convert_to_numeric <- function(column) {
    # Treat NA or blank values as 'unknown'
    column[is.na(column) | column == ""] <- "unknown"
  
    # Convert the categorical column to a factor and then to numeric
    as.numeric(factor(column)) - 1  # Subtract 1 to start encoding at 0
  }
  
  # Create dummy variables for 'sex' and 'race', and bind them to the dataset
  # if("sex" %in% names(data)) {
  #   sex_dummies <- model.matrix(~ sex - 1, data)
  #   colnames(sex_dummies) <- paste("sex", colnames(sex_dummies), sep = "_")
  #   data <- bind_cols(data, as.data.frame(sex_dummies))
  #   data <- data %>% select(-sex)
  # }
  
  # if("race" %in% names(data)) {
  #   race_dummies <- model.matrix(~ race - 1, data)
  #   colnames(race_dummies) <- paste("race", colnames(race_dummies), sep = "_")
  #   data <- bind_cols(data, as.data.frame(race_dummies))
  #   data <- data %>% select(-race)
  # }
  
  # Apply the conversion to all other categorical columns except the specified ones
  categorical_columns <- sapply(df, is.character)
  
  categorical_columns[1] <- FALSE # Assuming the first column is the Subject ID
  #categorical_columns[20:37] <- FALSE # Exclude columns 20 to 37
  categorical_columns[20] <- FALSE
  categorical_columns[22] <- FALSE
  categorical_columns[24] <- FALSE
  categorical_columns[26] <- FALSE
  categorical_columns[28] <- FALSE
  categorical_columns[30] <- FALSE
  categorical_columns[32] <- FALSE
  categorical_columns[34] <- FALSE
  categorical_columns[36] <- FALSE
  
  df[categorical_columns] <- lapply(df[categorical_columns], convert_to_numeric)

  # variables of interest
  var_of_interest <- c("ATRT", "PRSURG", "LIVERMET", "AGE", "SEX", "B_WEIGHT", "B_HEIGHT", "B_ECOG", "B_METANM", "DIAGTYPE", "DTH", "DTHDY")
  df <- df[var_of_interest]
  
  # Explore missing values
  missing_values <- numeric(length(var_of_interest))
  
  for (i in seq_along(var_of_interest)) {
    missing_values[i] <- sum(is.na(df[[var_of_interest[i]]]))
  }

  missing_values_df <- data.frame(Variable = var_of_interest, MissingValuesCount = missing_values)
  print(missing_values_df)
  
  # Remove rows with any missing values
  final_df <- na.omit(df)
  
  print(paste("Original number of rows:", nrow(df)))
  print(paste("Number of rows after removing missing data:", nrow(final_df)))
  
  return(final_df)
}

final_df_clean = my_func(data)

## -----------------------------------------------------------------------------
# Check Package Installation
library(randomForest)
library(rpart.plot)
library(caTools)
library(caret)
library(pROC)

## -----------------------------------------------------------------------------
library(survival)

fit_coxph <- function(df){
  Vars = c("ATRT", "PRSURG", "LIVERMET", "AGE", "SEX", "B_WEIGHT", "B_HEIGHT", "RACE", "B_ECOG", "B_METANM", "DIAGTYPE")

  Model <- coxph(Surv(DTHDY, DTH) ~
                  ATRT + PRSURG + LIVERMET + AGE + SEX + B_WEIGHT + B_HEIGHT + B_ECOG + B_METANM + DIAGTYPE,
                    data = df)
  return(Model)
}

Model1 = fit_coxph(final_df_clean)
Model1

## -----------------------------------------------------------------------------
library(caTools)
library(caret)
library(pROC)

# Splitting the data into training and test sets
train_test_split <- function(df){
  
  set.seed(123)
  split <- sample.split(final_df_clean$DTH, SplitRatio = 0.7)
  train_data <- final_df_clean[split == TRUE, ]
  test_data <- final_df_clean[split == FALSE, ]
  
  return(list(train = train_data, test = test_data))
}

data_all = train_test_split(final_df_clean)
train_data = data_all$train
test_data = data_all$test

fit_logistic_regression <- function(train_data, test_data){
  
  Model2 <- glm(DTH ~ ATRT + PRSURG + LIVERMET + AGE + SEX + B_WEIGHT + B_HEIGHT + B_ECOG + B_METANM + DIAGTYPE, 
             data = train_data, family = binomial())

  predictions <- predict(Model2, newdata = test_data, type = "response")
  predicted_class <- ifelse(predictions > 0.5, 1, 0)
  accuracy <- sum(predicted_class == test_data$DTH) / nrow(test_data)
  print(paste("Accuracy:", accuracy))
  
  # Generate ROC curve and compute AUC
  par(pty = "s")
  # roc(test_data$DTH, predictions, plot = TRUE, legacy.axes = TRUE,
  #                  col="#377eb8", lwd = 3, print.auc = TRUE)
  
  roc_curve <<- roc(test_data$DTH, predictions, plot = TRUE, legacy.axes = TRUE,
                   col="#377eb8", lwd = 3, print.auc = TRUE)
  logistic_auc_value <<- auc(roc_curve)
  print(paste("AUC:", logistic_auc_value))
  
  par(pty = "m")
  
  print(summary(Model2))
  
  return(Model2)
}

Model2 = fit_logistic_regression(train_data, test_data)
Model2

## -----------------------------------------------------------------------------
library(randomForest)
library(rpart.plot)

fit_random_forest <- function(train_data, test_data){
  
  # for bug fixes
  train_data$DTH <- as.factor(train_data$DTH)
  
  # Fit random forest model
  rf_model <- randomForest(DTH ~ ATRT + PRSURG + LIVERMET + AGE + SEX + B_WEIGHT + B_HEIGHT + B_ECOG + B_METANM + DIAGTYPE, 
                           data = train_data, 
                           ntree = 1000, # number of trees
                           importance = TRUE) # variable importance
  # print(summary(rf_model))
  # Make predictions on the test set
  rf_predictions <- predict(rf_model, newdata = test_data, type = "prob")[,2]
  rf_predicted_class <- ifelse(rf_predictions > 0.5, 1, 0)
  rf_accuracy <- sum(rf_predicted_class == test_data$DTH) / nrow(test_data)
  print(paste("Random Forest model accuracy:", rf_accuracy))
  
  # Generate ROC curve and compute AUC for random forest
  par(pty = "s")
  
  # plot.roc(test_data$DTH, rf_predictions, legacy.axes = TRUE,
  #         col="#4daf4a", lwd = 3, print.auc = TRUE,)
  
  rf_roc_curve <<- roc(test_data$DTH, rf_predictions, plot = TRUE, legacy.axes = TRUE,
                      col="#4daf4a", lwd = 3, print.auc = TRUE)
  rf_auc_value <<- auc(rf_roc_curve)
  
  par(pty = "m")
  
  importance <- importance(rf_model)
  varImpPlot(rf_model)
  
  return(rf_model)

}

Model3 = fit_random_forest(train_data, test_data)

## -----------------------------------------------------------------------------
generate_roc_for_two <- function(logistic_roc, rf_roc, logistic_auc, rf_auc){
    
  par(pty = "s")
  plot(logistic_roc, col="#377eb8", lwd = 2.5, main="ROC Curves Comparison")
  # Add ROC curve for random forest
  lines(rf_roc, col="#4daf4a", lwd = 2.5)
  
  legend("bottomright", legend=c("Logistic Regression", "Random Forest"), 
         col=c("#377eb8", "#4daf4a"), lty=c(1, 2), cex=0.7)
  
  # Optionally, add the AUC scores to the legend if you have them calculated
  # Assume logistic_auc_value and rf_auc_value hold the AUC values for logistic regression and random forest, respectively
  legend("bottomright", legend=c(paste("Logistic Regression AUC =", round(logistic_auc, 4)), 
                                 paste("Random Forest AUC =", round(rf_auc, 4))), 
         col=c("#377eb8", "#4daf4a"), lty=c(1, 2), cex=0.7)
  
  par(pty = "m")

}

generate_roc_for_two(roc_curve, rf_roc_curve, logistic_auc_value, rf_auc_value)

