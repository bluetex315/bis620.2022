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

## -----------------------------------------------------------------------------
data <- adsl %>%
  full_join(biomark, by = 'SUBJID')

final_df_clean = cleaning_helperfunc(data)
help(cleaning_helperfunc)
# Description
# This function performs several cleaning operations on a data frame. It converts categorical columns to numeric, focusing on specific variables of interest, and removes rows with missing values. It also provides a summary of missing values in the specified variables of interest.

## -----------------------------------------------------------------------------
head(final_df_clean)

## -----------------------------------------------------------------------------
# Relevant Package Installation
library(randomForest)
library(rpart.plot)
library(caTools)
library(caret)
library(pROC)

## -----------------------------------------------------------------------------
library(survival)
Model1 = fit_coxph(final_df_clean)
# Description
# This function fits a Cox Proportional Hazards model to the provided dataset using specified covariates. It is designed to analyze survival data, specifically to model the time until an event of interest (or hazard) occurs.

## -----------------------------------------------------------------------------
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

