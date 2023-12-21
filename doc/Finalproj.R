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
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(rpart.plot))
suppressPackageStartupMessages(library(caTools))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(pROC))

## -----------------------------------------------------------------------------
suppressPackageStartupMessages(library(survival))
Model1 = fit_coxph(final_df_clean)
?fit_coxph
# Description
# This function fits a Cox Proportional Hazards model to the provided dataset using specified covariates. It is designed to analyze survival data, specifically to model the time until an event of interest (or hazard) occurs.

## -----------------------------------------------------------------------------
Model1

## -----------------------------------------------------------------------------
suppressPackageStartupMessages(library(caTools))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(pROC))

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

Model2 = fit_logistic_regression(train_data, test_data)
?fit_logistic_regression
# Description:
# Fits a logistic regression model to the training data and evaluates its performance on the test data. The function prints the model's accuracy and Area Under the Curve (AUC) for the Receiver Operating Characteristic (ROC) curve. It also returns the fitted model object.

## -----------------------------------------------------------------------------
Model2

## -----------------------------------------------------------------------------
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(rpart.plot))

# we defined a new function called fit random forest
# The following is our function description:
# Fits a random forest model to the provided training data and evaluates its performance on the test data. It prints the model's accuracy and generates a ROC curve with the AUC value. The function also plots variable importance and returns the fitted random forest model object.
?fit_random_forest

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
print(summary(Model1))

## -----------------------------------------------------------------------------
print(summary(Model2))

## -----------------------------------------------------------------------------
Model3

