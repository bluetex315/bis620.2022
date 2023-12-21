#' Fit Random Forest Model
#'
#' Fits a random forest model to the provided training data and evaluates its performance on the test data.
#' It prints the model's accuracy and generates a ROC curve with the AUC value. The function also plots
#' variable importance and returns the fitted random forest model object.
#'
#' @param train_data Training dataset including predictors and response variable.
#' @param test_data Test dataset including predictors and response variable.
#' @return The fitted random forest model object (`randomForest` object).
#' @importFrom randomForest randomForest
#' @importFrom pROC roc auc
#' @importFrom graphics par
#' @importFrom randomForest importance varImpPlot
#' @export
#' @seealso \link[randomForest]{randomForest}, \link[pROC]{roc}
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
#' @export
fit_random_forest