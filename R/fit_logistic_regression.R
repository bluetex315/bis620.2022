#' Fit Logistic Regression Model
#'
#' Fits a logistic regression model to the training data and evaluates its performance on the test data.
#' The function prints the model's accuracy and Area Under the Curve (AUC) for the Receiver Operating
#' Characteristic (ROC) curve. It also returns the fitted model object.
#'
#' @param train_data Training dataset including both predictors and response variable.
#' @param test_data Test dataset including both predictors and response variable.
#' @return The fitted logistic regression model object (`glm` object).
#' @importFrom stats glm
#' @importFrom pROC roc auc
#' @export
#' @seealso \link[stats]{glm}, \link[pROC]{roc}
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
#' @export
fit_logistic_regression