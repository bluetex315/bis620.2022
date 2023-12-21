#' Data Cleaning Helper Function
#'
#' This function performs several cleaning operations on a data frame. It converts
#' categorical columns to numeric, focusing on specific variables of interest,
#' and removes rows with missing values. It also provides a summary of missing values
#' in the specified variables of interest.
#' 
#' @param df A data frame that needs cleaning.
#' @return A cleaned data frame with categorical variables converted to numeric,
#'         specific variables of interest selected, and rows with missing values removed.
#' @details The function first converts categorical columns (excluding specified ones)
#'          to numeric by treating NA or blank values as 'unknown'. It then selects
#'          variables of interest and removes rows with any missing values in these
#'          variables. The function also prints out the number of missing values for
#'          each variable of interest both before and after the cleaning process.
#' @examples
#' df <- data.frame(
#'   ATRT = c("Yes", "No", "No", NA),
#'   PRSURG = c("Type1", "Type2", "", "Type1"),
#'   LIVERMET = c("Yes", NA, "No", "No"),
#'   AGE = c(45, 50, 60, 30)
#'   # ... other columns ...
#' )
#' cleaned_df <- cleaning_helperfunc(df)
#' @export

cleaning_helperfunc <- function(df) {
  
  # Function to convert a categorical column to numeric
  convert_to_numeric <- function(column) {
    # Treat NA or blank values as 'unknown'
    column[is.na(column) | column == ""] <- "unknown"
    
    # Convert the categorical column to a factor and then to numeric
    as.numeric(factor(column)) - 1  # Subtract 1 to start encoding at 0
  }
  
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
#' @export
cleaning_helperfunc