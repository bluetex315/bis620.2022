#' Fit a Cox Proportional Hazards Model
#'
#' This function fits a Cox Proportional Hazards model to the provided dataset using specified covariates. 
#' It is designed to analyze survival data, specifically to model the time until an event of interest (or hazard) occurs.
#'
#' @param df A data frame containing the variables of interest, including the survival time, event indicator, 
#' and covariates. Expected covariates are "ATRT", "PRSURG", "LIVERMET", "AGE", "SEX", "B_WEIGHT", "B_HEIGHT", 
#' "RACE", "B_ECOG", "B_METANM", and "DIAGTYPE".
#' @return A Cox Proportional Hazards model object (`coxph` object).
#' @importFrom survival coxph Surv
#' @export
fit_coxph <- function(df){
  Vars = c("ATRT", "PRSURG", "LIVERMET", "AGE", "SEX", "B_WEIGHT", "B_HEIGHT", "RACE", "B_ECOG", "B_METANM", "DIAGTYPE")
  
  Model <- coxph(Surv(DTHDY, DTH) ~
                   ATRT + PRSURG + LIVERMET + AGE + SEX + B_WEIGHT + B_HEIGHT + B_ECOG + B_METANM + DIAGTYPE,
                 data = df)
  return(Model)
}
#' @export
fit_coxph