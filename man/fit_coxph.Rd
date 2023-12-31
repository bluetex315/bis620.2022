% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_coxph.R
\name{fit_coxph}
\alias{fit_coxph}
\title{Fit a Cox Proportional Hazards Model}
\usage{
fit_coxph(df)
}
\arguments{
\item{df}{A data frame containing the variables of interest, including the survival time, event indicator,
and covariates. Expected covariates are "ATRT", "PRSURG", "LIVERMET", "AGE", "SEX", "B_WEIGHT", "B_HEIGHT",
"RACE", "B_ECOG", "B_METANM", and "DIAGTYPE".}
}
\value{
A Cox Proportional Hazards model object (\code{coxph} object).
}
\description{
This function fits a Cox Proportional Hazards model to the provided dataset using specified covariates.
It is designed to analyze survival data, specifically to model the time until an event of interest (or hazard) occurs.
}
