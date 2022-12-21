#' Results from a simulated clinical trial with interaction effects.
#'
#' A dataset containing physiological data, biomarkers, and categorical data.
#'
#' @format A tibble with 300 rows and 24 variables:
#' \describe{
#'   \item{Sex}{Sex of animal, factor with levels 'female', 'male'}
#'   \item{Agegroup}{Factor with levels 'young','middle','old'}
#'   \item{Treatment}{Factor with levels 'sham', 'OP'}
#'   \item{HR}{Heart rate}
#'   \item{sysRR,diaRR}{Systolic and diastolic blood pressure}
#'   \item{Med xxx}{Pseudo-medications, factors with levels 'y','n'}
#'   \item{Biomarker x [units]}{Biomarkers with log-normal distribution}
#'   \item{Responder}{factor yes/no, systolic plood pressure >= 120?}
#' }
#' 
"faketrial"