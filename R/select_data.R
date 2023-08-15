#' Select variables and apply filters
#'
#' Selects the variables required for analysis and selects only the rows without missing data
#' for specified variables.
#'
#' @param data Data table - the Health Survey for England dataset.
#' @param ages Integer vector - the ages in single years to retain (defaults to 16 to 89 years).
#' @param years Integer vector - the years in single years to retain (defaults to 2010 to 2020).
#' @param keep_vars Character vector - the names of the variables to keep (defaults to year and age).
#' @param complete_vars Character vector - the names of the variables on which the selection of complete
#' cases will be based (NUll, default, for no complete case filtering).
#' @param longitudinal Logical - TRUE if the data comes from the LFS 5-q longitudinal data, FALSE if from the quarterly cross sectional data
#'
#' @importFrom data.table :=
#' @return Returns a reduced version of data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data <- select_data(data, keep_vars = c("age", "gender", "region"))
#'
#' }
#'

select_data <- function(
  data,
  ages = 16:89,
  years = 1993:2020,
  keep_vars = NULL,
  complete_vars = NULL,
  longitudinal = FALSE
) {

  ###########################
  ## filter age and years

  if (longitudinal == FALSE){
  data <- data[d_age %in% ages & year %in% years]
  }


  #################################################################
  ## keep only complete cases of variables named in complete_vars

  for(cv in complete_vars) {

    data <- data[!is.na(get(cv))]

  }

  if (is.null(keep_vars)) {

    keep_vars <- names(data)
  }

  ## only keep variables named in keep_vars
  keep_vars <- intersect(names(data), keep_vars)

  data <- data[ , keep_vars, with = F]


  return(data)
}
