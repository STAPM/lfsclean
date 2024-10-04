#' Clean benefits data
#'
#' Cleans the raw data for social security benefit related variables.
#'
#' @param data data table. Raw LFS data produced using the reading functions.
#'
#' @return Returns a new set of variables
#' @export
lfs_clean_benefit <- function(
    data
) {

  # Main type of benefit claimed by those out of work
  if ("ooben" %in% colnames(data)) {
    data[ooben %in% c(1) , oowben := "unemployed"]
    data[ooben %in% c(2) , oowben := "sick_injured_disabled"]
    data[ooben %in% c(3:5) , oowben := "lone_parent_carer_other_oow"]
    data[ooben %in% c(6) , oowben := "other_benefits"]
    data[ooben %in% c(7) , oowben := "no_benefits"]

  } else {

    data[, oowben := NA]
  }

  ###############################
  ### RETAIN CLEANED VARIABLES

  final_data <- data[, c("obs_id", "oowben")]

  var_names <- c("oowben")

  setnames(final_data, var_names, paste0("b_", var_names))

  return(final_data)

}
