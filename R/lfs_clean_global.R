#' Clean LFS data
#'
#' A wrapper function for applying all of the cleaning functions and selecting the
#' desired variables/observations for the analysis by applying the select_data function.
#'
#' @param data Data table - the Labour Force Survey dataset.
#' @param ages Integer vector - the ages in single years to retain (defaults to 16 to 89 years).
#' @param years Integer vector - the years in single years to retain (defaults to 2010 to 2020).
#' @param keep_vars Character vector - the names of the variables to keep (defaults to year, age and gender).
#' @param deflator character. Inflation index to use in producing real values. One of "cpih" or "rpi".
#' Default is cpih.
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to year, age and gender).
#' @return Returns a new set of variables
#' @export
lfs_clean_global <- function(data,
                             ages = 16:89,
                             years = 1993:2021,
                             keep_vars = NULL,
                             deflator = "cpih",
                             complete_vars = c("year", "age", "sex")
) {

  ## fix bug that occurs if age is not in keep_vars

  if (!("age" %in% names(data)) ) {
    ages = NULL
  }

  #######################################################################
  #### Save out a dataset containing the key identifiers and weights ####

  data$obs_id <- 1:nrow(data)

  main_data <- data[, c("obs_id",
                        "month","quarter","year","pwt","piwt")]

  ### demographics

  #cat(crayon::cyan("\n\t\tDemographic variables module\n"))

  demographics <- lfsclean::lfs_clean_demographic(data = data)

  ### labour market

  #cat(crayon::cyan("\n\t\tLabour market variables module\n"))

  lmkt <- lfsclean::lfs_clean_economic_activity(data = data)

  ### hours and earnings

  #cat(crayon::cyan("\n\t\tHours and earnings variables module\n"))

  earn <- lfsclean::lfs_clean_hours_earn(data = data, deflator = deflator)

  ### benefits

  #cat(crayon::cyan("\n\t\tHours and earnings variables module\n"))

  bene <- lfsclean::lfs_clean_benefit(data = data)

  ### education

  #cat(crayon::cyan("\n\t\tEducation variables module\n\n"))

  educ <- lfsclean::lfs_clean_education(data = data)

  ### health

  #cat(crayon::cyan("\n\t\tHealth variables module\n\n"))

  heal <- lfsclean::lfs_clean_health(data = data)

  ######################
  ### Merge datasets ###

  merged_data <- merge(main_data, demographics, by = c("obs_id"), all.x = TRUE)
  merged_data <- merge(merged_data, lmkt,       by = c("obs_id"), all.x = TRUE)
  merged_data <- merge(merged_data, bene,       by = c("obs_id"), all.x = TRUE)
  merged_data <- merge(merged_data, educ,       by = c("obs_id"), all.x = TRUE)
  merged_data <- merge(merged_data, heal,       by = c("obs_id"), all.x = TRUE)
  merged_data <- merge(merged_data, earn,       by = c("obs_id"), all.x = TRUE)

   final_data <- lfsclean::select_data(
      data = merged_data,
      ages = ages,
      years = years,
      keep_vars = keep_vars,
      complete_vars = complete_vars
    )

  return(final_data)
}
