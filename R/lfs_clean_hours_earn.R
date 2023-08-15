#' Clean earnings and hours of work data
#'
#' Converts weekly earnings into real terms, constructs actual and usual hours worked per week
#' and two hourly wage measures based on these hours variables.
#'
#' @param data data table. Raw LFS data produced using the reading functions.
#' @param deflator character. Inflation index to use in producing real values. One of "cpih" or "rpi".
#' Default is cpih.
#'
#' @return Returns a new set of variables
#' @export
lfs_clean_hours_earn <- function(
  data,
  deflator = "cpih"
) {

  infl <- lfsclean::inflation[measure == deflator, ]

  # merge CPI into the main dataset
  data <- merge(data, infl, by = c("month","year"))

  # convert weekly earnings from nominal to real
  data[, weekly_earnings := grsswk*(100/index) ]
  data[, weekly_earnings_nom := grsswk ]

  # calculate hourly wages for both usual and actual hours worked.
  data[, awage := weekly_earnings/ttachr ]
  data[, uwage := weekly_earnings/ttushr ]
  data[, awage_nom := weekly_earnings_nom/ttachr ]
  data[, uwage_nom := weekly_earnings_nom/ttushr ]
  data[, ahours := ttachr ]
  data[, uhours := ttushr ]

  # clean infinite values
  data[is.infinite(awage), awage := NA ]
  data[is.infinite(uwage), uwage := NA ]
  data[is.infinite(awage_nom), awage_nom := NA ]
  data[is.infinite(uwage_nom), uwage_nom := NA ]

  ###############################
  ### Over/underemployment

  if ("undemp" %in% colnames(data)){
  data[, undemp := factor(undemp, levels = 1:2, labels = c("yes","no"))]
  data[, ovemp  := factor(lespay2, levels = 1:2, labels = c("yes","no"))]

  data[, undhrs := ifelse(undhrs %in% 97:99, NA, undhrs)]
  data[, ovhrs := ifelse(ovhrs %in% 97:99, NA, ovhrs)]

  } else {

  data[, undemp := NA]
  data[, ovemp := NA]
  data[, undhrs := NA]
  data[, ovhrs := NA]

  }
  ###############################
  ### RETAIN CLEANED VARIABLES

  final_data <- data[, c("obs_id",
                         "weekly_earnings", "weekly_earnings_nom", "awage", "uwage",
                         "awage_nom", "uwage_nom", "ahours", "uhours",
                         "undemp","ovemp","undhrs","ovhrs")]

  var_names <- c("weekly_earnings", "weekly_earnings_nom", "awage", "uwage",
                 "awage_nom", "uwage_nom", "ahours", "uhours",
                 "undemp","ovemp","undhrs","ovhrs")

  setnames(final_data, var_names, paste0("eh_", var_names))

  return(final_data)
}
