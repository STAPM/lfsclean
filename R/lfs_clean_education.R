#' Clean education data
#'
#' Construct highest qualification variable and a binary variable for whether or not the
#' individual is degree-level educated.
#'
#' @return Returns a new set of variables
#' @export
lfs_clean_education <- function(
  data
) {

  ### Age completed full time education

  # 97 = never had education (assume current school leaving age of 16)
  # 96 = still in education
  data[edage == 97, edage := 16]

  data[edage >= age & edage != 96, exper := age - edage]

  ### 2022- based on hiqul22d
  if("hiqul22d" %in% colnames(data)) {
    data[hiqul22d == 1, highest_qual := 6]
    data[hiqul22d == 2, highest_qual := 5]
    data[hiqul22d == 3, highest_qual := 4]
    data[hiqul22d == 4, highest_qual := 3]
    data[hiqul22d == 5, highest_qual := 2]
    data[hiqul22d == 6, highest_qual := 1]
    data[hiqul22d == 7, highest_qual := NA]

    data$highest_qual <- as.factor(data$highest_qual)

    data <- subset(data,select = -c(hiqul22d))
  }

  ### 2015-2021 based on hiqul15d
  if("hiqul15d" %in% colnames(data)) {
  data[hiqul15d == 1, highest_qual := 6]
  data[hiqul15d == 2, highest_qual := 5]
  data[hiqul15d == 3, highest_qual := 4]
  data[hiqul15d == 4, highest_qual := 3]
  data[hiqul15d == 5, highest_qual := 2]
  data[hiqul15d == 6, highest_qual := 1]
  data[hiqul15d == 7, highest_qual := NA]

  data$highest_qual <- as.factor(data$highest_qual)

  data <- subset(data,select = -c(hiqul15d))
  }
  ### 2011-2014 based on hiqul11d
  if("hiqul11d" %in% colnames(data)) {
  data[hiqul11d == 1, highest_qual := 6]
  data[hiqul11d == 2, highest_qual := 5]
  data[hiqul11d == 3, highest_qual := 4]
  data[hiqul11d == 4, highest_qual := 3]
  data[hiqul11d == 5, highest_qual := 2]
  data[hiqul11d == 6, highest_qual := 1]
  data[hiqul11d == 7, highest_qual := NA]

  data$highest_qual <- as.factor(data$highest_qual)

  data <- subset(data,select = -c(hiqul11d))
  }
  ### 2008-2010 based on hiqual8d
  if("hiqual8d" %in% colnames(data)) {
    data[hiqual8d == 1, highest_qual := 6]
    data[hiqual8d == 2, highest_qual := 5]
    data[hiqual8d == 3, highest_qual := 4]
    data[hiqual8d == 4, highest_qual := 3]
    data[hiqual8d == 5, highest_qual := 2]
    data[hiqual8d == 6, highest_qual := 1]
    data[hiqual8d == 7, highest_qual := NA]

    data$highest_qual <- as.factor(data$highest_qual)

    data <- subset(data,select = -c(hiqual8d))
  }
  ### 2005q2-2007 based on hiqual5d
  if("hiqual5d" %in% colnames(data)) {
    data[hiqual5d == 1, highest_qual := 6]
    data[hiqual5d == 2, highest_qual := 5]
    data[hiqual5d == 3, highest_qual := 4]
    data[hiqual5d == 4, highest_qual := 3]
    data[hiqual5d == 5, highest_qual := 2]
    data[hiqual5d == 6, highest_qual := 1]
    data[hiqual5d == 7, highest_qual := NA]

    data$highest_qual <- as.factor(data$highest_qual)

    data <- subset(data,select = -c(hiqual5d))
  }
  ### 2004-2005q1 based on hiqual4d
  if("hiqual4d" %in% colnames(data)) {
    data[hiqual4d == 1, highest_qual := 6]
    data[hiqual4d == 2, highest_qual := 5]
    data[hiqual4d == 3, highest_qual := 4]
    data[hiqual4d == 4, highest_qual := 3]
    data[hiqual4d == 5, highest_qual := 2]
    data[hiqual4d == 6, highest_qual := 1]
    data[hiqual4d == 7, highest_qual := NA]

    data$highest_qual <- as.factor(data$highest_qual)

    data <- subset(data,select = -c(hiqual4d))
  }
  ### 1996-2003 based on hiquald
  if("hiquald" %in% colnames(data)) {
  data[hiquald == 1, highest_qual := 6]
  data[hiquald == 2, highest_qual := 5]
  data[hiquald == 3, highest_qual := 4]
  data[hiquald == 4, highest_qual := 3]
  data[hiquald == 5, highest_qual := 2]
  data[hiquald == 6, highest_qual := 1]
  data[hiquald == 7, highest_qual := NA]

  data$highest_qual <- as.factor(data$highest_qual)

  data <- subset(data,select = -c(hiquald))
}
  ### 1993-1995 based on hiquapd
  if("hiquapd" %in% colnames(data)) {
  data[hiquapd == 1, highest_qual := 6]
  data[hiquapd == 2, highest_qual := 5]
  data[hiquapd == 3, highest_qual := 4]
  data[hiquapd == 4, highest_qual := 3]
  data[hiquapd == 5, highest_qual := 2]
  data[hiquapd == 6, highest_qual := 1]
  data[hiquapd == 7, highest_qual := NA]

  data$highest_qual <- as.factor(data$highest_qual)

  data <- subset(data,select = -c(hiquapd))
  }
  # re-order the factor levels

  data$highest_qual <- factor(data$highest_qual,
                              levels = c(1,2,3,4,5,6),
                              labels = c("No qualifications","Below Level 2/Other",
                                         "Level 2","Level 3","Level 4+ Vocational","Degree"))

  ###############################
  ### RETAIN CLEANED VARIABLES

  final_data <- data[, c("obs_id",
                         "highest_qual","edage","exper")]

  var_names <- c("highest_qual","edage","exper")

  setnames(final_data, var_names, paste0("q_", var_names))

  return(final_data)

}

