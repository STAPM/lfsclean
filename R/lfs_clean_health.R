#' Clean health data
#'
#' Cleans the raw data for health and health related variables.
#'
#' @return Returns a new set of variables
#' @export
lfs_clean_health <- function(
    data
) {

  # Disability (under the Equality Act 2010)
  if ("disea" %in% colnames(data)) {
  data[disea == 1 , disability := "disabled"]
  data[disea == 2 , disability := "not_disabled"]
  } else {

  data[, disability := NULL]
  }

  # Main health problem/disability

  if ("health" %in% colnames(data)){
  data[ , health_condition := "none"]
  data[health == 1 , health_condition := "msk_arms_hands"]
  data[health == 2 , health_condition := "msk_legs_feet"]
  data[health == 3 , health_condition := "msk_back_neck"]
  data[health == 4 , health_condition := "difficulty_seeing"]
  data[health == 5 , health_condition := "difficulty_hearing"]
  data[health == 6 , health_condition := "speech_impediment"]
  data[health == 7 , health_condition := "skin_conditions_disfigurement"]
  data[health == 8 , health_condition := "respiratory_problems"]
  data[health == 9 , health_condition := "heart_blood_circulatory_problems"]
  data[health == 10 , health_condition := "digestive_problems"]
  data[health == 11 , health_condition := "diabetes"]
  data[health == 12 , health_condition := "depression_anxiety"]
  data[health == 13 , health_condition := "epilepsy"]
  data[health == 18 , health_condition := "autism"]
  data[health == 14 , health_condition := "learning_difficulties"]
  data[health == 15 , health_condition := "mental_illness"]
  data[health == 16 , health_condition := "progressive_illness_nec"]
  data[health == 17 , health_condition := "other_problems"]
  } else {

  data[, health_condition := NULL]
  }

  ### convert the variables to factors
  data$disability          <- as.factor(data$disability)
  data$health_condition    <- as.factor(data$health_condition)

  ###############################
  ### RETAIN CLEANED VARIABLES

  final_data <- data[, c("obs_id",
                         "disability", "health_condition")]

  var_names <- c("disability", "health_condition")

  setnames(final_data, var_names, paste0("h_", var_names))

  return(final_data)

}
