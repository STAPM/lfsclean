#' Clean demographic data
#'
#' Cleans the raw data for age, gender, ethnicity, region, and marital status.
#'
#' @param data data table. Raw LFS data produced using the reading functions.
#'
#' @return Returns a new set of variables
#' @export
lfs_clean_demographic <- function(
  data
) {

  # Calculate birth cohort
  data[ , cohort := year - age]

  # sex

  data[, sex := factor(sex, levels = 1:2, labels = c("male","female"))]

  data[sex == "male" , male := 1]
  data[sex == "female" , male := 0]

  ######### ethnicity ###############

  if ("ethukeul" %in% colnames(data)) {
    data[ethukeul == 1, ethnicity_4cat := "white"]
    data[ethukeul == 2, ethnicity_4cat := "mixed"]
    data[ethukeul == 8, ethnicity_4cat := "black"]
    data[ethukeul %in% c(3,4,5,6,7,9), ethnicity_4cat := "asian_other"]

    data[ethnicity_4cat == "white", ethnicity_2cat := "white"]
    data[ethnicity_4cat %in% c("mixed","black","asian_other") , ethnicity_2cat := "non_white"]

    data <- subset(data,select = -c(ethukeul))

  }
  if ("eth01" %in% colnames(data)) {
    data[eth01 == 1, ethnicity_4cat := "white"]
    data[eth01 == 2, ethnicity_4cat := "mixed"]
    data[eth01 == 4, ethnicity_4cat := "black"]
    data[eth01 %in% c(3,5,6), ethnicity_4cat := "asian_other"]

    data[ethnicity_4cat == "white", ethnicity_2cat := "white"]
    data[ethnicity_4cat %in% c("mixed","black","asian_other") , ethnicity_2cat := "non_white"]

    data <- subset(data,select = -c(eth01))
  }
  if ("ethcen" %in% colnames(data)) {
    data[ethcen == 0, ethnicity_4cat := "white"]
    data[ethcen %in% c(4,11), ethnicity_4cat := "mixed"]
    data[ethcen %in% c(1,2,3), ethnicity_4cat := "black"]
    data[ethcen %in% c(5,6,7,8,9,10), ethnicity_4cat := "asian_other"]

    data[ethnicity_4cat == "white", ethnicity_2cat := "white"]
    data[ethnicity_4cat %in% c("mixed","black","asian_other") , ethnicity_2cat := "non_white"]

    data <- subset(data,select = -c(ethcen))
  }

  ##### government office region and country variables ######

  if ("govtor" %in% colnames(data)) {
    data[govtor %in% c(1,2) , region := "north_east"]
    data[govtor %in% c(3,4,5) , region := "north_west"]
    data[govtor %in% c(6,7,8) , region := "yorks_and_humber"]
    data[govtor == 9 , region := "east_midlands"]
    data[govtor %in% c(10,11) , region := "west_midlands"]
    data[govtor == 12 , region := "east_of_england"]
    data[govtor %in% c(13,14) , region := "london"]
    data[govtor == 15 , region := "south_east"]
    data[govtor == 16, region := "south_west"]
    data[govtor == 17, region := "wales"]
    data[govtor %in% c(18,19), region := "scotland"]
    data[govtor == 20, region := "northern_ireland"]

    data[govtor %in% c(1:16), country := "england"]
    data[govtor == 17, country := "wales"]
    data[govtor %in% c(18,19), country := "scotland"]
    data[govtor == 20, country := "northern_ireland"]

    data <- subset(data,select = -c(govtor))
  }

  if ("govtof" %in% colnames(data)) {
  data[govtof == 1 , region := "north_east"]
  data[govtof == 2 , region := "north_west"]
  data[govtof == 3 , region := "north_west"]
  data[govtof == 4 , region := "yorks_and_humber"]
  data[govtof == 5 , region := "east_midlands"]
  data[govtof == 6 , region := "west_midlands"]
  data[govtof == 7 , region := "east_of_england"]
  data[govtof == 8 , region := "london"]
  data[govtof == 9 , region := "south_east"]
  data[govtof == 10, region := "south_west"]
  data[govtof == 11, region := "wales"]
  data[govtof == 12, region := "scotland"]
  data[govtof == 13, region := "northern_ireland"]

  data[govtof %in% c(1:10), country := "england"]
  data[govtof == 11, country := "wales"]
  data[govtof == 12, country := "scotland"]
  data[govtof == 13, country := "northern_ireland"]

  data <- subset(data,select = -c(govtof))
  }

  if ("gor9d" %in% colnames(data)) {
    data[gor9d == "E12000001", region := "north_east"]
    data[gor9d == "E12000002", region := "north_west"]
    data[gor9d == "E12000003", region := "yorks_and_humber"]
    data[gor9d == "E12000004", region := "east_midlands"]
    data[gor9d == "E12000005", region := "west_midlands"]
    data[gor9d == "E12000006", region := "east_of_england"]
    data[gor9d == "E12000007", region := "london"]
    data[gor9d == "E12000008", region := "south_east"]
    data[gor9d == "E12000009", region := "south_west"]
    data[gor9d == "W99999999", region := "wales"]
    data[gor9d == "S99999999", region := "scotland"]
    data[gor9d == "N99999999", region := "northern_ireland"]

    data[region %in% c("north_east","north_west","yorks_and_humber",
                       "east_midlands","west_midlands","east_of_england",
                       "london","south_east","south_west")         , country := "england"]
    data[region == "wales"           , country := "wales"]
    data[region == "scotland"        , country := "scotland"]
    data[region == "northern_ireland", country := "northern_ireland"]

    data <- subset(data,select = -c(gor9d))
  }
  # marital status

  if ("marcon" %in% colnames(data)) {
    data[marcon %in% c(2,3), marstat := "single"]
    data[marcon == 1, marstat := "married"]
    data[marcon %in% c(4,5,6), marstat := "sep_div_wid"]

  } else if ("marstt" %in% colnames(data)) {
  data[marstt == 1, marstat := "single"]
  data[marstt == 2, marstat := "married"]
  data[marstt %in% c(3,4,5), marstat := "sep_div_wid"]

  } else if ("marsta" %in% colnames(data)) {
    data[marsta == 1, marstat := "single"]
    data[marsta == 2, marstat := "married"]
    data[marsta %in% c(3,4,5), marstat := "sep_div_wid"]
    data[marsta == 6, marstat := "married"]

  }

  # married or not binary

  if ("mardy6" %in% colnames(data)) {
    data[mardy6 == 1, married_cohabiting := 1]
    data[mardy6 == 2, married_cohabiting := 0]

  } else {

    data[, married_cohabiting := NA]

  }

  # dependent children in the family

  data[is.na(fdpch16) | fdpch16 == 0, dep_children := "no"]
  data[fdpch16 > 0, dep_children := "yes"]

  ### convert the variables to factors
  data$country             <- as.factor(data$country)
  data$region              <- as.factor(data$region)
  data$ethnicity_4cat      <- as.factor(data$ethnicity_4cat)
  data$ethnicity_2cat      <- as.factor(data$ethnicity_2cat)
  data$marstat             <- as.factor(data$marstat)
  data$dep_children        <- as.factor(data$dep_children)


  ###############################
  ### RETAIN CLEANED VARIABLES

  final_data <- data[, c("obs_id",
                         "age", "sex", "ethnicity_2cat", "ethnicity_4cat",
                         "region", "country", "marstat", "married_cohabiting", "dep_children")]

  var_names <- c("age", "sex", "ethnicity_2cat", "ethnicity_4cat",
                 "region", "country", "marstat", "married_cohabiting", "dep_children")

  setnames(final_data, var_names, paste0("d_", var_names))

  return(final_data)

}
