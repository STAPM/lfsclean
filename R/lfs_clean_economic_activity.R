#' Clean economic activity data
#'
#' Produce variables indicating employment status, and for the employed also full/part-time status,
#' industry, occupation, public/private sector, and firm size.
#'
#' @return Returns a new set of variables
#' @export
lfs_clean_economic_activity <- function(
  data
) {

  #################################
  ### Economic status variables ###

  # use inecac05 - detailed break down of economic status
if ("inecac05" %in% colnames(data)) {
  data[inecac05 %in% c(1:3,4), lmstatus_8cat := "employed"]
  data[inecac05 %in% c(2), lmstatus_8cat := "self_employed"]
  data[inecac05 %in% c(20,31), lmstatus_8cat := "retired"]
  data[inecac05 %in% c(5), lmstatus_8cat := "unemployed"]
  data[inecac05 %in% c(8:9,15:16,26:27), lmstatus_8cat := "sick"]
  data[inecac05 %in% c(7,14,25), lmstatus_8cat := "caring"]
  data[inecac05 %in% c(6,13,24), lmstatus_8cat := "education"]
  data[inecac05 %in% c(10:11,17:19,21:23,28:30,32:34), lmstatus_8cat := "other"]

  data[inecac05 %in% c(1:4), lmstatus_3cat := "employed"]
  data[inecac05 %in% c(5), lmstatus_3cat := "unemployed"]
  data[inecac05 %in% c(6:34), lmstatus_3cat := "inactive"]

  data[inecac05 %in% c(1:4), lmstatus_2cat := "employed"]
  data[inecac05 %in% c(5:34), lmstatus_2cat := "non_employed"]


  data <- subset(data,select = -c(inecac05))

}

  # use inecacr - detailed break down of economic status
  if ("inecacr" %in% colnames(data)) {
    data[inecacr %in% c(1,3:4), lmstatus_8cat := "employed"]
    data[inecacr %in% c(2), lmstatus_8cat := "self_employed"]
    data[inecacr %in% c(27), lmstatus_8cat := "retired"]
    data[inecacr %in% c(5), lmstatus_8cat := "unemployed"]
    data[inecacr %in% c(8:9,15:16,24:25), lmstatus_8cat := "sick"]
    data[inecacr %in% c(7,14,23), lmstatus_8cat := "caring"]
    data[inecacr %in% c(6,13,22), lmstatus_8cat := "education"]
    data[inecacr %in% c(10:12,17:21,26,28:30), lmstatus_8cat := "other"]

    data[inecacr %in% c(1:4), lmstatus_3cat := "employed"]
    data[inecacr %in% c(5), lmstatus_3cat := "unemployed"]
    data[inecacr %in% c(6:34), lmstatus_3cat := "inactive"]

    data[inecacr %in% c(1:4), lmstatus_2cat := "employed"]
    data[inecacr %in% c(5:30), lmstatus_2cat := "non_employed"]

    data <- subset(data,select = -c(inecacr))
  }

  data[, lmstatus_2cat := factor(lmstatus_2cat, levels = c("employed","non_employed"))]
  data[, lmstatus_3cat := factor(lmstatus_3cat, levels = c("employed","unemployed","inactive"))]
  data[, lmstatus_8cat := factor(lmstatus_8cat, levels = c("employed","self_employed","retired","unemployed",
                                                           "sick","caring","education","other"))]

  ##################################
  # full-time/part- time status ####

  data[age >= 16 & ftptwk == 1, full_time := "full_time"]
  data[age >= 16 & ftptwk == 2, full_time := "part_time"]

  data[, full_time := factor(full_time, levels = c("full_time","part_time")) ]

  ############################
  ### INDUSTRY VARIABLES #####

  # industry 4, 2, and 1 digit SIC-2007 years
  if ("indc07m" %in% colnames(data)) {
  data$sic2007code <- as.factor(data$indc07m)
  #data$sic2007_2dig <- as.factor(data$indd07m)
  #data$sic2007_1dig <- as.factor(data$inds07m)

  } else {
    data$sic2007code <- NA
    #data$sic2007_2dig <- NA
    #data$sic2007_1dig <- NA

  }
  # industry 4, 2, and 1 digit SIC-1992 years
  if ("indm92m" %in% colnames(data)) {
    data$sic1992code <- as.factor(data$indm92m)
    #data$sic1992_2dig <- as.factor(data$indd92m)
   # data$sic1992_1dig <- as.factor(data$inds92m)

  } else {
    data$sic1992code <- NA
    #data$sic1992_2dig <- NA
    #data$sic1992_1dig <- NA

  }

  #############################
  ### Create a consistent SIC2007 section variable from Jennifer Smiths
  ### SIC mapping do-file (originally ONS LFS team). Based on -indm92m-

  if ("indm92m" %in% colnames(data)) {
  data <- lfsclean::RecodeSIC92to07(data)
  data[, ind_section := indm92m07s]

  } else {
  data[, ind_section := NA]

  }

  if ("inds007m" %in% colnames(data)) {
  data[is.na(ind_section), ind_section := inds07m]

  }

  data[, ind_section := factor(ind_section,
                               levels = 1:21,
                               labels = c("A) Agriculture, Forestry, and Fishing",
                                          "B) Mining and Quarrying",
                                          "C) Manufacturing",
                                          "D) Electricity, Gas, Steam, and Air Conditioning Supply",
                                          "E) Water Supply; Sewerage, Waste Management, and Remediation Activities",
                                          "F) Construction",
                                          "G) Wholesale and Retail Trade; Repair of Motor Vehicles and Motorcycles",
                                          "H) Transportation and Storage",
                                          "I) Accommodation and Food Service Activities",
                                          "J) Information and Communication",
                                          "K) Financial and Insurance Activities",
                                          "L) Real Estate Activities",
                                          "M) Professional, Scientific and Technical Activities",
                                          "N) Administrative and Support Service Activities",
                                          "O) Public Administration and Defence; Compulsory Social Security",
                                          "P) Education",
                                          "Q) Human Health and Social Work Activities",
                                          "R) Arts, Entertainment and Recreation",
                                          "S) Other Service Activities",
                                          "T) Activities of Households as Employers",
                                          "U) Activities of Extraterritorial Organisation and Bodies"))]

  ############################
  ### OCCUPATION #############

  # occupation - 3/4 digit - SOC2020
  if ("sc20mmn" %in% colnames(data)) {
    data$soc2020code <- as.character(data$soc20m)

  } else {
    data$soc2020code <- NA

  }

  # occupation - 3/4 digit - SOC2010
  if ("sc10mmn" %in% colnames(data)) {
    data$soc2010code <- as.character(data$soc10m)

  } else {
    data$soc2010code <- NA

  }
  # occupation - 3/4 digit - SOC2000

  if ("sc2kmmn" %in% colnames(data)) {
    data$soc2000code <- as.character(data$soc2km)

  } else {
    data$soc2000code <- NA

  }
  # occupation - 3/4 digit - SOC1990

  if ("socmain" %in% colnames(data)) {
    data$soc1990code <- as.character(data$socmain)

  } else {
    data$soc1990code <- NA

  }

  ###############################
  # public/private sector #######

  if("publicr" %in% colnames(data)) {
  data[age >= 16 & publicr == 1, sector := "private_sector"]
  data[age >= 16 & publicr == 2, sector := "public_sector"]

  } else {

  data[, sector := NA]
  }

  data[, sector := as.factor(sector)]

  #################
  ### firm size ###

  if ("mpnr02" %in% colnames(data)) {
    data[mpnr02 %in% c(1,2,3,4), firm_size := "1_25"]
    data[mpnr02 %in% 5         , firm_size := "25_49"]
    data[mpnr02 %in% c(6,7,8,9), firm_size := "50+"]

  } else if ("mpnr01" %in% colnames(data)) {
    data[mpnr01 %in% c(1,2,3,4), firm_size := "1_25"]
    data[mpnr01 %in% 5         , firm_size := "25_49"]
    data[mpnr01 %in% c(6,7,8), firm_size := "50+"]

  } else if ("mpnor" %in% colnames(data)) {
    data[mpnor %in% c(1,2,3,4), firm_size := "1_25"]
    data[mpnor %in% 5         , firm_size := "25_49"]
    data[mpnor %in% c(6,7), firm_size := "50+"]

  } else {

    data[, firm_size := NA]
  }

  data[, firm_size := as.factor(firm_size)]


  ###############################
  ### RETAIN CLEANED VARIABLES

  final_data <- data[, c("obs_id",
                         "lmstatus_8cat", "lmstatus_3cat", "lmstatus_2cat",
                         "sic2007code", "ind_section",
                         "firm_size", "sector", "full_time",
                         "soc2020code", "soc2010code", "soc2000code", "soc1990code")]

  var_names <- c("lmstatus_8cat", "lmstatus_3cat", "lmstatus_2cat",
                 "sic2007code", "ind_section",
                 "firm_size", "sector", "full_time",
                 "soc2020code", "soc2010code", "soc2000code", "soc1990code")

  setnames(final_data, var_names, paste0("l_", var_names))

  return(final_data)
}
