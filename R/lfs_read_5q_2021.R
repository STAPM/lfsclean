#' Read LFS 2021 longitudinal
#'
#' Reads and performs basic cleaning on the Labour Force Survey five-quarter longitudinal
#' data with a 1st wave that began in calendar year 2021.
#'
#' @param root Character - the root directory
#' @param file Character - the file path and name
#'
#'
#' @return Returns a data table
#' @export
lfs_read_5q_2021 <- function(
    root = c("C:/"),
    file = "Users/cm1djm/Documents/Data/LFS/longitudinal/tab/"
) {

  path <- here::here(paste0(root[1], file))

  cat(crayon::yellow("Reading LFS 2021:\n"))

  ###### Read in each quarter
  cat(crayon::green("\tJan-Mar 2021 to Jan-Mar 2022"))
  data.q1 <- data.table::fread(
    paste0(path,"/lgwt22_5q_jm21_jm22_eul.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::yellow("\tdone\n"))


  cat(crayon::green("\tApr-Jun 2021 to Apr-Jun 2022"))
  data.q2 <- data.table::fread(
    paste0(path,"/lgwt22_5q_aj21_aj22_eul.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::yellow("\tdone\n"))


  cat(crayon::green("\tJul-Sep 2021 to Jul-Sep 2022"))
  data.q3 <- data.table::fread(
    paste0(path,"/lgwt22_5q_js21_js22_eul.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::yellow("\tdone\n"))


  cat(crayon::green("\tOct-Dec 2021 to Oct-Dec 2022"))
  data.q4 <- data.table::fread(
    paste0(path,"/lgwt22_5q_od21_od22_eul.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::yellow("\tdone\n"))

  ###### no qualification data for some waves, add in as NA

  data.q1[, HIQUL22D1 := NA]
  data.q1[, HIQUL22D2 := NA]
  data.q1[, HIQUL22D3 := NA]
  data.q1[, HIQUL22D4 := NA]

  data.q2[, HIQUL22D1 := NA]
  data.q2[, HIQUL22D2 := NA]
  data.q2[, HIQUL22D3 := NA]
  data.q2[, HIQUL22D4 := NA]

  data.q3[, HIQUL22D1 := NA]
  data.q3[, HIQUL22D2 := NA]

  data.q4[, HIQUL22D1 := NA]
  data.q4[, HIQUL22D2 := NA]

  ###### group data tables into a list and initialize a list to store cleaned data tables in

  data.list <- list(data.q1, data.q2, data.q3, data.q4)

  clean.data.list <- list()

  ##### loop the cleaning function over the four quarters
  for (l in c(1:4)) {
    data <- data.list[[l]]

    setnames(data, names(data), tolower(names(data)))

    id_weights_vars  <- Hmisc::Cs(persid, lgwt22)

    demographic_vars <- Hmisc::Cs(sex,
                                  age1, age2, age3, age4, age5,
                                  etukeul1, etukeul2, etukeul3, etukeul4, etukeul5,
                                  govtof21, govtof22, govtof23, govtof24, govtof25)

    education_vars   <- Hmisc::Cs(hiqul22d1, hiqul22d2, hiqul22d3, hiqul22d4, hiqul22d5)

    empstat_vars        <- Hmisc::Cs(ilodefr1, ilodefr2, ilodefr3, ilodefr4, ilodefr5,
                                     incac051, incac052, incac053, incac054, incac055)

    health_vars      <- Hmisc::Cs(disea1, disea2, disea3, disea4, disea5,
                                  illoff1, illoff2, illoff3, illoff4, illoff5)

    benefit_vars     <- Hmisc::Cs(ucredit1, ucredit2, ucredit3, ucredit4, ucredit5,
                                  clims141, clims142, clims143, clims144, clims145,
                                  benfts1, benfts2, benfts3, benfts4, benfts5,
                                  ooben1, ooben2, ooben3, ooben4, ooben5)

    work_vars     <- Hmisc::Cs(ttushr1, ttushr2, ttushr3, ttushr4, ttushr5,
                               grsswk1, grsswk2, grsswk3, grsswk4, grsswk5)

    names <- c(id_weights_vars, demographic_vars, education_vars, empstat_vars,
               work_vars, health_vars, benefit_vars)

    data <- data[ ,names, with=F]

    data$quarter <- l
    data$year <- 2021

    clean.data.list[[l]] <- data
  }

  ### combine quarters into a single data table

  data <- rbind(clean.data.list[[1]],
                clean.data.list[[2]],
                clean.data.list[[3]],
                clean.data.list[[4]], fill=TRUE)

  setDT(data)

  return(data)
}
