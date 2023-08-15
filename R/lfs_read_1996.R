#' Read LFS 1996 quarters
#'
#' Reads and performs basic cleaning on the Labour Force Survey 1996 quarters.
#'
#' @param root Character - the root directory
#' @param file Character - the file path and name
#'
#'
#' @return Returns a data table
#' @export
lfs_read_1996 <- function(
  root = c("C:/"),
  file = "Users/cm1djm/Documents/Datasets/Labour Force Survey/raw data/"
) {

  path <- here::here(paste0(root[1], file))

  cat(crayon::yellow("Reading LFS 1996:"))

  ###### Read in each quarter
  cat(crayon::green("\tJan - Mar..."))
  data.q1 <- data.table::fread(
    paste0(path ,"/qlfsjm96.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-5", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::green("\tApr - Jun..."))
  data.q2 <- data.table::fread(
    paste0(path ,"/qlfsaj96.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-5", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::green("\tJul - Sep..."))
  data.q3 <- data.table::fread(
    paste0(path ,"/qlfsjs96.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-5", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::green("\tOct - Dec..."))
  data.q4 <- data.table::fread(
    paste0(path ,"/qlfsod96.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-5", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  cat(crayon::yellow("\tdone\n"))

  ###### group data tables into a list and initialize a list to store cleaned data tables in

  data.list <- list(data.q1,data.q2,data.q3,data.q4)

  clean.data.list <- list()

  ##### loop the cleaning function over the four quarters

  for (l in c(1)) {
    data <- data.list[[l]]

    setnames(data, names(data), tolower(names(data)))

    # hiquap and hiqual variables in the same quarter, convert hiquap to hiqual

      data[hiquapd == 1, hiquald := 1]
      data[hiquapd == 2, hiquald := 2]
      data[hiquapd == 3, hiquald := 3]
      data[hiquapd == 4, hiquald := 4]
      data[hiquapd == 5, hiquald := 5]
      data[hiquapd == 6, hiquald := 6]

      data[hiquap == 1, hiqual := 1]
      data[hiquap == 2, hiqual := 3]
      data[hiquap == 3, hiqual := 4]
      data[hiquap == 4, hiqual := 6]
      data[hiquap == 5, hiqual := 7]
      data[hiquap == 6, hiqual := 8]
      data[hiquap == 7, hiqual := 9]
      data[hiquap == 8, hiqual := 10]
      data[hiquap == 9, hiqual := 11]
      data[hiquap == 10, hiqual := 12]
      data[hiquap == 11, hiqual := 14]
      data[hiquap == 12, hiqual := 13]
      data[hiquap == 13, hiqual := 17]
      data[hiquap == 14, hiqual := 18]
      data[hiquap == 15, hiqual := 19]
      data[hiquap == 16, hiqual := 20]
      data[hiquap == 17, hiqual := 21]
      data[hiquap == 18, hiqual := 22]
      data[hiquap == 19, hiqual := 23]
      data[hiquap == 20, hiqual := 24]
      data[hiquap == 21, hiqual := 27]
      data[hiquap == 22, hiqual := 28]
      data[hiquap == 23, hiqual := 29]
      data[hiquap == 24, hiqual := 30]
      data[hiquap == 25, hiqual := 33]
      data[hiquap == 26, hiqual := 34]
      data[hiquap == 27, hiqual := 38]
      data[hiquap == 28, hiqual := 35]
      data[hiquap == 29, hiqual := 36]
      data[hiquap == 30, hiqual := 37]
      data[hiquap == 31, hiqual := 39]
      data[hiquap == 32, hiqual := 40]

    weights_vars     <- Hmisc::Cs(pwt07,piwt07)
    demographic_vars <- Hmisc::Cs(age,sex,govtor,ethcen,marstt)
    education_vars   <- Hmisc::Cs(edage,hiqual,hiquald,numal,numas,numol,
                                  quals01,quals02,quals03,quals04,quals05,quals06,quals07,quals08,quals09,quals10,quals11,
                                  quals12,quals13,quals14,quals15,qualch0)
    work_vars        <- Hmisc::Cs(inecacr,grsswk,ftptwk,ttachr,ttushr,mpnor,publicr,indm92m,indd92m,inds92m,socmain,socminm)
    other_vars       <- Hmisc::Cs(refwkm,thiswv)

    names <- c(demographic_vars,education_vars,work_vars, weights_vars,other_vars)
    names <- tolower(names)

    data <- data[ ,names, with=F]

    data$quarter <- l
    data$year <- 1996

    ### tidy data

    # rename variables which have names which change over time but don't need cleaning separately, and variables
    # which don't change over time at all.

    data.table::setnames(data, c("refwkm", "pwt07","piwt07"),
                         c("month", "pwt", "piwt"))

    data[numol  %in% c(3), numol := NA]
    data[numal  %in% c(3), numal := NA]
    data[numas  %in% c(4), numas := NA]


    clean.data.list[[l]] <- data
  }

  for (l in c(2,3,4)) {
    data <- data.list[[l]]

    setnames(data, names(data), tolower(names(data)))

    weights_vars     <- Hmisc::Cs(pwt07,piwt07)
    demographic_vars <- Hmisc::Cs(age,sex,govtor,ethcen,marstt)
    education_vars   <- Hmisc::Cs(edage,hiqual,hiquald,btec,gnvq,nvqlev,rsa,candg,numal,numas,numol,
                                  quals01,quals02,quals03,quals04,quals05,quals06,quals07,quals08,quals09,quals10,quals11,
                                  quals12,quals13,quals14,quals15,qualch0)
    work_vars        <- Hmisc::Cs(inecacr,grsswk,ftptwk,ttachr,ttushr,mpnor,publicr,indm92m,indd92m,inds92m,socmain,socminm)
    other_vars       <- Hmisc::Cs(refwkm,thiswv)

    names <- c(demographic_vars,education_vars,work_vars, weights_vars,other_vars)
    names <- tolower(names)

    data <- data[ ,names, with=F]

    data$quarter <- l
    data$year <- 1996

    ### tidy data

    # rename variables which have names which change over time but don't need cleaning separately, and variables
    # which don't change over time at all.

    data.table::setnames(data, c("refwkm", "pwt07","piwt07"),
                         c("month", "pwt", "piwt"))

    data[btec   %in% c(5,6), btec := NA]
    data[gnvq   %in% c(6,7), gnvq := NA]
    data[nvqlev %in% c(6,7), nvqlev := NA]
    data[rsa    %in% c(5,6), rsa := NA]
    data[candg  %in% c(4,5), candg := NA]
    data[numol  %in% c(3), numol := NA]
    data[numal  %in% c(3), numal := NA]
    data[numas  %in% c(4), numas := NA]


    clean.data.list[[l]] <- data
  }

  ### combine quarters into a single data table

  data <- rbind(clean.data.list[[1]],clean.data.list[[2]],clean.data.list[[3]],clean.data.list[[4]],fill=TRUE)

  data <- data.table(data)

  return(data)
}
