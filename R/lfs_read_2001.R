#' Read LFS 2001 quarters
#'
#' Reads and performs basic cleaning on the Labour Force Survey 2001 quarters.
#'
#' @param root Character - the root directory
#' @param file Character - the file path and name
#'
#'
#' @return Returns a data table
#' @export
lfs_read_2001 <- function(
  root = c("C:/"),
  file = "Users/cm1djm/Documents/Datasets/Labour Force Survey/raw data/"
) {

  path <- here::here(paste0(root[1], file))

  cat(crayon::yellow("Reading LFS 2001:"))

  ###### Read in each quarter
  cat(crayon::green("\tJan - Mar..."))
  data.q1 <- data.table::fread(
    paste0(path ,"/qlfsjm01.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::green("\tApr - Jun..."))
  data.q2 <- data.table::fread(
    paste0(path ,"/qlfsaj01.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::green("\tJul - Sep..."))
  data.q3 <- data.table::fread(
    paste0(path ,"/lfsp_js01_end_user.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::green("\tOct - Dec..."))
  data.q4 <- data.table::fread(
    paste0(path ,"/lfsp_od01_end_user.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  cat(crayon::yellow("\tdone\n"))

  ###### group data tables into a list and initialize a list to store cleaned data tables in

  data.list <- list(data.q1,data.q2,data.q3,data.q4)

  clean.data.list <- list()

  ##### loop the cleaning function over the four quarters

  for (l in c(1)) {
    data <- data.list[[l]]

    setnames(data, names(data), tolower(names(data)))

    weights_vars     <- Hmisc::Cs(pwt07)
    demographic_vars <- Hmisc::Cs(age,sex,govtof,fdpch16)
    education_vars   <- Hmisc::Cs(edage,btec,sctvec,gnvq,nvqlev,rsa,candg,numal,numas,numol,gcse,qgcse,
                                  quals01,quals02,quals03,quals04,quals05,quals06,quals07,quals08,quals09,quals10,quals11,qualch1)
    health_vars      <- Hmisc::Cs(health,discurr,
                                  heal01,heal02,heal03,heal04,heal05,
                                  heal06,heal07,heal08,heal09,heal10)
    work_vars        <- Hmisc::Cs(inecacr,     ftptwk,ttachr,ttushr,      publicr,indm92m,indd92m,inds92m)
    other_vars       <- Hmisc::Cs(refwkm,thiswv)

    names <- c(demographic_vars,education_vars,work_vars, weights_vars,other_vars)
    names <- tolower(names)

    data <- data[ ,names, with=F]

    data$quarter <- l
    data$year <- 2001

    ### tidy data

    # rename variables which have names which change over time but don't need cleaning separately, and variables
    # which don't change over time at all.

    data.table::setnames(data, c("refwkm", "pwt07"),
                         c("month", "pwt"))

    data[btec   %in% c(5,6), btec := NA]
    data[sctvec %in% c(6,7), sctvec := NA]
    data[gnvq   %in% c(6,7), gnvq := NA]
    data[nvqlev %in% c(6,7), nvqlev := NA]
    data[rsa    %in% c(5,6), rsa := NA]
    data[candg  %in% c(4,5), candg := NA]
    data[numol  %in% c(3), numol := NA]
    data[numal  %in% c(3), numal := NA]
    data[numas  %in% c(4), numas := NA]


    clean.data.list[[l]] <- data
  }

  for (l in c(2)) {
    data <- data.list[[l]]

    setnames(data, names(data), tolower(names(data)))

    weights_vars     <- Hmisc::Cs(pwt07,piwt07)
    demographic_vars <- Hmisc::Cs(age,sex,govtof,eth01,marstt,fdpch16)
    education_vars   <- Hmisc::Cs(edage,btec,sctvec,gnvq,nvqlev,rsa,candg,numal,numas,numol,
                                  quals01,quals02,quals03,quals04,quals05,quals06,quals07,quals08,quals09,quals10,quals11,qualch1)
    health_vars      <- Hmisc::Cs(health,discurr,
                                  heal01,heal02,heal03,heal04,heal05,
                                  heal06,heal07,heal08,heal09,heal10)
    work_vars        <- Hmisc::Cs(inecacr,grsswk,ftptwk,ttachr,ttushr,mpnr01,publicr,indm92m,indd92m,inds92m,soc2km,sc2kmmn)
    other_vars       <- Hmisc::Cs(refwkm)

    names <- c(demographic_vars,education_vars,work_vars, weights_vars,other_vars)
    names <- tolower(names)

    data <- data[ ,names, with=F]

    data$quarter <- l
    data$year <- 2001

    ### tidy data

    # rename variables which have names which change over time but don't need cleaning separately, and variables
    # which don't change over time at all.

    data.table::setnames(data, c("refwkm", "pwt07","piwt07"),
                         c("month", "pwt", "piwt"))

    data[btec   %in% c(5,6), btec := NA]
    data[sctvec %in% c(6,7), sctvec := NA]
    data[gnvq   %in% c(6,7), gnvq := NA]
    data[nvqlev %in% c(6,7), nvqlev := NA]
    data[rsa    %in% c(5,6), rsa := NA]
    data[candg  %in% c(4,5), candg := NA]
    data[numol  %in% c(3), numol := NA]
    data[numal  %in% c(3), numal := NA]
    data[numas  %in% c(4), numas := NA]


    clean.data.list[[l]] <- data
  }


  for (l in c(3,4)) {
    data <- data.list[[l]]

    setnames(data, names(data), tolower(names(data)))

    weights_vars     <- Hmisc::Cs(pwt14,piwt14)
    demographic_vars <- Hmisc::Cs(age,sex,govtof,eth01,marstt,fdpch16)
    education_vars   <- Hmisc::Cs(hiqual,hiquald,btec,sctvec,gnvq,nvqlev,rsa,candg,numal,numas,numol,
                                  quals01,quals02,quals03,quals04,quals05,quals06,quals07,quals08,quals09,quals10,quals11,qualch1)
    health_vars      <- Hmisc::Cs(health,discurr,
                                  heal01,heal02,heal03,heal04,heal05,
                                  heal06,heal07,heal08,heal09,heal10)
    work_vars        <- Hmisc::Cs(inecacr,grsswk,ftptwk,ttachr,ttushr,mpnr01,publicr,indm92m,indd92m,inds92m,soc2km,sc2kmmn)
    other_vars       <- Hmisc::Cs(refwkm)

    names <- c(demographic_vars,education_vars,work_vars,health_vars,weights_vars,other_vars)
    names <- tolower(names)

    data <- data[ ,names, with=F]

    data$quarter <- l
    data$year <- 2001

    ### tidy data

    # rename variables which have names which change over time but don't need cleaning separately, and variables
    # which don't change over time at all.

    data.table::setnames(data, c("refwkm", "pwt14","piwt14"),
                         c("month", "pwt", "piwt"))

    data[btec   %in% c(5,6), btec := NA]
    data[sctvec %in% c(6,7), sctvec := NA]
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
