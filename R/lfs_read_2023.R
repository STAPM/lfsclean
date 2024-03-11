#' Read LFS 2023 quarters
#'
#' Reads and performs basic cleaning on the Labour Force Survey 2023 quarters.
#'
#' @param root Character - the root directory
#' @param file Character - the file path and name
#'
#'
#' @return Returns a data table
#' @export
lfs_read_2023 <- function(
    root = c("C:/"),
    file = "Users/damon/OneDrive/Documents/Datasets/Labour Force Survey/tab/"
) {

  path <- here::here(paste0(root[1], file))

  cat(crayon::yellow("Reading LFS 2023:"))

  ###### Read in each quarter
  cat(crayon::green("\tJan - Mar..."))
  data.q1 <- data.table::fread(
    paste0(path,"/lfsp_jm23_eul_pwt22.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::green("\tApr - Jun..."))
  data.q2 <- data.table::fread(
    paste0(path,"/lfsp_aj23_eul_pwt22.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

    cat(crayon::green("\tJul - Sep..."))
    data.q3 <- data.table::fread(
      paste0(path,"/lfsp_js23_eul_pwt23.tab"), showProgress = FALSE,
      na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
    )

    cat(crayon::green("\tOct - Dec..."))
    data.q4 <- data.table::fread(
      paste0(path,"/lfsp_od23_eul_pwt23.tab"), showProgress = FALSE,
      na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
    )
  cat(crayon::yellow("\tdone\n"))

  ###### group data tables into a list and initialize a list to store cleaned data tables in

  data.list <- list(data.q1, data.q2, data.q3, data.q4)

  clean.data.list <- list()

  ##### loop the cleaning function over the four quarters
  for (l in c(1:2)) {
    data <- data.list[[l]]

    setnames(data, names(data), tolower(names(data)))

    weights_vars     <- Hmisc::Cs(pwt22,piwt22)
    demographic_vars <- Hmisc::Cs(age,sex,gor9d,ethukeul,marsta,fdpch16)
    education_vars   <- Hmisc::Cs(edage,hiqul22d,hiqual22)
    health_vars      <- Hmisc::Cs(health20,discurr20,disea)
    work_vars        <- Hmisc::Cs(inecac05,grsswk,ftptwk,ttachr,ttushr,mpnr02,publicr,indc07m,indd07m,inds07m,soc20m,sc20mmn,
                                  undemp,undhrs,ovhrs,lespay2)
    benefit_vars     <- Hmisc::Cs(ooben)
    other_vars       <- Hmisc::Cs(refwkm,thiswv)

    names <- c(demographic_vars,education_vars,health_vars,work_vars,benefit_vars,weights_vars,other_vars)
    names <- tolower(names)

    data <- data[ ,names, with=F]

    data$quarter <- l
    data$year <- 2023

    ### tidy data

    # rename variables which have names which change over time but don't need cleaning separately, and variables
    # which don't change over time at all.

    data.table::setnames(data,
                         c("refwkm", "pwt22","piwt22"),
                         c("month", "pwt", "piwt") )

    data.table::setnames(data, c("health20","discurr20"), c("health","discurr"))

    clean.data.list[[l]] <- data
  }


  for (l in c(3)) {
    data <- data.list[[l]]

    setnames(data, names(data), tolower(names(data)))

    weights_vars     <- Hmisc::Cs(pwt23,piwt22)
    demographic_vars <- Hmisc::Cs(age,sex,gor9d,ethukeul,marsta,fdpch16)
    education_vars   <- Hmisc::Cs(edage,hiqul22d,hiqual22)
    health_vars      <- Hmisc::Cs(health20,discurr20,disea)
    work_vars        <- Hmisc::Cs(inecac05,grsswk,ftptwk,ttachr,ttushr,mpnr02,publicr,indc07m,indd07m,inds07m,soc20m,sc20mmn,
                                  undemp,undhrs,ovhrs,lespay2)
    benefit_vars     <- Hmisc::Cs(ooben)
    other_vars       <- Hmisc::Cs(refwkm,thiswv)

    names <- c(demographic_vars,education_vars,health_vars,work_vars,benefit_vars,weights_vars,other_vars)
    names <- tolower(names)

    data <- data[ ,names, with=F]

    data$quarter <- l
    data$year <- 2023

    ### tidy data

    # rename variables which have names which change over time but don't need cleaning separately, and variables
    # which don't change over time at all.

    data.table::setnames(data,
                         c("refwkm", "pwt23","piwt22"),
                         c("month", "pwt", "piwt") )


    clean.data.list[[l]] <- data
  }





  for (l in c(4)) {
    data <- data.list[[l]]

    setnames(data, names(data), tolower(names(data)))

    weights_vars     <- Hmisc::Cs(pwt23,piwt23)
    demographic_vars <- Hmisc::Cs(age,sex,gor9d,ethukeul,marsta,fdpch16)
    education_vars   <- Hmisc::Cs(edage,hiqul22d,hiqual22)
    health_vars      <- Hmisc::Cs(health20,discurr20,disea)
    work_vars        <- Hmisc::Cs(inecac05,grsswk,ftptwk,ttachr,ttushr,mpnr02,publicr,indc07m,indd07m,inds07m,soc20m,sc20mmn,
                                  undemp,undhrs,ovhrs,lespay2)
    benefit_vars     <- Hmisc::Cs(ooben)
    other_vars       <- Hmisc::Cs(refwkm,thiswv)

    names <- c(demographic_vars,education_vars,health_vars,work_vars,benefit_vars,weights_vars,other_vars)
    names <- tolower(names)

    data <- data[ ,names, with=F]

    data$quarter <- l
    data$year <- 2023

    ### tidy data

    # rename variables which have names which change over time but don't need cleaning separately, and variables
    # which don't change over time at all.

    data.table::setnames(data,
                         c("refwkm", "pwt23","piwt23"),
                         c("month", "pwt", "piwt") )


    clean.data.list[[l]] <- data
  }


  ### combine quarters into a single data table

  data <- rbind(clean.data.list[[1]],
                clean.data.list[[2]],
                clean.data.list[[3]],
                clean.data.list[[4]],
                fill=TRUE)

  data <- setDT(data)

  return(data)
}
