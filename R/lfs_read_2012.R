#' Read LFS 2012 quarters
#'
#' Reads and performs basic cleaning on the Labour Force Survey 2012 quarters.
#'
#' @param root Character - the root directory
#' @param file Character - the file path and name
#'
#'
#' @return Returns a data table
#' @export
lfs_read_2012 <- function(
  root = c("C:/"),
  file = "Users/cm1djm/Documents/Datasets/Labour Force Survey/raw data/"
) {

  path <- here::here(paste0(root[1], file))

  cat(crayon::yellow("Reading LFS 2012:"))

  ###### Read in each quarter
  cat(crayon::green("\tJan - Mar..."))
  data.q1 <- data.table::fread(
    paste0(path ,"/lfsp_jm12_end_user.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::green("\tApr - Jun..."))
  data.q2 <- data.table::fread(
    paste0(path ,"/lfsp_aj12_end_user.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::green("\tJul - Sep..."))
  data.q3 <- data.table::fread(
    paste0(path ,"/lfsp_js12_end_user.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::green("\tOct - Dec..."))
  data.q4 <- data.table::fread(
    paste0(path ,"/lfsp_od12_end_user.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  cat(crayon::yellow("\tdone\n"))

  ###### group data tables into a list and initialize a list to store cleaned data tables in

  data.list <- list(data.q1,data.q2,data.q3,data.q4)

  clean.data.list <- list()

  ##### loop the cleaning function over the four quarters
  for (l in c(1,2,3,4)) {
    data <- data.list[[l]]

    setnames(data, names(data), tolower(names(data)))

    weights_vars     <- Hmisc::Cs(pwt18,piwt18)
    demographic_vars <- Hmisc::Cs(age,sex,govtof,ethukeul,marsta,fdpch16)
    education_vars   <- Hmisc::Cs(edage,hiqul11d,hiqual11,bte11,sctvc11,gnvq11,nvq11,rsa11,cag11,numol5,numal,numas,hst,advhst,typhst1,typhst2,typhst3,typhst4,typhst5,
                                  gcseful1,gcseful2,gcseful3,gcseful4,qgcse41,qgcse42,qgcse43,qgcse44,
                                  qual_1,qual_2,qual_3,qual_4,qual_5,qual_6,qual_7,qual_8,qual_9,qual_10,qual_11,qual_12,qual_13,qual_14,
                                  qual_15,qual_16,qual_17,qual_18,qual_19,qual_20,qual_21,qual_22,qual_23,qual_24,qual_25,qual_26,qual_27,
                                  qual_28,qual_29,qual_30,qual_31)
    health_vars      <- Hmisc::Cs(health,discurr,
                                  heal01,heal02,heal03,heal04,heal05,
                                  heal06,heal07,heal08,heal09,heal10)
    work_vars        <- Hmisc::Cs(inecac05,grsswk,ftptwk,ttachr,ttushr,mpnr02,publicr,indc07m,indd07m,inds07m,soc10m,sc10mmn,
                                  undemp,undhrs,ovhrs,lespay2)
    other_vars       <- Hmisc::Cs(refwkm,thiswv)

    names <- c(demographic_vars,education_vars,work_vars, weights_vars,other_vars)
    names <- tolower(names)

    data <- data[ ,names, with=F]

    data$quarter <- l
    data$year <- 2012

    ### tidy data

    # rename variables which have names which change over time but don't need cleaning separately, and variables
    # which don't change over time at all.

    data.table::setnames(data, c("refwkm", "pwt18","piwt18"),
                         c("month", "pwt", "piwt"))


    clean.data.list[[l]] <- data
  }

  ### combine quarters into a single data table

  data <- rbind(clean.data.list[[1]],clean.data.list[[2]],clean.data.list[[3]],clean.data.list[[4]],fill=TRUE)

  data <- data.table(data)

  return(data)
}
