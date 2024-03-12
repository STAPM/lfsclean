#' Read LFS 2020 quarters
#'
#' Reads and performs basic cleaning on the Labour Force Survey 2020 quarters.
#'
#' @param root Character - the root directory
#' @param file Character - the file path and name
#'
#'
#' @return Returns a data table
#' @export
lfs_read_2020 <- function(
  root = c("C:/"),
  file = "Users/cm1djm/Documents/Datasets/Labour Force Survey/raw data/"
) {

  path <- here::here(paste0(root[1], file))

  cat(crayon::yellow("Reading LFS 2020:"))

  ###### Read in each quarter
  cat(crayon::green("\tJan - Mar..."))
  data.q1 <- data.table::fread(
    paste0(path, "/lfsp_jm20_eul_pwt22.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::green("\tApr - Jun..."))
  data.q2 <- data.table::fread(
    paste0(path, "/lfsp_aj20_eul_pwt22.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::green("\tJul - Sep..."))
  data.q3 <- data.table::fread(
    paste0(path, "/lfsp_js20_eul_pwt22.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )

  cat(crayon::green("\tOct - Dec..."))
  data.q4 <- data.table::fread(
    paste0(path, "/lfsp_od20_eul_pwt22.tab"), showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  cat(crayon::yellow("\tdone\n"))

  ###### group data tables into a list and initialize a list to store cleaned data tables in

  data.list <- list(data.q1,data.q2,data.q3,data.q4)

  clean.data.list <- list()

  ##### loop the cleaning function over the four quarters
  for (l in c(1:4)) {
    data <- data.list[[l]]

    setnames(data, names(data), tolower(names(data)))

    weights_vars     <- Hmisc::Cs(pwt22,piwt22)
    demographic_vars <- Hmisc::Cs(age,sex,gor9d,ethukeul,marsta,fdpch16)
    education_vars   <- Hmisc::Cs(edage,hiqul15d,hiqual15,bte11,sctvc11,gnvq11,nvq11,rsa11,cag11,numol5,numal,numas,hst,advhst,typhst1,typhst2,typhst3,typhst4,typhst5,
                                  gcseful1,gcseful2,gcseful3,gcseful4,qgcse41,qgcse42,qgcse43,qgcse44,
                                  qual_1,qual_2,qual_3,qual_4,qual_5,qual_6,qual_7,qual_8,qual_9,qual_10,qual_11,qual_12,qual_13,qual_14,
                                  qual_15,qual_16,qual_17,qual_18,qual_19,qual_20,qual_21,qual_22,qual_23,qual_24,qual_25,qual_26,qual_27,
                                  qual_28,qual_29,qual_30,qual_31)
    health_vars      <- Hmisc::Cs(health20,discurr20,disea,
                                  heal2001,heal2002,heal2003,heal2004,heal2005,
                                  heal2006,heal2007,heal2008,heal2009,heal2010)
    work_vars        <- Hmisc::Cs(inecac05,grsswk,ftptwk,ttachr,ttushr,mpnr02,publicr,indc07m,indd07m,inds07m,soc10m,sc10mmn)
    benefit_vars     <- Hmisc::Cs(ooben)
    other_vars       <- Hmisc::Cs(refwkm,thiswv)

    names <- c(demographic_vars,education_vars,health_vars,work_vars,benefit_vars,weights_vars,other_vars)
    names <- tolower(names)

    data <- data[ ,names, with=F]

    data$quarter <- l
    data$year <- 2020

    ### tidy data

    # rename variables which have names which change over time but don't need cleaning separately, and variables
    # which don't change over time at all.

    data.table::setnames(data, c("refwkm", "pwt22","piwt22","bte11","sctvc11","gnvq11","nvq11","rsa11","cag11","numol5",
                                 "qual_1","qual_2","qual_3","qual_4","qual_5","qual_6","qual_7","qual_8","qual_9","qual_10",
                                 "qual_11","qual_12","qual_13","qual_14","qual_15","qual_16","qual_17","qual_18","qual_19","qual_20","qual_21","qual_22",
                                 "qual_23","qual_24","qual_25","qual_26","qual_27","qual_28","qual_29","qual_30","qual_31"),
                         c("month", "pwt", "piwt","btec","sctvec","gnvq","nvqlev","rsa","candg","numol",
                           "qf1","qf2","qf3","qf4","qf5","qf6","qf7","qf8","qf9","qf10",
                           "qf11","qf12","qf13","qf14", "qf15","qf16","qf17","qf18","qf19","qf20","qf21","qf22",
                           "qf23","qf24","qf25","qf26","qf27","qf28","qf29","qf30","qf31") )

    data.table::setnames(data,
                         c("health20","discurr20",
                           "heal2001","heal2002","heal2003","heal2004","heal2005",
                           "heal2006","heal2007","heal2008","heal2009","heal2010"),
                         c("health","discurr",
                           "heal01","heal02","heal03","heal04","heal05",
                           "heal06","heal07","heal08","heal09","heal10"))

    #preliminary cleaning of the vocational qualification variables
    data[btec   %in% c(5,6), btec := NA]
    data[sctvec %in% c(6,7), sctvec := NA]
    data[gnvq   %in% c(6,7), gnvq := NA]
    data[nvqlev %in% c(6,7), nvqlev := NA]
    data[rsa    %in% c(5,6), rsa := NA]
    data[candg  %in% c(4,5), candg := NA]
    data[numol  %in% c(3), numol := NA]
    data[numal  %in% c(3), numal := NA]
    data[numas  %in% c(4), numas := NA]
    data[hst    %in% c(3), hst := NA]
    data[advhst %in% c(3), advhst := NA]

    clean.data.list[[l]] <- data
  }

  ### combine quarters into a single data table

  data <- rbind(clean.data.list[[1]],
                clean.data.list[[2]],
                clean.data.list[[3]],
                clean.data.list[[4]], fill=TRUE)

  data <- data.table(data)

  return(data)
}
