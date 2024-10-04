#' Clean qualifications data
#'
#' Cleans the raw data for detailed qualifications held.
#'
#' @param data data table. Raw LFS data produced using the reading functions.
#'
#' @return Returns a new set of variables
#' @export
lfs_clean_qualifications <- function(
    data
) {

  year <- unique(data[ , year][1])

  #####################################################################
  #### GENERATE DUMMIES FOR THE 30ish INDIVIDUAL QUALIFICATIONS #######

  ###############################
  ### 2005 -

  if(year == 2005){

    for (i in 1:30) {
      data[, qf := 0]
      data[quals401 == i, qf := 1]
      data[quals402 == i, qf := 1]
      data[quals403 == i, qf := 1]
      data[quals404 == i, qf := 1]
      data[quals405 == i, qf := 1]
      data[quals406 == i, qf := 1]
      data[quals407 == i, qf := 1]
      data[quals408 == i, qf := 1]
      data[quals409 == i, qf := 1]
      data[quals410 == i, qf := 1]
      data[quals411 == i, qf := 1]
      data[is.na(quals401) & (is.na(qualch51) | qualch51 == 7), qf := NA]
      setnames(data, "qf", paste0("qf",`i`))
    }

    data[, qual0 := qf1] # Postgraduate degree
    data[quarter == 1     & hiqual4 != 1 & qual0 == 1 , qual0 := 0]
    data[quarter %in% 2:4 & hiqual5 != 1 & qual0 == 1 , qual0 := 0]
  }

  ###########################################################################
  ## Get the id's for observations with all qualification variables observed

  ids_samp <- data[!is.na(qf1)]$obs_id

  #####################################################################
  ##### Apply general cleaning code that applies to all years



  setnames(data, c("qf1","qf2","qf3","qf4","qf5"), c("qual1","qual2","qual3","qual4","qual5"))

  #### BTECs (qual5)
  data[qf6 == 1, qual5 := 1 ] # combine BTEC with SCOTVEC

  data[, qf5_4 := ifelse(qual5 == 1 & (btec == 1 | sctvec == 1), 1, 0)]
  data[is.na(qual5) | (is.na(btec) & is.na(sctvec)), qf5_4 := NA]

  data[, qf5_3 := ifelse(qual5 == 1 & (btec == 2 | sctvec == 2), 1, 0)]
  data[is.na(qual5) | (is.na(btec) & is.na(sctvec)), qf5_3 := NA]

  data[, qf5_2 := ifelse(qual5 == 1 & (btec == 3 | sctvec == 3), 1, 0)]
  data[is.na(qual5) | (is.na(btec) & is.na(sctvec)), qf5_2 := NA]

  data[, qf5_1 := ifelse(qual5 == 1 & (btec == 4 | sctvec == 4), 1, 0)]
  data[is.na(qual5) | (is.na(btec) & is.na(sctvec)), qf5_1 := NA]

  setnames(data, c("qf7","qf8","qf9","qf10"), c("qual6","qual7","qual8","qual9"))

  #### A-Level and equivalents (qual9)
  data[qf11 == 1, qual9 := 1]
  data[qf12 == 1, qual9 := 1]
  data[qf13 == 1, qual9 := 1]
  data[qf17 == 1, qual9 := 1]
  data[qf18 == 1, qual9 := 1]
  data[qf23 == 1 & typhst1 == 4 | typhst2 == 4 | typhst3 == 4 | typhst4 == 4 | typhst5 == 4, qual9 := 1]
  data[qf23 == 1 & typhst1 == 5 | typhst2 == 5 | typhst3 == 5 | typhst4 == 5 | typhst5 == 5, qual9 := 1]

  #### NVQs (qual10), GNVQs (qual11), and AS-Levels (qual12)
  setnames(data, c("qf14","qf15","qf16"), c("qual10","qual11","qual12"))

  ## NVQs
  data[, qf10_5 := ifelse(qual10 == 1 & (nvqlev == 5), 1, 0)]
  data[is.na(qual10) | (is.na(nvqlev)), qf10_5 := NA]

  data[, qf10_4 := ifelse(qual10 == 1 & (nvqlev == 4), 1, 0)]
  data[is.na(qual10) | (is.na(nvqlev)), qf10_4 := NA]

  data[, qf10_3 := ifelse(qual10 == 1 & (nvqlev == 3), 1, 0)]
  data[is.na(qual10) | (is.na(nvqlev)), qf10_3 := NA]

  data[, qf10_2 := ifelse(qual10 == 1 & (nvqlev == 2), 1, 0)]
  data[is.na(qual10) | (is.na(nvqlev)), qf10_2 := NA]

  data[, qf10_1 := ifelse(qual10 == 1 & (nvqlev == 1), 1, 0)]
  data[is.na(qual10) | (is.na(nvqlev)), qf10_1 := NA]

  ## GNVQs
  data[, qf11_3 := ifelse(qual11 == 1 & (gnvq == 1), 1, 0)]
  data[is.na(qual11) | (is.na(gnvq)), qf11_3 := NA]

  data[, qf11_2 := ifelse(qual11 == 1 & (gnvq %in% 2:3), 1, 0)]
  data[is.na(qual11) | (is.na(gnvq)), qf11_2 := NA]

  data[, qf11_1 := ifelse(qual11 == 1 & (gnvq %in% 4:5), 1, 0)]
  data[is.na(qual11) | (is.na(gnvq)), qf11_1 := NA]

  #### GCSE Grade C or above
  setnames(data, "qf19", "qual13")
  data[qf20 == 1 & (gcseful1==3 | gcseful2==3 | gcseful3==3 | gcseful4==3), qual13 := 1]
  data[qf21 == 1 & (gcseful1==1 | gcseful2==1 | gcseful3==1 | gcseful4==1), qual13 := 1]
  data[qf22 == 1 & (gcseful1==2 | gcseful2==2 | gcseful3==2 | gcseful4==2), qual13 := 1]
  data[qf23 == 1 & (gcseful1==4 | gcseful2==4 | gcseful3==4 | gcseful4==4), qual13 := 1]
  data[qf23 == 1 & (gcseful1==5 | gcseful2==5 | gcseful3==5 | gcseful4==5), qual13 := 1]

  # 1-4, or 5+ GCSEs A*-C
  data[, qf13_1_4 := ifelse(qual13 == 1 & numol == 1, 1, 0)]
  data[is.na(qual13) | is.na(numol), qf13_1_4 := NA]

  data[, qf13_5 := ifelse(qual13 == 1 & numol == 2, 1, 0)]
  data[is.na(qual13) | is.na(numol), qf13_5 := NA]

  #### GCSE Grade D or below
  data[, qual14 := 0]
  data[qf20 == 1 & (qgcse41==3 | qgcse42==3 | qgcse43==3 | qgcse44==3), qual14 := 1 ]
  data[qf21 == 1 & (qgcse41==1 | qgcse42==1 | qgcse43==1 | qgcse44==1), qual14 := 1 ]
  data[qf22 == 1 & (qgcse41==2 | qgcse42==2 | qgcse43==2 | qgcse44==2), qual14 := 1 ]
  data[qf23 == 1 & (qgcse41==4 | qgcse42==4 | qgcse43==4 | qgcse44==4), qual14 := 1 ]
  data[qf23 == 1 & (qgcse41==5 | qgcse42==5 | qgcse43==5 | qgcse44==5), qual14 := 1 ]
  data[is.na(qual13), qual14 := 0]

  #### RSA / OCR qualifications
  setnames(data, "qf24", "qual15")

  data[, qf15_4 := ifelse(qual15 == 1 & (rsa == 1), 1, 0)]
  data[is.na(qual15) | (is.na(rsa)), qf15_4 := NA]

  data[, qf15_3 := ifelse(qual15 == 1 & (rsa == 2), 1, 0)]
  data[is.na(qual15) | (is.na(rsa)), qf15_3 := NA]

  data[, qf15_2 := ifelse(qual15 == 1 & (rsa == 3), 1, 0)]
  data[is.na(qual15) | (is.na(rsa)), qf15_2 := NA]

  data[, qf15_1 := ifelse(qual15 == 1 & (rsa == 4 | sctvec == 4), 1, 0)]
  data[is.na(qual15) | (is.na(rsa) ), qf15_1 := NA]

  #### City & Guilds qualifications
  setnames(data, "qf25", "qual16")

  data[, qf16_3 := ifelse(qual16 == 1 & (rsa == 1), 1, 0)]
  data[is.na(qual16) | (is.na(rsa)), qf16_3 := NA]

  data[, qf16_2 := ifelse(qual16 == 1 & (rsa == 2), 1, 0)]
  data[is.na(qual16) | (is.na(rsa)), qf16_2 := NA]

  data[, qf16_1 := ifelse(qual16 == 1 & (rsa == 3 | sctvec == 4), 1, 0)]
  data[is.na(qual16) | (is.na(rsa) ), qf16_1 := NA]

  ### Youth Training Certificate
  setnames(data, "qf26", "qual17")

  ### All other
  setnames(data, "qf27", "qual18")
  data[qf28 == 1, qual18 := 1]
  data[qf29 == 1, qual18 := 1]
  data[qf30 == 1, qual18 := 1]

  ##########################################
  #### Tidy data

  data[, c("qf11","qf12","qf13","qf17","qf18","qf20",
           "qf21","qf22","qf23","qf28","qf29","qf30") := NULL]

  setnames(data, paste0("qual",0:18), paste0("qf",0:18))

  ## create a "no qualifications" variable
  data[!is.na(qf1), qfnone := 1]
  data[qf0 == 1, qfnone := 0]
  data[qf1 == 1, qfnone := 0]  ; data[qf2 == 1, qfnone := 0]  ; data[qf3 == 1, qfnone := 0]
  data[qf4 == 1, qfnone := 0]  ; data[qf5 == 1, qfnone := 0]  ; data[qf6 == 1, qfnone := 0]
  data[qf7 == 1, qfnone := 0]  ; data[qf8 == 1, qfnone := 0]  ; data[qf9 == 1, qfnone := 0]
  data[qf10 == 1, qfnone := 0] ; data[qf11 == 1, qfnone := 0] ; data[qf12 == 1, qfnone := 0]
  data[qf13 == 1, qfnone := 0] ; data[qf14 == 1, qfnone := 0] ; data[qf15 == 1, qfnone := 0]
  data[qf16 == 1, qfnone := 0] ; data[qf15 == 1, qfnone := 0] ; data[qf18 == 1, qfnone := 0]

  ## rename all qualifications variables
  setnames(data,

           c("qf0","qf1","qf2",
             "qf5_4","qf5_3","qf5_2","qf5_1",
             "qf10_5","qf10_4","qf10_3","qf10_2","qf10_1",
             "qf11_3","qf11_2","qf11_1",
             "qf9","qf12",
             "qf13_1_4","qf13_5","qf14",
             "qf15_4","qf15_3","qf15_2","qf15_1",
             "qf16_3","qf16_2","qf16_1",
             "qf18",
             "qfnone"),

           c("higher","firstdegree","otherdegree",
             "btec4","btec3","btec2","btec1",
             "nvq5","nvq4","nvq3","nvq2","nvq1",
             "gnvq3","gnvq2","gnvq1",
             "alevel","aslevel",
             "olevelless","olevelmore","cse",
             "rsa4","rsa3","rsa2","rsa1",
             "cag3","cag2","cag1",
             "otherquals",
             "noquals"))

  ### merge nursing, teaching,diploma in HE into "otherdegree
  data[qf6 == 1 | qf7 == 1 | qf8 == 1, otherdegree := 1]
  data[, c("qf6","qf7","qf8") := NULL]

  ### merge HND/HNC and OND/ONC with BTEC 4 and 3
  data[qf3 == 1, btec4 := 1]
  data[, c("qf3") := NULL]

  data[qf4 == 1, btec3 := 1]
  data[, c("qf4") := NULL]

  ### merge Youth Training Certificate into "other quals"
  data[qf17 == 1, otherquals := 1]
  data[, c("qf17") := NULL]

  ###############################################
  ### VOCATIONAL QUALIFICATION SUBJECTS #########

  ## subject recode I
  ## subject recode II
  ## simplify vocational subjects

  ###############################################
  ### APPRENTICESHIPS ###########################

  if("appr4" %in% colnames(data)){

  data[appr4 %in% c(1,3), apprenticeship := 1]
  data[appr4 %in% c(2,4), apprenticeship := 0]

  data[, modern_app := apprenticeship]
  data[modapp4 %in% 4:5, modern_app := 0]

  data[, amodern_app := modern_app]
  data[modapp4 == 1 | (modapp4 == 3 & appr4 == 3), amodern_app := 0]

  data[, fmodern_app := modern_app]
  data[modapp4 %in% 2:3, fmodern_app := 0]

  }

  ###############################
  ### get complete cases #######

  data <- data[obs_id %in% ids_samp,]

  data[is.na(btec4), btec4 := 0]; data[is.na(btec3), btec3 := 0]; data[is.na(btec2), btec2 := 0]; data[is.na(btec1), btec1 := 0]

  data[is.na(nvq5), nvq5 := 0]; data[is.na(nvq4), nvq4 := 0]; data[is.na(nvq3), nvq3 := 0]; data[is.na(nvq2), nvq2 := 0]; data[is.na(nvq1), nvq1 := 0]

  data[is.na(gnvq3), gnvq3 := 0]; data[is.na(gnvq2), gnvq2 := 0]; data[is.na(gnvq1), gnvq1 := 0]

  data[is.na(olevelmore), olevelmore := 0]; data[is.na(olevelless), olevelless := 0]

  data[is.na(rsa4), rsa4 := 0]; data[is.na(rsa3), rsa3 := 0]; data[is.na(rsa2), rsa2 := 0]; data[is.na(rsa1), rsa1 := 0]

  data[is.na(cag3), cag3 := 0]; data[is.na(cag2), cag2 := 0]; data[is.na(cag1), cag1 := 0]


  ###############################
  ### RETAIN CLEANED VARIABLES

  final_data <- data[, c("obs_id", "oowben")]

  var_names <- c("oowben")

  setnames(final_data, var_names, paste0("q_", var_names))

  return(final_data)

}

