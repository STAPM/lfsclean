#' Clean LFS longitudinal data
#'
#' A function to apply all cleaning processes to the LFS five-quarter longitudinal data to.
#'
#' @param data Data table - the Labour Force Survey (longitudinal) dataset.
#' @param ages Integer vector - the ages - in wave 1 - in single years to retain (defaults to 16 to 89 years).
#' @param years Integer vector - the years in single years to retain.
#' @param keep_vars Character vector - the names of the variables to keep (defaults to year, age and gender).
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to year, age and gender).
#' @param deflator character. Inflation index to use in producing real values. One of "cpih" or "rpi".
#' Default is cpih.
#' @param long logical. If TRUE, present data in long form (one observation per person per quarter), if FALSE (default) present data in wide form (one observation per person)
#' @return Returns a new set of variables
#' @export
lfs_clean_global_5q <- function(data,
                                ages = 16:89,
                                years = 2022,
                                keep_vars = NULL,
                                complete_vars = NULL,
                                deflator = "cpih",
                                long = TRUE
) {

  ############################################################
  ### Do age/years filtering, complete-case selection here ###

  ## fix bug that occurs if age is not in keep_vars

  if (!("age1" %in% names(data)) ) {
    ages = NULL
  }

  ##################################################
  ## Create clean variables

  ## create an employment pattern variable


  data <- data %>%
    mutate(empl_sequence = paste0(ilodefr1, ilodefr2, ilodefr3, ilodefr4, ilodefr5)) %>%
    mutate(empl_sequence = str_replace_all(empl_sequence, c("1" = "E",
                                                            "2" = "U",
                                                            "3" = "I",
                                                            "4" = "I")))

  ## clean variables

  data <- data %>%
    mutate(sex = factor(sex, levels = 1:2, labels = c("male","female"))) %>%

    mutate(eth2cat1 = case_match(etukeul1, c(1) ~ "white", c(2:9) ~ "non_white")) %>%
    mutate(eth2cat2 = case_match(etukeul2, c(1) ~ "white", c(2:9) ~ "non_white")) %>%
    mutate(eth2cat3 = case_match(etukeul3, c(1) ~ "white", c(2:9) ~ "non_white")) %>%
    mutate(eth2cat4 = case_match(etukeul4, c(1) ~ "white", c(2:9) ~ "non_white")) %>%
    mutate(eth2cat5 = case_match(etukeul5, c(1) ~ "white", c(2:9) ~ "non_white")) %>%

    mutate(eth4cat1 = case_match(etukeul1, c(1) ~ "white", c(2) ~ "mixed", c(3:7,9) ~ "asian_other", c(8) ~ "black")) %>%
    mutate(eth4cat2 = case_match(etukeul2, c(1) ~ "white", c(2) ~ "mixed", c(3:7,9) ~ "asian_other", c(8) ~ "black")) %>%
    mutate(eth4cat3 = case_match(etukeul3, c(1) ~ "white", c(2) ~ "mixed", c(3:7,9) ~ "asian_other", c(8) ~ "black")) %>%
    mutate(eth4cat4 = case_match(etukeul4, c(1) ~ "white", c(2) ~ "mixed", c(3:7,9) ~ "asian_other", c(8) ~ "black")) %>%
    mutate(eth4cat5 = case_match(etukeul5, c(1) ~ "white", c(2) ~ "mixed", c(3:7,9) ~ "asian_other", c(8) ~ "black")) %>%

    mutate(eth4cat1 = factor(eth4cat1, levels = c("white","black","mixed","asian_other"))) %>%
    mutate(eth4cat2 = factor(eth4cat2, levels = c("white","black","mixed","asian_other"))) %>%
    mutate(eth4cat3 = factor(eth4cat3, levels = c("white","black","mixed","asian_other"))) %>%
    mutate(eth4cat4 = factor(eth4cat4, levels = c("white","black","mixed","asian_other"))) %>%
    mutate(eth4cat5 = factor(eth4cat5, levels = c("white","black","mixed","asian_other"))) %>%

    mutate(empstat2cat1 = case_match(incac051, c(1:4) ~ "employed", c(5:33) ~ "not_employed")) %>%
    mutate(empstat2cat2 = case_match(incac052, c(1:4) ~ "employed", c(5:33) ~ "not_employed")) %>%
    mutate(empstat2cat3 = case_match(incac053, c(1:4) ~ "employed", c(5:33) ~ "not_employed")) %>%
    mutate(empstat2cat4 = case_match(incac054, c(1:4) ~ "employed", c(5:33) ~ "not_employed")) %>%
    mutate(empstat2cat5 = case_match(incac055, c(1:4) ~ "employed", c(5:33) ~ "not_employed")) %>%

    mutate(empstat3cat1 = case_match(incac051, c(1:4) ~ "employed", c(5) ~ "unemployed", c(6:33) ~ "inactive")) %>%
    mutate(empstat3cat2 = case_match(incac052, c(1:4) ~ "employed", c(5) ~ "unemployed", c(6:33) ~ "inactive")) %>%
    mutate(empstat3cat3 = case_match(incac053, c(1:4) ~ "employed", c(5) ~ "unemployed", c(6:33) ~ "inactive")) %>%
    mutate(empstat3cat4 = case_match(incac054, c(1:4) ~ "employed", c(5) ~ "unemployed", c(6:33) ~ "inactive")) %>%
    mutate(empstat3cat5 = case_match(incac055, c(1:4) ~ "employed", c(5) ~ "unemployed", c(6:33) ~ "inactive")) %>%

    mutate(empstat8cat1 = case_match(incac051, c(1,3,4) ~ "employed", c(2) ~ "self_employed", c(6,13,24) ~ "education", c(5) ~ "unemployed", c(8:9,15:16,26:27) ~ "sick", c(7,14,25) ~ "caring", c(20,31) ~ "retired", c(10:11,17:19,21:23,28:30,32:34) ~ "other")) %>%
    mutate(empstat8cat2 = case_match(incac052, c(1,3,4) ~ "employed", c(2) ~ "self_employed", c(6,13,24) ~ "education", c(5) ~ "unemployed", c(8:9,15:16,26:27) ~ "sick", c(7,14,25) ~ "caring", c(20,31) ~ "retired", c(10:11,17:19,21:23,28:30,32:34) ~ "other")) %>%
    mutate(empstat8cat3 = case_match(incac053, c(1,3,4) ~ "employed", c(2) ~ "self_employed", c(6,13,24) ~ "education", c(5) ~ "unemployed", c(8:9,15:16,26:27) ~ "sick", c(7,14,25) ~ "caring", c(20,31) ~ "retired", c(10:11,17:19,21:23,28:30,32:34) ~ "other")) %>%
    mutate(empstat8cat4 = case_match(incac054, c(1,3,4) ~ "employed", c(2) ~ "self_employed", c(6,13,24) ~ "education", c(5) ~ "unemployed", c(8:9,15:16,26:27) ~ "sick", c(7,14,25) ~ "caring", c(20,31) ~ "retired", c(10:11,17:19,21:23,28:30,32:34) ~ "other")) %>%
    mutate(empstat8cat5 = case_match(incac055, c(1,3,4) ~ "employed", c(2) ~ "self_employed", c(6,13,24) ~ "education", c(5) ~ "unemployed", c(8:9,15:16,26:27) ~ "sick", c(7,14,25) ~ "caring", c(20,31) ~ "retired", c(10:11,17:19,21:23,28:30,32:34) ~ "other")) %>%

    mutate(empstat2cat1 = as.factor(empstat2cat1), empstat2cat2 = as.factor(empstat2cat2), empstat2cat3 = as.factor(empstat2cat3), empstat2cat4 = as.factor(empstat2cat4), empstat2cat5 = as.factor(empstat2cat5)) %>%
    mutate(empstat3cat1 = as.factor(empstat3cat1), empstat3cat2 = as.factor(empstat3cat2), empstat3cat3 = as.factor(empstat3cat3), empstat3cat4 = as.factor(empstat3cat4), empstat3cat5 = as.factor(empstat3cat5)) %>%
    mutate(empstat8cat1 = as.factor(empstat8cat1), empstat8cat2 = as.factor(empstat8cat2), empstat8cat3 = as.factor(empstat8cat3), empstat8cat4 = as.factor(empstat8cat4), empstat8cat5 = as.factor(empstat8cat5)) %>%

    mutate(region1 = factor(govtof21, levels = c(1:2,4:13), labels = c("north_east","north_west","yorks_and_humber","east_mids","west_mids",
                                                                       "east_of_england","london","south_east","south_west","wales","scotland","northern_ireland"))) %>%
    mutate(region2 = factor(govtof22, levels = c(1:2,4:13), labels = c("north_east","north_west","yorks_and_humber","east_mids","west_mids",
                                                                       "east_of_england","london","south_east","south_west","wales","scotland","northern_ireland"))) %>%
    mutate(region3 = factor(govtof23, levels = c(1:2,4:13), labels = c("north_east","north_west","yorks_and_humber","east_mids","west_mids",
                                                                       "east_of_england","london","south_east","south_west","wales","scotland","northern_ireland"))) %>%
    mutate(region4 = factor(govtof24, levels = c(1:2,4:13), labels = c("north_east","north_west","yorks_and_humber","east_mids","west_mids",
                                                                       "east_of_england","london","south_east","south_west","wales","scotland","northern_ireland"))) %>%
    mutate(region5 = factor(govtof25, levels = c(1:2,4:13), labels = c("north_east","north_west","yorks_and_humber","east_mids","west_mids",
                                                                       "east_of_england","london","south_east","south_west","wales","scotland","northern_ireland"))) %>%
    rename(disab1 = disea1, disab2 = disea2, disab3 = disea3, disab4 = disea4, disab5 = disea5) %>%
    mutate(disab1 = factor(disab1, levels = 1:2, labels = c("disabled","not_disabled"))) %>%
    mutate(disab2 = factor(disab2, levels = 1:2, labels = c("disabled","not_disabled"))) %>%
    mutate(disab3 = factor(disab3, levels = 1:2, labels = c("disabled","not_disabled"))) %>%
    mutate(disab4 = factor(disab4, levels = 1:2, labels = c("disabled","not_disabled"))) %>%
    mutate(disab5 = factor(disab5, levels = 1:2, labels = c("disabled","not_disabled"))) %>%

    rename(numsickdays1 = illoff1, numsickdays2 = illoff2, numsickdays3 = illoff3, numsickdays4 = illoff4, numsickdays5 = illoff5) %>%
    rename(uhours1 = ttushr1, uhours2 = ttushr2, uhours3 = ttushr3, uhours4 = ttushr4, uhours5 = ttushr5) %>%

    mutate(benclaim1 = factor(ooben1, levels = 1:7, labels = c("jobseeker","sick","lone_parent","carer","other_oow_benefits","other_benefits","no_benefits"))) %>%
    mutate(benclaim2 = factor(ooben2, levels = 1:7, labels = c("jobseeker","sick","lone_parent","carer","other_oow_benefits","other_benefits","no_benefits"))) %>%
    mutate(benclaim3 = factor(ooben3, levels = 1:7, labels = c("jobseeker","sick","lone_parent","carer","other_oow_benefits","other_benefits","no_benefits"))) %>%
    mutate(benclaim4 = factor(ooben4, levels = 1:7, labels = c("jobseeker","sick","lone_parent","carer","other_oow_benefits","other_benefits","no_benefits"))) %>%
    mutate(benclaim5 = factor(ooben5, levels = 1:7, labels = c("jobseeker","sick","lone_parent","carer","other_oow_benefits","other_benefits","no_benefits"))) %>%
    rename(lgwt = lgwt22)



  #################################
  ### remove variables not needed and retain

  data <- data %>%
    dplyr::select(c(persid, lgwt, sex, quarter, year, empl_sequence,
             age1, age2, age3, age4, age5,
             disab1, disab2, disab3, disab4, disab5,
             region1, region2, region3, region4, region5,
             eth4cat1, eth4cat2, eth4cat3, eth4cat4, eth4cat5,
             empstat2cat1, empstat2cat2, empstat2cat3, empstat2cat4, empstat2cat5,
             empstat3cat1, empstat3cat2, empstat3cat3, empstat3cat4, empstat3cat5,
             empstat8cat1, empstat8cat2, empstat8cat3, empstat8cat4, empstat8cat5,
             numsickdays1, numsickdays2, numsickdays3, numsickdays4, numsickdays5,
             benclaim1, benclaim2, benclaim3, benclaim4, benclaim5,
             uhours1, uhours2, uhours3, uhours4, uhours5,
             grsswk1,                            grsswk5))

  #############################################################################
  ### reshape long, match in inflation data and create real-earnings variables


  ######################
  ### Data filtering ###


  final_data <- lfsclean::select_data(
    data = data,
    ages = ages,
    years = years,
    keep_vars = keep_vars,
    complete_vars = complete_vars,
    longitudinal = TRUE
  )

  return(final_data)
}
