#' Read, Clean, Combine Labour Force Survey data
#'
#' A wrapper function for applying all reading and cleaning functions for the five-quarter
#' longitudinal Labour Force Survey datasets
#'
#' @param root Character - the root directory.
#' @param file Character - the file path and name.
#' @param year Integer vector - the years of the LFS retain (defaults to all full years - 1993 to 2021).
#' @param ages Integer vector - the ages in single years to retain (defaults to 16 to 89 years).
#' @param keep_vars Character vector - the names of the variables to keep (defaults to NULL - retaining all variables).
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to year, age and gender).
#' @param deflator character. Inflation index to use in producing real values. One of "cpih" or "rpi".
#' Default is cpih.
#' @return Returns a new set of variables.
#' @export
lfsclean_5q <- function(root = "X:/",
                        file = "HAR_PR/PR/LFS/Data/longitudinal/tab/",
                        year = 2012:2022,
                        ages = 16:89,
                        keep_vars = NULL,
                        complete_vars = NULL,
                        deflator = "cpih"){

  cat(crayon::green("Cleaning the Labour Force Survey (5-quarter longitudinal) Data\n"))

  start_time <- Sys.time()

  ###############################################################################
  #### For each year, wrap the reading function in the global cleaning function

  data_list <- list()

  ### 2012

  if (2012 %in% year){

    lfs <- lfs_clean_global_5q(lfs_read_5q_2012(root = root, file = file),
                               year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2013

  if (2013 %in% year){

    lfs <- lfs_clean_global_5q(lfs_read_5q_2013(root = root, file = file),
                               year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2014

  if (2014 %in% year){

    lfs <- lfs_clean_global_5q(lfs_read_5q_2014(root = root, file = file),
                               year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2015

  if (2015 %in% year){

    lfs <- lfs_clean_global_5q(lfs_read_5q_2015(root = root, file = file),
                               year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2016

  if (2016 %in% year){

    lfs <- lfs_clean_global_5q(lfs_read_5q_2016(root = root, file = file),
                               year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2017

  if (2017 %in% year){

    lfs <- lfs_clean_global_5q(lfs_read_5q_2017(root = root, file = file),
                               year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2018

  if (2018 %in% year){

    lfs <- lfs_clean_global_5q(lfs_read_5q_2018(root = root, file = file),
                               year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2019

  if (2019 %in% year){

    lfs <- lfs_clean_global_5q(lfs_read_5q_2019(root = root, file = file),
                               year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2020

  if (2020 %in% year){

    lfs <- lfs_clean_global_5q(lfs_read_5q_2020(root = root, file = file),
                               year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2021

  if (2021 %in% year){

    lfs <- lfs_clean_global_5q(lfs_read_5q_2021(root = root, file = file),
                            year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2022

  if (2022 %in% year){

    lfs <- lfs_clean_global_5q(lfs_read_5q_2022(root = root, file = file),
                            year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)


    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  #############################################################
  ### Combine all waves in the list into a single dataset

  data <- data.table::rbindlist(data_list, use.names = T, fill = T)

  #######################
  ## Record time taken

  end_time <- Sys.time()

  tdiff <- difftime(end_time, start_time, units = "mins")

  time <- paste0("Total Data reading and cleaning time: ", round(tdiff,2), " minutes\n\n")

  cat(crayon::green(time))

  return(data)
}
