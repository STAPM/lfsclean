#' Read, Clean, Combine Labour Force Survey data
#'
#' A wrapper function for applying all of the reading and cleaning functions, selecting the
#' desired variables/observations for the analysis, and specifying complete case
#' restrictions.
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
lfsclean <- function(root,
                     file,
                     year = 1993:2022,
                     ages = 16:89,
                     keep_vars = NULL,
                     complete_vars = NULL,
                     deflator = "cpih"){

  cat(crayon::green("Cleaning the Labour Force Survey Data\n"))

  start_time <- Sys.time()

  ###############################################################################
  #### For each year, wrap the reading function in the global cleaning function

  data_list <- list()

  ### 1993

  if (1993 %in% year){

    lfs <- lfs_clean_global(lfs_read_1993(root = root, file = file),
                                year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 1994

  if (1994 %in% year){

    lfs <- lfs_clean_global(lfs_read_1994(root = root, file = file),
                                year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 1995

  if (1995 %in% year){

    lfs <- lfs_clean_global(lfs_read_1995(root = root, file = file),
                                year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 1996

  if (1996 %in% year){

    lfs <- lfs_clean_global(lfs_read_1996(root = root, file = file),
                                year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 1997

  if (1997 %in% year){

    lfs <- lfs_clean_global(lfs_read_1997(root = root, file = file),
                                year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 1998

  if (1998 %in% year){

    lfs <- lfs_clean_global(lfs_read_1998(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 1999

  if (1999 %in% year){

    lfs <- lfs_clean_global(lfs_read_1999(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2000

  if (2000 %in% year){

    lfs <- lfs_clean_global(lfs_read_2000(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2001

  if (2001 %in% year){

    lfs <- lfs_clean_global(lfs_read_2001(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2002

  if (2002 %in% year){

    lfs <- lfs_clean_global(lfs_read_2002(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2003

  if (2003 %in% year){

    lfs <- lfs_clean_global(lfs_read_2003(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2004

  if (2004 %in% year){

    lfs <- lfs_clean_global(lfs_read_2004(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2005

  if (2005 %in% year){

    lfs <- lfs_clean_global(lfs_read_2005(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2006

  if (2006 %in% year){

    lfs <- lfs_clean_global(lfs_read_2006(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2007

  if (2007 %in% year){

    lfs <- lfs_clean_global(lfs_read_2007(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2008

  if (2008 %in% year){

    lfs <- lfs_clean_global(lfs_read_2008(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2009

  if (2009 %in% year){

    lfs <- lfs_clean_global(lfs_read_2009(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2010

  if (2010 %in% year){

    lfs <- lfs_clean_global(lfs_read_2010(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2011

  if (2011 %in% year){

    lfs <- lfs_clean_global(lfs_read_2011(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2012

  if (2012 %in% year){

    lfs <- lfs_clean_global(lfs_read_2012(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2013

  if (2013 %in% year){

    lfs <- lfs_clean_global(lfs_read_2013(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2014

  if (2014 %in% year){

    lfs <- lfs_clean_global(lfs_read_2014(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2015

  if (2015 %in% year){

    lfs <- lfs_clean_global(lfs_read_2015(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2016

  if (2016 %in% year){

    lfs <- lfs_clean_global(lfs_read_2016(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2017

  if (2017 %in% year){

    lfs <- lfs_clean_global(lfs_read_2017(root = root, file = file),
                              year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2018

  if (2018 %in% year){

    lfs <- lfs_clean_global(lfs_read_2018(root = root, file = file),
                            year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2019

  if (2019 %in% year){

    lfs <- lfs_clean_global(lfs_read_2019(root = root, file = file),
                            year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2020

  if (2020 %in% year){

    lfs <- lfs_clean_global(lfs_read_2020(root = root, file = file),
                            year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2021

  if (2021 %in% year){

    lfs <- lfs_clean_global(lfs_read_2021(root = root, file = file),
                            year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2022

  if (2022 %in% year){

    lfs <- lfs_clean_global(lfs_read_2022(root = root, file = file),
                            year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  ### 2023

  if (2023 %in% year){

    lfs <- lfs_clean_global(lfs_read_2023(root = root, file = file),
                            year = year, ages = ages, keep_vars = keep_vars, complete_vars = complete_vars, deflator = deflator)

    data_list <- append(data_list, list(lfs)) ; rm(lfs)
  }

  #############################################################
  ### Combine all waves in the list into a single dataset

  #data <- lfsclean::combine_years(data_list)
  data <- data.table::rbindlist(data_list, use.names = T, fill = T)

  #######################
  ## Record time taken

  end_time <- Sys.time()

  tdiff <- difftime(end_time, start_time, units = "mins")

  time <- paste0("Total Data reading and cleaning time: ", round(tdiff,2), " minutes\n\n")

  cat(crayon::green(time))

  return(data)
}
