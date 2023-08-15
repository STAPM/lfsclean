#' Combine years of data
#'
#' Combines years of data when provided as a list of data tables.
#'
#' @param data_list List of data tables to combine.
#'
#' @return Returns data table of combined data.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data_all <- combine_years(list(data1, data2, data3))
#'
#' }
#'
combine_years <- function(
  data_list
) {

  #start_time <- Sys.time()

  data <- data.table::rbindlist(data_list, use.names = T, fill = T)


  # create a year-quarter time variable
  #data$time <- zoo::as.yearqtr(paste0(data$year, "-", data$quarter))


  #end_time <- Sys.time()

  #tdiff <- difftime(end_time, start_time, units = "mins")

  #time <- paste0("Data reading and cleaning time: ", round(tdiff,2), " minutes")

  #cat(crayon::green(time))

  return(data)
}
