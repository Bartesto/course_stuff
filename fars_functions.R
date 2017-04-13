#' Read FARS data into a data frame
#' 
#' \code{fars_read} is a helper function designed to read data downloaded from 
#' the Fatality Analysis Reporting System 
#' \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS}.
#' 
#' Conditions that may result in an error include:
#' \itemize{
#'   \item \code{fars_read} uses functions from the packages \code{dplyr} and 
#'     \code{readr} and they therefore must be installed prior to use. 
#'   \item \code{fars_read} also expects the character string of the FARS data 
#'     file name to be the full file name (i.e. include the file extension).
#'     } 
#'  
#' @param filename A character string of the name of a download FARS data file. 
#'    String can include path to the file if the file to be read is not in the 
#'    working directory.
#'    
#' @return This function unzips the FARS data and returns a data frame of class
#'    tbl_df.
#' 
#' @examples 
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")}
#' 
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Make generic FARS data file name
#' 
#' \code{make_filename} is a helper function that constructs the correct FARS 
#' data file name given a year. This function can be used to generate the input 
#' required for the \code{\link{fars_read}} function.
#' 
#' Conditions that may result in an error include:
#' \itemize{
#'   \item \code{make_filename} will not recognise inputs that cannot be coerced
#'   to an integer. For example 2013 and "2013" are acceptable inputs whereas
#'   "two thousand and thirteen" will cause an error.
#'   
#' @param year A character string or numeric representing the year of the FARS 
#' data file required for further processing. 
#' 
#' @return This function returns a correctly formatted FARS data file name as a
#' character string given the year of interest.
#' 
#' @examples 
#' make_filename(2013)
#' make_filename("2013")
#' 
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>% 
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}