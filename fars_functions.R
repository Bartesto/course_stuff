#' Read FARS data into a data frame.
#' 
#' \code{fars_read} is a helper function designed to read data downloaded from 
#' the Fatality Analysis Reporting System (FARS).
#' 
#' @source FARS data can be from 
#' \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{here}.
#' 
#' @notes Conditions that may result in an error include:
#' \itemize{
#'   \item \code{fars_read} uses functions from the packages \code{\link{dplyr}} 
#'     and \code{\link{readr}} and they therefore must be installed prior to use. 
#'   \item \code{fars_read} also expects the character string of the FARS data 
#'     file name to be the full file name (i.e. include the file extension).
#'     } 
#'  
#' @param filename A character string of the name of a download FARS data file. 
#'    String can include path to the file if the file to be read is not in the 
#'    working directory.
#'    
#' @return This function unzips the FARS data and returns a data frame of class
#'    'tbl_df', 'tbl' and 'data.frame'.
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

#' Make generic FARS data file name.
#' 
#' \code{make_filename} is a helper function that constructs the correct 
#' Fatality Analysis Reporting System (FARS) data file name given a year. This 
#' function can be used to generate the input required for the 
#' \code{\link{fars_read}} function.
#' 
#' @source FARS data can be from 
#' \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{here}. 
#' 
#' @notes Conditions that may result in an error include:
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


#' Read multiple years of FARS data into a list of data frames.
#' 
#' Given a vector of years corresponding to Fatality Analysis Reporting System 
#' (FARS) data file names,  \code{fars_read_years} will read each of the FARS 
#' data files and create a list of data frames of the months and years contained 
#' in each FARS data file.
#' 
#' @source FARS data can be from 
#' \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{here}. 
#' 
#' @notes Conditions that may result in an error include:
#' \itemize{
#'   \item This function uses \code{\link{make_filename}} and as such it must be 
#'     loaded into the NAMESPACE.
#'   \item This function and others that it rely on, \code{\link{make_filename}},
#'     require the packages \code{\link{dplyr}} and \code{\link{readr}} to be 
#'     installed.
#'   \item This function also requires that the package \code{\link{dpyr}} is 
#'     loaded into the NAMESPACE.
#'   \item This function must be run from the same location as the downloaded
#'     FARS data files.
#'   \item Providing a vector of years where there is no corresponding FARS data
#'     file will result in an error for that year/years. The function will still 
#'     return a list however, and it will contain results for years that do have 
#'     a corresponding FARS data file.
#'   }
#'   
#' @param years A character or numeric vector representing the years of the FARS 
#' data files required for further processing.
#' 
#' @return The function will return a list containing as many data frames as 
#' years provided as input. The list is of class 'list' whilst the individual 
#' data frames are of class 'tbl_df', 'tbl' and 'data.frame'. Each data frame 
#' contains the columns:
#' \describe{
#'  \item{MONTH}{The months corresponding to each observation in the FARS data 
#'  file}
#'  \item{year}{The year corresponding to each observation in the FARS data file}}
#' 
#' @importfrom dplyr readr
#'  
#' @examples 
#'  \dontrun{
#'  fars_read_years(c(2013, 2014, 2015))
#'  fars_read_years(c("2013", "2014", "2015"))
#'  }
#'  
#'  @export
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


#' Summarise FARS data.
#' 
#' Given a vector of years corresponding to Fatality Analysis Reporting System 
#' (FARS) data file names, \code{fars_summarize_years} will read each of the FARS 
#' data files and create a single data frame summarising fatalities by month for 
#' the input years.
#' 
#' @source FARS data can be from 
#' \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{here}.
#' 
#' @notes Conditions that may result in an error include:
#' \itemize{
#'   \item This function uses \code{\link{make_filename}} and 
#'     \code{link{fars_read_years}}. As such they must be loaded into the 
#'     NAMESPACE.
#'   \item This function and others that it rely on, \code{\link{make_filename}}
#'     and \code{link{fars_read_years}}, require the packages \code{\link{dplyr}}, 
#'     \code{\link{readr}} and \code{\link{tidyr}} to be installed.
#'   \item This function also requires that the package \code{\link{dpyr}} is 
#'     loaded into the NAMESPACE.
#'   \item This function must be run from the same location as the downloaded
#'     FARS data files.
#'   \item Providing a vector of years where there is no corresponding FARS data
#'     file will result in an error for that year/years. The function will still 
#'     return a data frame however, and it will contain results for years that 
#'     do have a corresponding FARS data file.
#'   }     
#' 
#' @inheritParams fars_read_years
#' 
#' @return This function will return a data frame of class 'tbl_df', 'tbl' and 
#' 'data.frame'. The data frame will contain columns:
#' \describe{
#'  \item{MONTH}{Numerical month abbreviations 1-12}
#'  \item{YYYY}{'YYYY' represents a named year. There will be a separate column 
#'  for each year as provided in the vector of years as input. Each column will 
#'  contain the number of fatalities per month for that year.}
#' 
#' @importfrom dplyr readr
#' 
#' @examples 
#' \dontrun{
#' fars_summarize_years(c(2013, 2014, 2015))
#' fars_summarize_years(c("2013", "2014", "2015"))
#' }
#' 
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Plot a map of accidents from FARS data
#' 
#' Given a US State number and a year, this function will produce a simple black 
#' and white map indicating accident locations as contained in a FARS data file.
#' 
#' @source FARS data can be from 
#' \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{here}.
#' 
#' @notes Conditions that may result in an error include:
#' \itemize{
#'   \item This function uses \code{\link{make_filename}} and 
#'     \code{\link{fars_read}} and these must be loaded into the NAMESPACE.
#'   \item This function also requires the packages \code{\link{dplyr}} and 
#'     \code{\link{maps}} to be installed and loaded into the NAMESPACE.
#'   \item Providing an incorrect US State number.
#'   \item The year input must be coerceable to class integer.
#'   \item The provided year must correspond to a downloaded FARS data file.
#'   \item The function must be run from the same location as the downloaded 
#'     FARS files that require analysis.
#'   }
#' 
#' @param state.num An integer or character representing the number 
#'  corresponding to the State in the US for analysis.
#' @inheritParams make_filename
#' 
#' @return A map plot of accident locations, represented as points, for a given 
#' US State and year with information from the appropriate FARS data file. The 
#' State boundary will also be drawn.
#' 
#' @importFrom dplyr readr maps
#' 
#' @examples 
#' \dontrun{
#' fars_map_state(47, 2013)
#' fars_map_state("47", "2013")
#' }
#' 
#' @export
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