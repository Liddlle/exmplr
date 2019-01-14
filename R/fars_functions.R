#' Silent read of csv file into a tibble
#'
#' This function allows for reading a comma separated values and transforms them
#' into tibble objects. After checking that the file exists, the function suppress
#' messages produced by read_csv().
#'
#' @param filename Either a path to a file or a file name (a single string).
#'
#' @return This function returns a tibble object, exported from the user's csv file.
#'
#' @details The function will not work correctly if provided with a wrong filepath or if a
#' file is not a csv object.
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#' my_df <- fars_read("accident_2013.csv.bz2")
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename) &
     !file.exists(system.file("extdata", filename, package = "exmplr")))
    stop("file '", filename, "' does not exist")
  else if (file.exists(filename)){
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })}
  else {
    data <- suppressMessages({
      readr::read_csv(system.file("extdata", filename, package = "exmplr"), progress = FALSE)
    })}
  dplyr::tbl_df(data)
}


#' Create a unified filename for a compressed bz2 file
#'
#' This function allows creating a filename using a year as an input. This function is
#' aimed to make reading files faster for the analysis of the data from the US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System.
#'
#' @param year an R object. Must be an integer or a character vector of length 1, containing any year
#'
#' @return This function returns a character object
#'
#' @details The function will provide NAs in the filename if provided with an object non-coercible to
#' an integer. The function will give errors if provided with an object with  a length longer than 1.
#'
#' @examples
#' make_filename(2013)
#' my_df <- read_csv(make_filename(2013))
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Reading month data from the FARS file for a given year
#'
#' This function allows for a simplier reading of the data from the US Fatality Analysis
#' Reporting System. It will return two columns - column with a month and column containing a
#' requested year.
#'
#' @param years an R object. Must be an integer or a character vector of length 1, containing any year
#'
#' @return This function returns a tibble object, containing a column with month exported from the
#' data and a year for which the month data was accessed. If provided with miltiple years -- it
#' will return a list with data frames inside.
#'
#' @details The function will give a warning if provided with an incorrect year. Note that
#' the function doesn't stop running if it found an error -- only produces a warning.
#'
#' @examples
#' my_df <- fars_read_years(2013)
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr "%>%"
#'
#' @export
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


#' Calculate the number of observations per month for the FARS data file for a given year
#'
#' For a given year this function allows for calculating the number of observations per
#' month for the US Fatality Analysis Reporting System data.
#'
#' @param years an R object. Must be an integer or a character vector of length 1 or more,
#' containing any year
#'
#' @return This function returns a tibble object containing a column with month exported from the
#' data and a column with the number of observations per month. If provided with multiple years,
#' the data frame will also contain multiple columns with numbers of observations -- each for a year
#'
#' @details The function will give a warning if provided with an incorrect year. Note that
#' the function doesn't stop running if it found an error -- only produces an erorr.
#'
#' @examples
#' fars_summarize_years(2013)
#' fars_summarize_years(c(2013,2014))
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr "%>%"
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Draw a map with FARS data for a given US state and year
#'
#' For a given year and US state this function plots a map with the distribution of accidents
#' regarding fatal injuries suffered in motor vehicle traffic crashes across the state using the
#' US Fatality Analysis Reporting System data.
#'
#' @param state.num integer: the number of the state from the FARS data
#' @param year an R object. Must be an integer or a character vector of length 1 or more, containing any year
#'
#' @return NULL
#'
#' @details The function will stop running of invalid state number was provided or
#' no event with given conditions were found
#'
#' @examples
#' fars_map_state(1, 2013)
#'
#' @importFrom dplyr filter
#' @importFrom graphics points
#' @importFrom maps map
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
