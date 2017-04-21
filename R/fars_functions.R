# Fars Functions ===============================================================

#' Read a CSV data file as a data frame.
#'
#' The filename must refer to an exsting filename containing comma-separated
#' data.
#' This function depends on the readr::read_csv function.
#'
#' @param filename (character) the name of the file to be read.
#'
#' @return (data frame, tbl_df, tbl) data read from the file
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


# ------------------------------------------------------------------------------

#' Create a data file name for a given year.
#'
#' @param year (integer or character) the year identifying the file whose name
#'   to construct.
#'
#' The "year" parameter must be supplied in a form which can be converted to
#' an integer year value (by `as.integer`).
#'
#' @return (character) file name incorporating year.
#'
#' @examples
#' \dontrun{
#'   make_filename(2017)
#'   make_filename("2017")
#' }
#' @export

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

# ------------------------------------------------------------------------------

#' Read and combine data for multiple years.
#'
#' @param years (integer or character) vector or list of the years for which to
#' read data.
#'
#' The `years` parameter must be supplied in a form which can be converted to
#' integer year value (by `as.integer`).
#'
#' This function depends on dplyr::select and dplyr::mutate.
#'
#' @return (list) of data frames each entry having data for a specific year.
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

# ------------------------------------------------------------------------------

#' Summarize data read for multiple years by number of accidents per month
#'
#' @param years (integer or character) vector or list of the years for which to
#' read data.
#'
#' The `years` parameter must be supplied in a form which can be converted to
#' integer year value (by `as.integer`).
#'
#' This function depends on several functions from the dplyr package:
#' bind_rows, group_by, and summarize. It also depends on tidyr::spread.
#'
#' @return (data frame, tbl_df, tbl) a data frame containing the accident count of every year selected (one column per year) computed by month (one row per month)
#'
#' @export

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

# ------------------------------------------------------------------------------

#' Create a map for the location of accidents within a state for a given year.
#'
#' @param year (integer or character) the year identifying the file whose name
#'   to construct.
#' @param state.num (integer) the number of the ID of the state to analyse.
#'
#' This function depends on the dplyr::filter, maps::map functions.
#' bind_rows, group_by, and summarize. It also depends on tidyr::spread.
#'
#' @return NULL. Function has side effect of plotting a map.
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
