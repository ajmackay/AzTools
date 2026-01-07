#' Unnest All
#'
#' @param df dataframe with nested columns
#'
#' @returns dataframe
#' @export

unnest_all <- function(df) {
  list_columns <- df %>% keep(is.list) %>% discard(~any(map_lgl(., is_empty))) %>% names()
  if (length(list_columns) == 0) {
    return(df)
  }
  for (l in list_columns) {
    df <- df %>% unnest_wider(l, names_sep = ".")
  }
  unnest_all(df)
}


#' Create a Date Reference Table
#'
#' @param start Start Date
#' @param end End Date
#' @param by The interval for which the date reference should be created by (determined by seq.Date())
#'
#' @return Tibble
#' @export

create_date_ref <- function(start, end, by) {

  # Check Start and End are Dates
  if(!any(str_detect(class(start), "POSIXt|Date"))) {
    start <- dmy(start, tz = .tz)
    if(is.na(start)) {
      stop("Start date must either be a character string with format d/m/Y or a datetime object")
    }
  }
  if(!any(str_detect(class(end), "POSIXt|Date"))) {
    end <- dmy(end, tz = .tz)
    if(is.na(end)) {
      stop("End date must either be a character string with format d/m/Y or a datetime object")
    }
  }

  # Create Table
  # browser()
  tibble(period.start = seq.Date(as_date(start), as_date(end), by = by)) %>%
    mutate(period.start = force_tz(period.start, .tz),
           month = period_label(period.start, "month"),
           month.year = period_label(period.start, "month.year"),
           fin.year = period_label(period.start, "fy"),
           fin.quarter = period_label(period.start, "finquarter"),
           fin.period = str_c(fin.quarter, "_", str_remove(fin.year, "FY")),
           period.end  = period.start + months(3) - 1,
           period.int = interval(period.start, period.end)
    )

}





#' Paste Path
#'
#' @return string
#' @export
#'
paste_path <- function() {
    raw_dir <- readClipboard()
    if (is.null(raw_dir)) {
      stop("Oops, your clipboard appears to be empty")
    }
    if (!str_detect(raw_dir, "\\\\")) {
      stop(stringr::str_glue("Hmm it seems you haven't provided a windows file directory path: \n{format(raw_dir)}"))
    }
    str_replace_all(raw_dir, "\\\\", "/")

}


#' Format dates into human readable format
#'
#' @param start Starting date
#' @param end End date
#'
#' @return String
#' @export
#'
format_date <- function(start, end) {
  if(!any(class(start) %in% c("POSIXct", "Date"))) {stop("Start variable does not appear to be a date variable")}
  if(!any(class(end) %in% c("POSIXct", "Date"))) {stop("End variable does not appear to be a date variable")}

  formatted.date <- str_c(format(start, "%d/%m/%y"), " to ", format(end, "%d/%m/%y"))

  formatted.date

}



#' Load Figures and Tables .RData
#'
#' @param file Location and name of figures and tables .RData object
#'
#' @return Saves the objects in Global Environment
#' @export

load.figs <- function(file = "objects/figs-tables"){
  dir <- str_glue("{file}.RData")
  load(dir, envir = globalenv())
  cat(crayon::green("Figures and Tables loaded in Global Environment"))
}


#' Save Figures and Tables
#'
#' @param dir Folder where to save object
#' @param name Name of .RData object
#'
#' @return .RData object saved in objects
#' @export
#'
save.figs <- function(dir = 'objects/', name = 'figs-tables'){
  save(list = ls(pattern = "plt\\.|tbl\\.|fig\\."), file = str_c(dir, name, ".RData"))
  cat(crayon::green(str_glue("Figures and Tables saved in {dir}{name}.RData")))
}



#' Stop R session without error
#'
#'`stop_quietly()` does exactly the same as `stop()`, however it does not print anything to the console.
#'
#' @author Stuart K. Grange
#'
#' @return Invisible, the R session will be stopped.
#'
#' @export
stop_quietly <- function() {

  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()

}



clean_filename = function(x){
  str_replace_all(x, "(?<!^\\w):|<|>|\"|\\||\\?|\\*|=", "_") %>%
    str_trim()
}

