#' Format dates into human readable format
#'
#' @param start Starting date
#' @param end End date
#'
#' @return String
#' @export format_date
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



