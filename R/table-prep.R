#' Prepare Names
#'
#' @param dat Dataframe
#'
#' @return
#' @export

prep.names <- function(dat) {
  old.names <- names(dat)
  new.names <- str_replace(old.names, "\\.|_", " ") %>%
    str_to_title()

  names(dat) <- new.names

  dat
}
