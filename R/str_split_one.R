#' Split a string
#'
#' @param string A character vector with, at most, one element.
#' @inheritParams stringr::str_split
#'
#' @return A charactor vector
#' @export
#'
#' @examples
#' x <- "alpha,bravo,charlie,delta"
#' str_split_one(x, pattern = ",")
str_split_one <- function(string, pattern, n = Inf){
  stopifnot(is.character(string), length(string) <= 1)
  if(length(string) == 1){
    stringr::str_split(string = string, pattern = pattern, n = n)
  } else {
    character()
  }
}
