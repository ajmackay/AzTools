#' Split a string
#'
#' @param x A character vector with one element
#' @param split What to split on
#'
#' @return A charactor vector
#' @export
#'
#' @examples
#' x <- "alpha,bravo,charlie,delta"
#' strsplit1(x, split = ",")
strsplit1 <- function(x, split){
  strsplit(x, split = split)[[1]]
}
