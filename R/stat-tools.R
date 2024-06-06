#' Wilson Score Confidence Intervals
#' @description
#' Calculates the low and high confidence intervals for proportions using the Wilson Score Interval Method.
#'
#'
#' @param num Numerator of proportion
#' @param denom Denominator of proportion
#' @param ... Additional arguments passed to prop.test
#'
#' @return Tibble with conf.low and conf.high
#' @export
#'
wilson_ci <- function(num, denom, ...) {
  wilson_score <- purrr::map2(.x = num, .y =  denom, function(x, y) {
    prop.test(x, y, correct = TRUE, ...) %>% tidy()
  })
  # browser()

  tibble(
    conf.low = map_dbl(wilson_score, ~.x$conf.low),
    conf.high = map_dbl(wilson_score, ~.x$conf.high)
  )
}
