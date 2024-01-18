#' Extract Time Units
#'
#' @param data Dataframe with at least one datetime variable
#' @param date_var Datetime variable to extract datetime unit from
#' @param time_units The units of datetime to extract as a string e.g. c("year", "month", "day")
#'
#' @return A dataframe with specified datetime units
#' @export
#'
#' @examples
extract_time <- function(data, date_var, time_units) {
  data %>%
    timetk::tk_augment_timeseries_signature({{date_var}}) |>
    select(
      all_of(
        c(colnames(data), time_units)
      )
    )
}
