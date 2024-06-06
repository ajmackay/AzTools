#' Date Label
#' @description Label Dates
#'
#'
#'
#' @param date Vector of dates to generate labels for
#' @param type Character for which type of label to generate. Only the first one will be used. These labels can be combined into any arbitrary order \cr
#' or combination by joining different components with a '.'. E.g., if you wanted a label in the format MMM-YY, you would do month.year, with sep = '-', or if instead you wanted YY_MMM, \cr
#' use year.month, with a sep = _. There is no minimum or maximum number of components for the combination (i.e., year.month.day is valid). \cr
#' report is report period, quarter and half are quarter/half a year, day is day of year.
#' @param as.factor Logical for whether to return as an ordered factor (TRUE), or a character(FALSE). Alternatively, a character/factor vector can be passed as the levels to be used for the resulting factor variable. Any values in the data that don't appear in the provided vector will be converted to NA.
#' @param sep Separator between the months and years for month.year and report.period
#'
#' @return Ordered factor or character vector of labels for each date
#' @export
#'
period_label = function(date, type = c("day", "week",
                                       "fortnight",
                                       "month", "quarter", "half", "year", "fy", "finmonth", "finquarter"),
                        as.factor = TRUE, sep = "_"){
  target.type = type[1] %>% stringr::str_split("\\.") %>% unlist()
  formatted.data = tibble(target.date = date,
                          year = stringr::str_extract(year(target.date), "\\d{2}$"),
                          month = lubridate::month(target.date),
                          day = lubridate::yday(target.date),
                          week = lubridate::week(target.date),
                          fortnight = ceiling(week/2),
                          quarter.n = lubridate::quarter(target.date),
                          quarter = dplyr::case_when(quarter.n == 1 ~ "Jan_Mar",
                                              quarter.n == 2 ~ "Apr_Jun",
                                              quarter.n == 3 ~ "Jul_Sep",
                                              quarter.n == 4 ~ "Oct_Dec") %>%
                            stringr::str_replace_all("_", sep),
                          half = if_else(month <= 6, stringr::str_glue("Jan{sep}Jun"), stringr::str_glue("Jul{sep}Dec")),
                          fy = if_else(month >= 7, as.integer(year) + 1, as.integer(year)) %>% str_c("FY", .),
                          finmonth = if_else(month >=7, month - 6, month + 6) %>% str_c("FM", .),
                          finquarter = if_else(month >= 7, quarter.n - 2, quarter.n + 2) %>% str_c("FQ", .)) %>%
    mutate(month = month(target.date, label = TRUE)) %>%
    select(target.date, all_of(target.type)) %>%
    mutate(label = str_c(!!!syms(target.type), sep = sep))

  if(!isFALSE(as.factor)){
    if(isTRUE(as.factor)) {
      as.factor = arrange(formatted.data, target.date) %>% distinct(label) %>% pull(label)
    }else if(!all(unique(formatted.data$label) %in% as.factor)){
      warning("Time periods in data that are not in as.factor have been replaced with NA")
    }
    formatted.data = mutate(formatted.data, label = factor(label,levels = as.factor, ordered = TRUE))
  }
  pull(formatted.data, label)
}
