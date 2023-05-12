#' Basic Summary Table
#'
#' @param dat A dataframe to summarise in wide format (one column for each variable to summarise)
#' @param summ.by Variable to stratify summary table by
#' @param dp Number of decimal places
#' @param caption Table Caption
#'
#' @return A gtsummary table
#' @export
#'
#' @examples
#'
#'
summ.tbl <- function(dat, summ.by = NULL, dp = 1, caption = NULL){
  tbl_summary(dat,
              by = summ.by,
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(
                all_continuous() ~ dp,
                all_categorical() ~ dp
              ),
              missing_text = "Missing",
              sort = list(everything() ~ "frequency")) %>%
    bold_labels() %>%
    as_flex_table() %>%
    fontsize(size = 10) %>%
    theme_zebra() %>%
    set_caption(caption) %>%
    set_table_properties(layout = "autofit")
}
