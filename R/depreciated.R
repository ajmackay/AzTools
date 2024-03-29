#' DEPRECIATING: Use set.flex.default() instead
#'
#' @param font.size Default is 10
#' @param font.family Default is Calibri
#' @param theme Default is vanilla
#' @param padding Default is 6
#'
#' @return Flextable Defaults
#' @export

ft.set.default <- function(font.size = 10, font.family = 'Calibri', padding = 6, digits = 1, na.str = "-") {
  flextable::set_flextable_defaults(
    font.size = font.size,
    font.family = font.family,
    theme_fun = 'theme_vanilla',
    padding = padding,
    table.layout = 'autofit',
    digits = digits,
    na_str = na.str
  )

  cat(crayon::green('Flextable Defaults Set:\n', '- Font Size:', font.size, '\n - Font Family:', font.family,
                    '\n - Padding:', padding, '\n - NA String:', na.str))

  warning("Need to call colformat_double() after flextable() in order for digits to take effect")

}


#' USE prep.flex() INSTEAD Prepare and Output a Flextable
#'
#' @param dat Dataframe
#' @param prep.names Turn Names into Titles
#'
#' @return flextable
#' @export ft.prep

ft.prep <- function(dat, prep.names = TRUE, digits = 1) {
  if(prep.names){
    old.names <- names(dat)
    new.names <- str_replace_all(old.names, "\\.|_", " ") %>%
      str_to_title()

    names(dat) <- new.names
  }

  dat %>%
    flextable::flextable() %>%
    flextable::colformat_double(digits = digits)

}


#' DEPRECIATED please use flex.summary()
#'
#' @param dat A dataframe to summarise in wide format (one column for each variable to summarise)
#' @param summ.by Variable to stratify summary table by
#' @param dp Number of decimal places
#' @param caption Table Caption
#' @param include.p Include P value
#'
#' @return A gtsummary table
#' @export ft.prep

ft.summary <- function(dat, summ.by = NULL, dp = 1, caption = NULL, include.p = FALSE){
  tbl.summary <- tbl_summary(dat,
                             by = summ.by,
                             statistic = list(all_continuous() ~ "{mean} ({sd})"),
                             digits = list(
                               all_continuous() ~ dp,
                               all_categorical() ~ dp
                             ),
                             missing_text = "Missing",
                             sort = list(everything() ~ "frequency")) %>%
    bold_labels()

  if(include.p){
    tbl.summary <- tbl.summary %>%
      gtsummary::add_p()
  }

  tbl.summary %>%
    as_flex_table() %>%
    flextable::colformat_double(digits = dp) %>%
    # fontsize(size = 10) %>%
    set_caption(caption)
}
