#' Set Flextable Defaults
#'
#' @param font.family
#' @param font.size
#' @param layout
#' @param padding
#' @param theme_fun
#' @param digits
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

ft.default <- function(font.family = 'Calibri',
                           font.size = 10,
                           layout = 'autofit',
                           padding = 6,
                           theme_fun = 'theme_vanilla',
                           digits = 3, ...) {

  flextable::set_flextable_defaults(
    font.family = font.family,
    font.size = font.size,
    layout = layout,
    padding = padding,
    theme_fun = theme_fun,
    digits = digits)

  if(digits != 3){
    cat(crayon::red("Use colformat_double() to adjust digits"))
  }

  cat(crayon::green("Flextable Default Set"))

}

