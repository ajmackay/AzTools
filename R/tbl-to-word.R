#' Save flextable in a word doc with
#'
#' @param dat A dataframe to summarise
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


tbl.to.word <- function(tbl.list){
  map(names(tbl.list), function(x){
    tbl <- tbl.list[[x]]
    full.path <- str_c(out.path, today(), "_", x, ".docx")
    save_as_docx(tbl, path = full.path)
    # browser()
  })
}
