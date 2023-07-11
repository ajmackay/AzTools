#' Save Flex Table
#' @description
#' Saves a flextable object in the preferred format to the tables folder in the output directory
#'
#'
#' @param dat Flextable Object
#' @param name String: Name of file to be saved
#' @param format Format for file saving (default .docx)
#'
#' @return File
#' @export
#'
save.flex <- function(dat, name, format = c("word")){
  # browser()
  if(any(dir.exists(c("output", "outputs")))){
    output.dir <- stringr::str_c(list.dirs()[str_detect(list.dirs(), "^./outpu[ts]$")], "/")

    if(any(stringr::str_detect(list.dirs(output.dir), "tables"))){
      output.dir <- stringr::str_c(output.dir, "tables/")
    } else{
      dir.create(output.dir, "tables")
      output.dir <- stringr::str_c(output.dir, "tables/")
    }

  } else{
    # dir.create("output")
    dir.create("output/tables")
    output.dir <- "output/tables/"
  }

  if(format == "word" & class(dat) == 'flextable'){
    full.dir <- stringr::str_c(output.dir, name, ".docx")

    flextable::save_as_docx(dat, path = full.dir)

    cat(crayon::green(stringr::str_glue("Table saved in {full.dir}")))
  }

}


#' Set Flextable Defaults
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

#' Export a flextable into a .xlsx file
#'
#' @param table A flextable
#' @param path Path to the Excel file to be created
#' @return Returns an .xlsx file based on a flextable
#' @export
#'
#' @examples
#' \dontrun{
#' ft <- flextable::flextable(head(mtcars))
#' # color some cells in blue
#' ft <- flextable::bg(ft, i=ft$body$dataset$disp>200, j=3, bg = "#7ed6df", part = "body")
#' # color a few cells in yellow
#' ft <- flextable::bg(ft, i=ft$body$dataset$vs==0, j=8, bg = "#FCEC20", part = "body")
#' # export your flextable as a .xlsx in the current working directory
#' exportxlsx(ft, filename ="myFlextable", path="path/to/the/excel_file.xlsx")
#' }

ft.xlsx = function(table, path) {

  # setwd(path) # Indique le repertoire ou sera enregistrer le fichier excel

  data = table$body$dataset
  bgcolor = as.data.frame(table$body$styles$cells$background.color$data)

  wb = openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "feuille1")
  openxlsx::writeData(wb,1,data)

  for (desc in 2:ncol(data)) {

    for (prod in 1:nrow(data)) {

      # on recupere la couleur de la cellule
      cell.bgcolor = bgcolor[prod,desc]

      # on cree un style pour la cellule
      cell.style = openxlsx::createStyle(numFmt = "0.000", border = c("top", "bottom", "left", "right"), borderColour = "black", fgFill = ifelse(cell.bgcolor=="transparent","#FFFFFF",cell.bgcolor), halign = "center")

      # on applique le style a la cellule
      openxlsx::addStyle(wb,sheet=1,style = cell.style, rows = prod+1, cols = desc)

    }
  }

  openxlsx::saveWorkbook(wb,path, overwrite = TRUE)

}


#' Prepare and Output a Flextable
#'
#' @param dat Dataframe
#' @param prep.names Turn Names into Titles
#'
#' @return flextable
#' @export

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
#' @export
#'
#' @examples
#'
#'
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



#' Basic Summary Table
#'
#' @param dat A dataframe to summarise in wide format (one column for each variable to summarise)
#' @param summ.by Variable to stratify summary table by
#' @param dp Number of decimal places
#' @param caption Table Caption
#' @param include.p Include P value
#'
#' @return A gtsummary table
#' @export
#'
#' @examples
#'
#'
flex.summary() <- function(dat, summ.by = NULL, dp = 1, caption = NULL, include.p = FALSE){
  tbl.summary <- gtsummary::tbl_summary(dat,
                             by = summ.by,
                             statistic = list(all_continuous() ~ "{mean} ({sd})"),
                             digits = list(
                               all_continuous() ~ dp,
                               all_categorical() ~ dp
                             ),
                             missing_text = "Missing",
                             sort = list(everything() ~ "frequency")) %>%
    gtsummary::bold_labels()

  if(include.p){
    tbl.summary <- tbl.summary %>%
      gtsummary::add_p()
  }

  tbl.summary %>%
    gtsummary::as_flex_table() %>%
    flextable::colformat_double(digits = dp) %>%
    # fontsize(size = 10) %>%
    flextable::set_caption(caption)
}

