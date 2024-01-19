prep.table = function(data, digits = 2, format.numeric = TRUE, format.date = FALSE, row.order = list(), merge.rows = NULL, spanner = TRUE,hide.cols = NULL, vars = "all", ...) {
  if (is.empty(data)) {
    data = tibble(Message = "This table is empty")
  }
  original.names = names(data)

  data = row.order(data, row.order = row.order)

  if(format.numeric) {
    data <- mutate(data, across(where(is.numeric), ~format.number(.x, digits)))
  } else {
    data = mutate(data, across(where(is.numeric), ~round(.x, digits)), across(everything(), as.character))
  }

  if(format.date) {
    data <- mutate(data, across(where(is.POSIXt), ~format.date(.x, ...)))
    data <- mutate(data, across(where(is.Date), ~format.date(.x, ...)))
  }

  data.h = merge.rows(data, merge.rows) %>% prettify(vars = vars, ..., underscore = !spanner) %>% mutate(across(where(is.numeric), ~to.char(.x, digits)), across(everything(), as.character))
  grouping.vars = group_vars(data.h)
  var.names = names(data.h)
  if ("labels" %not.in% names(list(...))) {
    labels = TRUE
  } else {
    labels = list(...)$labels
  }
  if (labels) {
    prettify.args = list(...)[which(names(list(...)) %in% c("abbreviations", "lowercase", "pronouns"))]

  }
  if (getOption("dcutilities.prep.table", default = "flex") == "flex") {
    #  flextable::set_flextable_defaults(font.family = "Calibri", font.size = 10, font.color = "black", table.layout = "autofit", padding = 3, line_spacing = 1.2, arraystretch = 1.2, tabcolsep = 1)
    data.h = relocate(data.h, all_of(grouping.vars))
    to.merge = c(grouping.vars, merge.rows)

    keys = names(data.h)
    if(!is.null(hide.cols)){

      to.hide = which(original.names %in% hide.cols)
      keys = keys[-to.hide]
    }
    t = flextable::flextable(data.h, col_keys = keys) %>% flextable::hline()
    if (!is.empty(to.merge)) {
      t = flextable::merge_v(t, j = to.merge) %>% flextable::valign(j = to.merge, valign = "top")
    }
    t = flextable::separate_header(t, split = "_")
    if (labels) {
      t = t %>% flextable::labelizor(part = "header", labels = function(x) {
        pretty.labels(x, ...)
      })
    }
  } else {
    row.names = var.names[!var.names %in% grouping.vars][1]
    row.names = which(var.names == row.names)
    row.names = names(data.h)[row.names]
    t = gt(data.h, rowname_col = row.names) %>% tab_stubhead(row.names)
    if (spanner) {
      t = tab_spanner_delim(t, "_")
    }
    if (labels) {
      t = text_transform(t, function(x) {
        prettify.args$word = x
        do.call(initial.upper, prettify.args)
      }, cells_column_labels())
    }
  }
  return(t)
}
