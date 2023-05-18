prep.names <- function(dat) {
  names.current <- names(dat)
  names.new <- names.current %>%
    str_replace_all("\\.|_")
}
