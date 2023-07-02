create.project <- function (project.name, dir, github = TRUE, initial.commit = TRUE){

  project.name <- str_trim(project.name)

  project.dir <- str_glue("~/{dir}/{project.name}")

  if(dir.exists(project.dir)) {
    stop("Folder with this name already exists")
  }

  dir.create(project.dir)
  setwd(project.dir)
  .GlobalEnv$.working.d = project.dir
  R.utils::copyDirectory(system.file("template", package = "dcutilities"),
                         project.dir)


  file.rename("PROJECT_NAME.Rproj", str_c(project.name, ".Rproj"))
  clean.project.name = str_replace_all(project.name, " ", "-")
  readLines("quarto-template.qmd") %>% str_replace("@REPORT-NAME-HTML",
                                                   str_c(clean.project.name, ".html")) %>% str_replace("@REPORT-NAME-QMD",
                                                                                                       str_c(clean.project.name, ".qmd")) %>% str_replace("@REPORT-NAME",
                                                                                                                                                          project.name) %>% writeLines("quarto-template.qmd")
  file.rename("quarto-template.qmd", str_c(clean.project.name,
                                           ".qmd"))
  document.name = str_replace_all(project.name, " ", "-")
  readLines("word-template.qmd") %>% str_replace("@OUTPUT-NAME",
                                                 str_c(document.name, ".docx")) %>% str_replace("@TEMPLATE-NAME",
                                                                                                str_c(document.name, "_word.qmd")) %>% str_replace("@REPORT-NAME",
                                                                                                                                                   project.name) %>% writeLines("word-template.qmd")
  file.rename("word-template.qmd", str_c(project.name, "_word.qmd"))
  saveRDS(list(), "./cache/outputs")
  if (!is.null(default.queries)) {
    library(rforce)
    target.queries = map_chr(default.queries, function(query) {
      switch(query, Cases = "cases", NPI = "npi", `Contributing Factors` = "contributing.factor",
             Milestones = "milestones", `Milestone Breaches` = "kpi.breaches")
    }) %>% str_c(collapse = "|")
    tibble(queries = system.file("default.queries", package = "rforce") %>%
             list.files(full.names = TRUE)) %>% filter(str_detect(queries,
                                                                  target.queries)) %>% pull(queries) %>% walk(~file.copy(.x,
                                                                                                                         "./cache/queries"))
    .GlobalEnv$query.list = load.queries()
  }
  if (initial.commit) {
    git2r::init()
    git.add()
    git.commit(message = "initial")
  }
  invisible(settings(reset = TRUE))
}
