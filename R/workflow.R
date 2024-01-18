# This function will:
# Create a new folder in your R directory
# Copy a template for analysis into the new folder
# Set the working directory to the newly created folder
#
# project.name: Name of the project, will be the name of the folder created
# alt.dir: A location other than your R folder to create the new analysis

#' new.project
#'
#' @description Initialises a new project with a folder structure and git repository. Project will be created in the default R directory unless alt.dir is supplied
#'
#' @param project.name Character of the name of the project
#' @param alt.dir      Character of a directory to save the project that isn't the default
#' @param shiny        Logical: Whether to include components for a shiny application in the project
#' @param data.ticket logical for whether this project is for a data ticket in the data ticket folder. If true, then a git repository won't be initialised.
#' @param default.queries Character vector of default queries to be included in the new project at setup
#' @param initial.commit Logical for whether to automatically do the initial git commit after setting up (mainly used for development purposes)
#'
#' @return             Nothing
#' @export
#'
#' @import knitr dplyr purrr stringr ggplot2 tibble tidyr utils fs shiny tidyselect readr lubridate
new.project = function(project.name, alt.dir = NULL, shiny = FALSE, data.ticket = FALSE, default.queries = NULL, initial.commit = TRUE){
  project.name = str_trim(project.name)
  if(is.null(alt.dir)){
    project.dir = str_c(.r.dir,project.name)
  }else{
    project.dir = str_c(alt.dir,"/", project.name)
  }

  if(dir.exists(project.dir)){
    stop("Folder with this name already exists")
  }
  project.dir =
    str_replace_all(project.dir, "//", "/") %>%
    clean.filename()
  dir.create(project.dir)
  setwd(project.dir)
  .GlobalEnv$.working.d = project.dir
  R.utils::copyDirectory(system.file("template", package = "dcutilities"), project.dir)

  # Alternative Template
  if(getOption("dcutilities.scripts.setup", default = "standard") == "alternate") {
    # Copy over alternate scripts
    R.utils::removeDirectory("scripts", recursive = TRUE)
    R.utils::copyDirectory(system.file("alternate", package = "dcutilities"), project.dir)
  }


  file.rename("PROJECT_NAME.Rproj", str_c(project.name, ".Rproj"))


  # clean.project.name = str_replace_all(project.name, " ", "-")
  clean.project.name = clean.filename(project.name) %>% str_replace_all( " ", "-")

  readLines("quarto-template.qmd") %>%
    str_replace("@REPORT-NAME-HTML", str_c(clean.project.name, ".html")) %>%
    str_replace("@REPORT-NAME-QMD", str_c(clean.project.name, ".qmd"))  %>%
    str_replace("@REPORT-NAME", project.name) %>%
    writeLines("quarto-template.qmd")

  readLines("quarto-template-v2.qmd") %>%
    str_replace("@REPORT-NAME", str_c(clean.project.name, "2")) %>%
    writeLines("quarto-template-v2.qmd")

  file.rename("quarto-template-v2.qmd", str_c(clean.project.name, "2.qmd"))


  file.rename("quarto-template.qmd", str_c(clean.project.name, ".qmd"))

  document.name = str_replace_all(project.name, " ", "-")

  # Update README title
  readLines("README.md") %>%
    str_replace("@PROJECT-NAME", project.name) %>%
    writeLines("README.md")




  saveRDS(list(), "./cache/outputs")

  if(!is.null(default.queries)){
    library(rforce)


    target.queries =
      map_chr(default.queries, function(query) {
        switch(query, Cases = "cases", NPI = "npi",
               `Contributing Factors` = "contributing.factor", Milestones = "milestones",
               `Milestone Breaches` = "kpi.breaches")
      }) %>%
      str_c(collapse = "|")


    tibble(queries = system.file("default.queries", package = "rforce") %>% list.files(full.names = TRUE)) %>%
      filter(str_detect(queries, target.queries)) %>%
      pull(queries) %>%
      walk(~file.copy(.x, "./cache/queries"))

    # if("rforce" %not.in% (.packages())){
    #   library(rforce)
    # }
    # map(default.queries, function(query){
    #   query.name = switch(query, "Cases" = "case.q", NPI = "npi.q", "Contributing Factors" = "cf.q", "Milestones" = "milestone.q", "Milestone Breaches" = "breach.q")
    #   create.default(query.name, query)
    # }) %>%
    #   save.queries()
    .GlobalEnv$query.list = load.queries()
  }

  if(initial.commit){
    git2r::init()
    git.add()
    git.commit(message = "initial")
  }

  invisible(settings(reset = TRUE))

}
