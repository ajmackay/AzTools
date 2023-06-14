#' Load a Single Object
#'
#' @param name File name of the object
#'
#' @return Loads the object from data/objects
#' @export
load.object <- function(name){
  readRDS(str_c("data/objects/", name, ".rds"))
}


#' Save a single object
#'
#' @param obj The object to Save
#' @param name File name of the Object
#'
#' @return Saves the object in data/objects/
#' @export
#'
#' @examples
save.object <- function(obj, name){
  saveRDS(obj, file = str_c("data/objects/", name, ".rds"))
}

#' Load all objects
#'
#' @param dataset String of the name of the dataset to load. Default is all-objects
#' @return Loads all objects saved in all-objects.RData into the environment
#' @export
load.objects <- function(dataset = "all-objects"){
  obj.dir <- str_c("data/objects/", dataset, ".RData")

  if(!file.exists(obj.dir)) stop("No Data File Found")

  load(obj.dir, envir = globalenv())
  cat(crayon::green("All Objects Loaded in the Global Environment"))
}




#' Save all objects
#'
#' @param dir directory where all objects should be saved (default is data/objects)
#'
#' @return Saves all the objects and shit
#' @export
save.objects.all <- function(dir = NULL){
  if(is.null(dir)){
    dir <- 'data/objects/'
  } else{
    dir <- dir
  }

  save.image(file = str_c(dir, 'all-objects.RData'))
  cat(crayon::green("All Objects Saved Successfully"))
}


#' Load Packages
#'
#' @return Loads packages in load-packages.R
#' @export
#'
#' @examples
load.packages <- function() {
  if(!exists("packages")){
    source("scripts/load-packages.R")
  }
}


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
