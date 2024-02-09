#' Create New R Project
#'
#' @description
#' Creates and opens a new R project with the template files in the AzTools package.
#' Initialises a git repository and performs an initial commit.
#'
#'
#' @param name            Name of the Project
#' @param dir             Directory of the Project
#' @param initial_commit  Initialise git repository and perform an initial commit
#'
#' @returns               Nothing
#' @export

create_project <- function(name, dir, init_git = TRUE) {
  name <- stringr::str_c(name)

  # Join Directory and Name
  proj_dir <- stringr::str_c(dir, name)

  if(dir.exists(proj_dir)) {
    stop("Folder with this name already exists")
  }

  proj_dir <- str_replace_all(proj_dir, "//", "/") %>%
    clean_filename()

  dir.create(proj_dir)
  setwd(proj_dir)
  .GlobalEnv$.working.d <- proj_dir # Set global environment as new project directory

  R.utils::copyDirectory(system.file("template", package = "AzTools"), proj_dir)

  file.rename("PROJECT_NAME.Rproj", str_c(name, ".Rproj"))

  clean_name <- clean_filename(name) %>% str_replace_all(" ", "-")

  if(init_git) {
    git2r::init()
  }

  # Look at usethis::create_project to see what needs to be done

}
