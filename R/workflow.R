#' Create New R Project
#'
#' @description
#' Creates and opens a new R project with the template files in the AzTools package.
#' Initialises a git repository and performs an initial commit.
#'
#'
#' @param path            Path where new project should be created
#' @param open Logical: Should tyhe new project be opened after creation?
#'
#' @returns Invisibly returns the path to the new project
#' @export

new_project <- function(path, open = TRUE) {

  # Create project
  usethis::create_project(path = path, open = FALSE, rstudio = TRUE)

  # Get template files
  template_dir <- system.file("template", package = "AzTools")

  template_files <- list.files(template_dir, full.names = TRUE, recursive = TRUE)

  # Copy template files

  for (template_file in template_files) {
    rel_path <- sub(paste0("^", template_dir, "/?"), "", template_file)

    dest_file <- file.path(path, rel_path)

    dest_dir <- dirname(dest_file)
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Check if file exists
    if (file.exists(dest_file)) {
      message("Skipping existing file")
    } else {
      file.copy(template_file, dest_file, overwrite = TRUE)
    }
  }

  if (open) {
    if(requireNamespace("rstudioapi", quietly = TRUE) &&
       rstudioapi::isAvailable()) {
      rstudioapi::openProject(path)
    }
  }

  message("Project Created")

}

