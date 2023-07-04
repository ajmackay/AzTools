#' Stop R session without error
#'
#'`stop_quietly()` does exactly the same as `stop()`, however it does not print anything to the console.
#'
#' @author Stuart K. Grange
#'
#' @return Invisible, the R session will be stopped.
#'
#' @export
stop_quietly <- function() {

  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()

}


#' Load a Single Object
#'
#' `load.object()` Loads the specified name of the RDS object located in data/objects/.
#' The .rds object must be assigned to an object in the workspace to be saved in the global environment.
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
#' `save.object()` Saves a specified object as a .rds file in data/objects/
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
#' `load.objects()` Loads a specified .RData file (default is "all-objects.RData") from data/objects/ into the global environment
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


