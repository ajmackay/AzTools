# Actions to be executed when the package is loaded
.onLoad <- function(libname, pkgname) {

  print("AzTools has been loaded my dude.")

  .GlobalEnv$.tz = "Australia/Sydney"

  # update_geom_defaults("bar", list(colour = "black", fill = "#EB7021"))
  # update_geom_defaults("col", list(colour = "black", fill = c("#EB7021")))
  #
  # options(ggplot2.discrete.fill = .colours$cat)
  # options(ggplot2.ordinal.fill = .colours$cat)
  # options(ggplot2.discrete.colour = .colours$cat)
  # options(ggplot2.ordinal.colour = .colours$cat)

}
