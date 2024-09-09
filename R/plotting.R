#' Clean Theme
#' @description
#' A simple and clean theme for ggplot
#'
#'
#' @param base_size Base Size of Text
#' @param dark_text Colour of the Title
#'
#' @return ggplot2 theme
#' @export
#'
theme_clean <- function(base_size = 12,
                        dark_text = "#1A242F") {

  mid_text <-  monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[2]
  light_text <-  monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[3]

  theme_minimal(base_size = base_size) +
    theme(text = element_text(colour = mid_text,
                              family = "EnriquetaSB",
                              lineheight = 1.1),
          plot.title = element_text(colour = dark_text,
                                    # family = "EnriquetaSB",
                                    size = rel(1.6), margin = margin(12, 0, 8, 0)),
          plot.subtitle = element_text(size = rel(1.1), margin = margin(4, 0, 0, 0)),

          axis.text.y = element_text(colour = light_text, size = rel(0.8)),
          axis.title.y = element_text(size = 12, margin = margin(0, 4, 0, 0)),
          axis.text.x = element_text(colour = mid_text, size = 12),
          axis.title.x = element_blank(),

          legend.position = "top",
          legend.justification = 1,

          panel.grid = element_line(colour = "#F3F4F5"),
          panel.grid.major.x = element_blank(),

          plot.caption = element_text(size = rel(0.8), margin = margin(8, 0, 0, 0)),
          plot.margin = margin(0.25, 0.25, 0.25, 0.25,"cm"),

    )
}


#' save_gg
#' @description
#' Saves a ggplot to the specified format and prints to the viewer plane
#'
#'
#' @param plot A ggplot
#' @param file.name The name of the file without format
#' @param overwrite Overwrite existing file of same file.name (default == TRUE)
#' @param dir Directory where to save image. Default output/figs
#' @param width Width of plot. Default is 9
#' @param height Height of plot. Default is widdth * 9/16
#' @param dpi Default 300
#' @param format The file format (default .png)
#'
#' @return Image file
#' @export save_gg
#'
save_gg <- function(plot, file.name = NULL, format = "png", overwrite = TRUE, dir = 'outputs/plots/', open = TRUE, width = 9, height = NULL, dpi = 300, ...) {
  # browser()
  if(!"ggplot" %in% class(plot)) {
    stop("Object does not appear to be a ggplot object. Please try again.")
  }

  if(is.null(height)) {
    height <- width * 9 / 16
  }

  # Remove . from format
  if(str_detect(format, "^\\.")) {
    format <- str_remove(format, "^\\.")
  }

  # Initialise camcorder to save object to dir
  camcorder::gg_record(
    dir = dir,
    width = width,
    height = height,
    dpi = 300,
    bg = NULL,
    device = format,
    ...

  ) %>% suppressWarnings()

  print(plot)

  camcorder::gg_stop_recording()

  # Gets the name of the most recent file in folder (the one created by camcorder)
  printed_file <- file.info(list.files(dir, full.names = TRUE)) %>%
    arrange(desc(ctime)) %>% slice(1) %>% rownames() %>%
    str_remove(dir)

  if(is.null(file.name)){
    new_name <- str_c(str_extract(printed_file, ".{19}"), ".", format) # Default new names are given the format YYYY_MM_DD_HH_MM_SS

    file.rename(str_c(dir, printed_file), str_c(dir, new_name))
  } else {
    new_name <- str_c(file.name, ".", format)

    if(overwrite) {
      file.rename(str_c(dir, printed_file), str_c(dir, new_name))
    } else(stop("Need to create options for overwrite"))
  }
  cat(crayon::green(str_glue("File Saved at {dir}{new_name}")))

  # Open the plot
  if(open) shell.exec(normalizePath(str_c(dir, new_name)))
}


#' view_gg
#' @description
#' Prints a ggplot to the viewer window.
#' view_gg calls camcorder::gg_record() which prints a ggplot to the viewer window, so that plot measurements can be viewed more accurately.
#'
#'
#' @param plot ggplot object
#' @param width Width of the plot in inches
#' @param height Default is for height to be set as a function of width. This can be overwritten with a manual value
#' @param dpi The dpi. Default is 300.
#'
#' @return An Image
#' @export view_gg
#'
view_gg <- function(plot, width = 9, height = NULL, dpi = 300, ...) {
  if(!"ggplot" %in% class(plot)) {
    stop("Object does not appear to be a ggplot object. Please try again.")
  }

  if(is.null(height)){
    height <- width * 9 / 16
  }

  camcorder::gg_record(
    dir = "tmp/",
    width = width,
    height = height,
    dpi = 300,
    bg = NULL,
    ...
  ) %>% suppressWarnings()

  print(plot)

  camcorder::gg_stop_recording()

  # Gets the name of the most recent file in folder (the one created by camcorder)
  printed_file <- file.info(list.files("tmp/", full.names = TRUE)) %>%
    arrange(desc(ctime)) %>% slice(1) %>% rownames()

  file.remove(printed_file)



}


#' stop_gg
#' @description
#' A function that calls camcorder::gg_stop_recording()
#'
#'
#' @return Stops record_gg()
#' @export stop_gg
#'
stop_gg <- function() {
  camcorder::gg_stop_recording()
}


#' record_gg
#' @description
#' Uses camcorder::gg_record() to save and view ggplots as they would appear when exported.
#' When record_gg is used, subsequent ggplots will be saved in the specified folder (default 'outputs/imgs') and the plot
#' will be viewed in the viewer pane. Use stop_gg() to stop record_gg()
#'
#'
#' @param dir The directory in which to save plots to (default is outputs/imgs)
#' @param width Width in inches of the image (default is 9)
#' @param height Height is calculated as a function of width. It will always be less than width
#' @param dpi Default 300
#'
#' @return Image in folder
#' @export record_gg
#'
record_gg <- function(dir = 'outputs/imgs', width = 9, dpi = 300) {
  camcorder::gg_record(
  dir = imgs,
  width = width,
  height = width * 9 / 16,
  dpi = dpi,
  bg = NULL
)
}


#' Prepare dataframe for ordering categories across facets
#' @description
#' IN DEVELOPMENT. Prepares a dataframe for easy ordering of a categorical variable across groups/facets used within ggplot.
#' Data must contain summarised counts and/or proportions of a categorical variable and a grouping variable that is used for faceting.
#'
#'
#' @param dat Dataframe including at least a categorical variable, a count or proportion of the categorical variable and a grouping variable
#' @param facet The variable which you want to facet/group by
#' @param x Categorical variable
#' @param y Count/Proportion of the categorical variable
#' @param smallest.left Defaults to ordering the categorical variables so that the smallest count/proportion is on the left within each facet
#'
#' @return Dataframe with categorical variable prepped for faceting
#' @export
#'
facet.arrange <- function(dat, facet, x, y, smallest.left = TRUE){
  # browser()
  ### ADD CHECK FOR GROUPED DF (will fail if grouped)
  x.name <- names(select(dat, {{x}}))
  y.name <- names(select(dat, {{y}}))

  if(is.numeric(dat[[x.name]])) stop("\"x\" must be a character or factor variable")
  if(!is.numeric(dat[[y.name]])) warning(crayon::red("\"y\" must be a numeric summary of x"))

  dat <- dat %>%
    group_by({{facet}}, {{x}}) %>%
    arrange(desc({{y}})) %>%
    ungroup()

  # Arranges x from smallest to largest y (left to right) or largest to smallest (top to bottom) if using coord_flip()
  if(smallest.left){
    dat <- mutate(dat, tmp = factor(paste({{x}}, {{facet}}, sep = "__"),
                                    levels = rev(paste({{x}}, {{facet}}, sep = "__"))))
  } else{ # Arranges columns in opposite direction
    dat <- mutate(dat, tmp = factor(paste({{x}}, {{facet}}, sep = "__"),
                                    levels = paste({{x}}, {{facet}}, sep = "__")))
  }

  # Workaround for ensuring the same names are kept in the dataframe
  dat <- dat %>%
    select(-x.name) %>%
    rename_with(~x.name, tmp)

  dat

  # Use scale_x_discrete(labels = ~str_remove(., \"__.+\") to remove facet label from x names
}
