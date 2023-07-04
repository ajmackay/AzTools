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
