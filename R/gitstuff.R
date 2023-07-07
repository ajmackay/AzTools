#' Post Issue to AzTools Repository
#'
#' @param title Title of the Issue
#' @param body Description of Issue
#'
#' @return
#' @export
#'
post.issue <- function(title, body) {
  gh::gh(endpoint = "POST /repos/ajmackay/AzTools/issues", title = title, body = body)
}

