

#' Creating a list of custom column definitions for use in reactables
#'
#' @param rawColNames The raw column names taken directly from the source
#'  data table that are to be overwritten in the reactable
#' @param niceColNames The formatted column names that will appear as-specified in 
#' the reactable
#' @param tooltipText The text to be displayed in a toolTip when hovering over the 
#' column in the reactable
#'
#' @return A named list of reactable::colDef objects
#' @export
#'
#' @examples
#' createCustomColDefList(rawColNames = c("firstName", "lastName), niceColNames = c("First Name", "Last Name"), tooltipText = c("The person's first name", "The person's last name"))
#' 
createCustomColDefList <- function(rawColNames, niceColNames, tooltipText) {
  withTooltip <- function(value, tooltip, ...) {
    shiny::div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
               tippy::tippy(value, tooltip, ...))
  }
  
  result <- vector("list", length(rawColNames))
  
  for (i in 1:length(rawColNames)) {
    result[[i]] <- reactable::colDef(
      header = withTooltip(niceColNames[[i]], tooltipText[[i]])
    )
  }
  
  names(result) <- rawColNames
  
  return(result)
}
