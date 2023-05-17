

#' Creating a list of custom column definitions for use in reactables
#'
#' @param rawColNames The raw column names taken directly from the source
#'  data table that are to be overwritten in the reactable
#' @param niceColNames The formatted column names that will appear as-specified in 
#' the reactable
#' @param tooltipText The text to be displayed in a toolTip when hovering over the 
#' column in the reactable
#' @param case Optional argument to convert raw column names to snake or camel case. Defaults to NULL and preserves
#' whatever raw column names are passed in
#'
#' @return A named list of reactable::colDef objects
#' @export
#'
#' @examples
#' 
#' 
createCustomColDefList <- function(rawColNames, niceColNames, tooltipText, case = NULL) {
  withTooltip <- function(value, tooltip, ...) {
    shiny::div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
               tippy::tippy(value, tooltip, ...))
  }
  
  result <- vector("list", length(rawColNames))
  
  if (!is.null(case)) {
    if (case == "snakeCaseToCamelCase") {
      rawColNames <- SqlRender::snakeCaseToCamelCase(rawColNames)
    } else if (case == "camelCaseToSnakeCase") {
      rawColNames <- SqlRender::camelCaseToSnakeCase(rawColNames)
    }
  }
  
  for (i in 1:length(rawColNames)) {
    result[[i]] <- reactable::colDef(
      header = withTooltip(niceColNames[[i]], tooltipText[[i]])
    )
  }
  
  names(result) <- rawColNames
  
  return(result)
}



# examples
# createCustomColDefList(rawColNames = c("firstName", "lastName"),
#                        niceColNames = c("First Name", "Last Name"),
#                        tooltipText = c("The person's first name", "The person's last name"))
# rawColNames <- c("col1", "col2", "col3")
# niceColNames <- c("Column 1", "Column 2", "Column 3")
# tooltipText <- c("Tooltip 1", "Tooltip 2", "Tooltip 3")# Sample data
# 
# Call the function
# colDefs <- createCustomColDefList(rawColNames, niceColNames, tooltipText)

# use the below as a guide to save named colDef list as JSON then read it back!
# test <- ParallelLogger::saveSettingsToJson(colDefs, "./inst/components-columnInformation/test.json")
#loadTest <- ParallelLogger::loadSettingsFromJson("./inst/components-columnInformation/test.json")



ParallelLogger::saveSettingsToJson(phevalColList, "./inst/components-columnInformation/phevaluator-colDefs.json")