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
#' @param customColDefOptions A list of lists, where the inner lists are any custom options from
#' reactable::colDef for each column 
#' @family Utils
#' @return A named list of reactable::colDef objects
#' 
#' @export 
createCustomColDefList <- function(rawColNames, niceColNames = NULL,
                                   tooltipText = NULL, case = NULL,
                                   customColDefOptions = NULL) {
  withTooltip <- function(value, tooltip, ...) {
    shiny::div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
               tippy::tippy(value, tooltip, ...))
  }
  
  if (is.null(niceColNames)) {
    niceColNames <- rawColNames
  }
  
  if (is.null(tooltipText)) {
    tooltipText <- rep("", length(rawColNames))
  }
  
  if (!is.null(case)) {
    if (case == "snakeCaseToCamelCase") {
      rawColNames <- SqlRender::snakeCaseToCamelCase(rawColNames)
    } else if (case == "camelCaseToSnakeCase") {
      rawColNames <- SqlRender::camelCaseToSnakeCase(rawColNames)
    }
  }
  
  result <- vector("list", length(rawColNames))
  
  if (is.null(customColDefOptions)) {
    customColDefOptions <- vector("list", length(rawColNames))
    for (i in seq_along(rawColNames)) {
      customColDefOptions[[i]] <- list()
    }
  }
  
  for (i in seq_along(rawColNames)) {
    colDefOptions <- c(
      list(name = rawColNames[[i]], header = withTooltip(niceColNames[[i]], tooltipText[[i]])),
      customColDefOptions[[i]]
    )
    
    result[[i]] <- do.call(reactable::colDef, colDefOptions)
  }

  names(result) <- rawColNames
  
  return(result)
}


# examples
# Define custom column definitions
# customColDefs <- createCustomColDefList(
#   rawColNames = colnames(comb),
#   niceColNames = c("Database Name",
#                    "Covariate Name",
#                    "Mean Target Before Matching", 
#                    "Mean Comparator Before Matching",
#                    "Abs Val StdDiff Before Matching",
#                    "Mean Target After Matching",
#                    "Mean Comparator After Matching", 
#                    "Abs Val StdDiff After Matching"),
#   tooltipText = c("The name of the database",
#                   "The name of the covariate",
#                   "Mean (Proportion) in Target Before Matching",
#                   "Mean (Proportion) in Comparator Before Matching",
#                   "Absolute Value of the Standardized Mean Difference Before Matching",
#                   "Mean (Proportion) in Target After Matching",
#                   "Mean (Proportion) in Comparator Before Matching",
#                   "Absolute Value of the Standardized Mean Difference After Matching"),
#   customColDefOptions = list(
#     list(NULL),  # No aggregation for "Name" column
#     list(NULL),  # Aggregate "Age" column using mean
#     list(NULL),
#     list(NULL),
#     list(NULL),  # No aggregation for "Name" column
#     list(NULL),  # Aggregate "Age" column using mean
#     list(NULL),
#     list(NULL)# No aggregation for "Country" column
#   )
# )

# use the below as a guide to save named colDef list as JSON then read it back!
# test <- ParallelLogger::saveSettingsToJson(colDefs, "./inst/components-columnInformation/test.json")
#loadTest <- ParallelLogger::loadSettingsFromJson("./inst/components-columnInformation/test.json")


#' Make a label for an html button
#'
#' @param label The desired label for hte button
#' @family Utils
#' @return html code to make a button label
#' 
#' @export
makeButtonLabel <- function(label) {
  as.character(htmltools::tags$div(htmltools::tags$button(paste(label))))
}
