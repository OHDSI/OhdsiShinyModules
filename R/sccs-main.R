#' SCCS shiny module UI code
#' @description
#' Load the ui for the sccs module
#' @param id        id for module
#' @export
sccsView <- function(id = "sccs-module") {
  ns <- shiny::NS(id)
  tags <- shiny::tags

  shinydashboard::box(
    status = 'info',
    width = 12,
    title = shiny::span( shiny::icon("people-arrows"), 'Self Controlled Case Series'),
    solidHeader = TRUE,

    infoHelperViewer(
      id = "helper",
      helpLocation= system.file("sccs-www", "sccs.html", package = utils::packageName())
    ),

    inputSelectionViewer(ns("input-selection-sccs")),

    shiny::conditionalPanel(
      condition = 'input.generate != 0',
      ns = shiny::NS(ns("input-selection-sccs")),

      shiny::tabsetPanel(
        type = 'pills',
        id = ns("mainTabsetPanel"),

        shiny::tabPanel(
          title = "Diagnostics",
          sccsDiagnosticsSummaryViewer(ns("sccsDiganostics"))
        ),
        shiny::tabPanel(
          title = 'Results',
            sccsResultsViewer(ns("sccsResults")),
            )
          )

        ) # end condition
  )
}

#' Gets input selection box for use with SCCS exposure indication selection
#' @noRd
.getSccsExposureIndicationSelection <- function(connectionHandler,
                                                resultDatabaseSettings) {
  migrations <- getMigrations(connectionHandler = connectionHandler,
                              resultDatabaseSettings = resultDatabaseSettings,
                              tablePrefix = resultDatabaseSettings$sccsTablePrefix)

  # Migration_2-v5_1_0.sql
  useNestingIndications <- migrations %>% migrationPresent(2)

  if (useNestingIndications) {
    # Requires migration in 5.1.0 of cohort generator
    expIndicationsTbl <- sccsGetExposureIndications(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings
    )

  } else {
    # Backwards compatability
    expIndicationsTbl <- sccsGetExposures(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings
    )
  }

 expIndicationsTbl <- expIndicationsTbl %>%
      dplyr::mutate(exposureIndicationId = paste(.data$exposureId,
                                                 .data$indicationId))

  exposureChoices <- expIndicationsTbl %>%
      shinyWidgets::prepare_choices(label = .data$indicationName,
                                    value = .data$exposureIndicationId,
                                    group_by = .data$exposureName,
                                    alias = .data$exposureName)

  namesCallback <- function(inputSelected) {
      if (is.null(inputSelected))
        return("")

      vars <- strsplit(inputSelected, " ")[[1]]

      res <- expIndicationsTbl %>%
        dplyr::filter(.data$exposureId == vars[[1]],
                      .data$indicationId == vars[[2]]) %>%
        dplyr::select("exposureName",
                      "indicationName")

      paste(res$exposureName, "\n\t-", res$indicationName)
    }

  return(
    createInputSetting(
      rowNumber = 1,
      columnWidth = 12,
      varName = 'exposure',
      uiFunction = 'shinyWidgets::virtualSelectInput',
      updateFunction = "shinyWidgets::updateVirtualSelectInput",
      uiInputs = list(
        label = 'Target/Indication: ',
        choices = exposureChoices,
        multiple = FALSE,
        search = TRUE,
        searchGroup = TRUE,
        hasOptionDescription = TRUE,
        keepAlwaysOpen = FALSE
      ),
      namesCallback = namesCallback
    )
  )
}


#' The module server for exploring SCCS
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list containing the prediction result schema and connection details
#'
#' @return
#' The server for the PatientLevelPrediction module
#'
#' @export
sccsServer <- function(
  id,
  connectionHandler,
  resultDatabaseSettings = list(port = 1)
) {
  ns <- shiny::NS(id)

  # create functions to result list
  outcomes <- sccsGetOutcomes(
    connectionHandler = connectionHandler,
    resultDatabaseSettings = resultDatabaseSettings
    )

  analyses <- sccsGetAnalyses(
    connectionHandler = connectionHandler,
    resultDatabaseSettings = resultDatabaseSettings
  )

  shiny::moduleServer(id, function(input, output, session) {

   inputSettings <- list(
     .getSccsExposureIndicationSelection(connectionHandler = connectionHandler,
                                         resultDatabaseSettings = resultDatabaseSettings),
     createInputSetting(
       rowNumber = 2,
       columnWidth = 6,
       varName = 'outcome',
       uiFunction = 'shinyWidgets::virtualSelectInput',
       updateFunction = "shinyWidgets::updateVirtualSelectInput",
       uiInputs = list(
         label = 'Outcome: ',
         choices = outcomes,
         selected = outcomes[1],
         multiple = F,
         search = TRUE
       )
     ),
     createInputSetting(
       rowNumber = 2,
       columnWidth = 6,
       varName = 'analysis',
       uiFunction = 'shinyWidgets::virtualSelectInput',
       updateFunction = "shinyWidgets::updateVirtualSelectInput",
       uiInputs = list(
         label = 'Analysis: ',
         choices = analyses,
         selected = analyses,
         multiple = T
       )
     )
   )

    inputSelected <- inputSelectionServer(
      id = "input-selection-sccs",
      inputSettingList = inputSettings
    )

    sccsDiagnosticsSummaryServer(
      id = "sccsDiganostics",
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      inputSelected = inputSelected
    )

    sccsResultsServer(
      id = "sccsResults",
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      inputSelected = inputSelected
    )
  })
}

#' The location of the description module helper file
#'
#' @details
#' Returns the location of the description helper file
#'
#' @return
#' string location of the description helper file
#'
#' @export
sccsHelperFile <- function() {
  fileLoc <- system.file('sccs-www', "sccs.html", package = utils::packageName())
  return(fileLoc)
}
