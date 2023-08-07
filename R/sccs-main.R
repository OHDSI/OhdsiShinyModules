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
    
    shinydashboard::box(
      collapsible = TRUE,
      collapsed = TRUE,
      title = "Info",
      width = "100%"#,
      #shiny::htmlTemplate(system.file("cohort-diagnostics-www", "cohortCounts.html", package = utils::packageName()))
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
  exposures <- sccsGetExposures(
    connectionHandler = connectionHandler, 
    resultDatabaseSettings = resultDatabaseSettings
  )
  analyses <- sccsGetAnalyses(
    connectionHandler = connectionHandler, 
    resultDatabaseSettings = resultDatabaseSettings
  )
    

  shiny::moduleServer(id, function(input, output, session) {
    
    inputSelected <- inputSelectionServer(
      id = "input-selection-sccs", 
      inputSettingList = list(
        createInputSetting(
          rowNumber = 1,                           
          columnWidth = 6,
          varName = 'exposure',
          uiFunction = 'shinyWidgets::pickerInput',
          uiInputs = list(
            label = 'Target: ',
            choices = exposures,
            selected = exposures[1],
            multiple = F,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              size = 10,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Type here to search",
              virtualScroll = 50
            )
          )
        ),
        createInputSetting(
          rowNumber = 1,                           
          columnWidth = 6,
          varName = 'outcome',
          uiFunction = 'shinyWidgets::pickerInput',
          uiInputs = list(
            label = 'Outcome: ',
            choices = outcomes,
            selected = outcomes[1],
            multiple = F,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              size = 10,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Type here to search",
              virtualScroll = 50
            )
          )
        ),
        createInputSetting(
          rowNumber = 2,                           
          columnWidth = 6,
          varName = 'analysis',
          uiFunction = 'shinyWidgets::pickerInput',
          uiInputs = list(
            label = 'Analysis: ',
            choices = analyses,
            selected = analyses,
            multiple = T,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              size = 10,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Type here to search",
              virtualScroll = 50
            )
          )
        )
      )
    )
    
    
    sccsDiagnosticsSummaryServer(
      id = "sccsDiganostics",
      connectionHandler = connectionHandler,
      resultDatabaseSettings,
      inputSelected = inputSelected
    )
    
    sccsResultsServer(
      id = "sccsResults",
      connectionHandler = connectionHandler,
      resultDatabaseSettings,
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
