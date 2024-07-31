estimationCmFullResultViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(  
    # add selected settings
    
    inputSelectionDfViewer(
      id = ns("input-selection-df"), 
      title = 'Result Selected: '
    ),
  
  shiny::tabsetPanel(
    id = ns("fullTabsetPanel"), 
    type = 'pills',
    shiny::tabPanel(
      title = "Power",
      cohortMethodPowerViewer(ns("power"))
    ),
    shiny::tabPanel(
      title = "Attrition",
      cohortMethodAttritionViewer(ns("attrition"))
    ),
    shiny::tabPanel(
      title = "Population characteristics",
      cohortMethodPopulationCharacteristicsViewer(ns("popCharacteristics"))
    ),
    shiny::tabPanel(
      title = "Propensity model",
      cohortMethodPropensityModelViewer(ns("propensityModel"))
    ),
    shiny::tabPanel(
      title = "Propensity scores",
      cohortMethodPropensityScoreDistViewer(ns("propensityScoreDist"))
    ),
    shiny::tabPanel(
      title = "Covariate balance",
      cohortMethodCovariateBalanceViewer(ns("covariateBalance"))
    ),
    shiny::tabPanel(
      title = "Systematic error",
      cohortMethodSystematicErrorViewer(ns("systematicError"))
    ),
    shiny::tabPanel(
      title = "Kaplan-Meier",
      cohortMethodKaplanMeierViewer(ns("kaplanMeier"))
    )
  )
  )
  
}

estimationCmFullResultServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings,
    selectedRow,
    actionCount
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # reset the tab when a new result is selected
      shiny::observeEvent(actionCount(), {
        shiny::updateTabsetPanel(session, "fullTabsetPanel", selected = "Power")
      })
      
      modifiedRow <- shiny::reactive({
        selectedRow() %>%
          dplyr::select(
            "target",
            "comparator",
            "outcome",
            "description",
            "cdmSourceAbbreviation"
          ) %>%
          dplyr::rename(
            Target = "target",
            Comparator = "comparator",
            Outcome = "outcome",
            Analysis = "description",
            Database = "cdmSourceAbbreviation"
          )
      })
      
      inputSelectionDfServer(
        id = "input-selection-df", 
        dataFrameRow = modifiedRow,
        ncol = 2
      )
      
      shiny::observeEvent(selectedRow(),{
        if(!is.null(selectedRow()$unblind)){
          if (selectedRow()$unblind == 1) {
            shiny::showTab("fullTabsetPanel", "Kaplan-Meier", session = session)
          } else{
            shiny::hideTab("fullTabsetPanel", "Kaplan-Meier", session = session)
          }
        }
      })
      
      # selected row: : - reactive list with: psStrategy
      
      cohortMethodPowerServer(
        id = "power",
        selectedRow = selectedRow,
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      cohortMethodAttritionServer(
        id = "attrition",
        selectedRow = selectedRow,
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      cohortMethodPopulationCharacteristicsServer(
        id = "popCharacteristics",
        selectedRow = selectedRow,
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      cohortMethodPropensityModelServer(
        id = "propensityModel",
        selectedRow = selectedRow,
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      cohortMethodPropensityScoreDistServer(
        id = "propensityScoreDist",
        selectedRow = selectedRow,
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      cohortMethodCovariateBalanceServer(
        id = "covariateBalance",
        selectedRow = selectedRow,
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      cohortMethodSystematicErrorServer(
        id = "systematicError",
        selectedRow = selectedRow,
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      cohortMethodKaplanMeierServer(
        id = "kaplanMeier",
        selectedRow = selectedRow,
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
    }
  )
}
