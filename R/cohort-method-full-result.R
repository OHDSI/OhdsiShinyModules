cohortMethodFullResultViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    status = 'info', 
    width = '100%',
    title = shiny::span('Result Explorer'),
    solidHeader = TRUE,
    
    # add selected settings
    shinydashboard::box(
      status = 'warning', 
      width = "100%",
      title = 'Selected: ', 
      collapsible = T,
      shiny::uiOutput(ns('selection'))
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

cohortMethodFullResultServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings,
    selectedRow
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$selection <- shiny::renderUI({
        otext <- list()
          otext[[1]] <- shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::tags$b('Target: '),
                selectedRow()$target
              ),
              shiny::column(
                width = 6,
                shiny::tags$b('Comparator: '),
                selectedRow()$comparator
              )
            )
          otext[[2]] <- shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::tags$b('Outcome: '),
              selectedRow()$outcome
            ),
            shiny::column(
              width = 6,
              shiny::tags$b('Analysis: '),
              selectedRow()$description
            )
            )
          otext[[3]] <- shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::tags$b('Database: '),
              selectedRow()$cdmSourceAbbreviation
            ),
            shiny::column(
              width = 6,
              shiny::tags$b('')
            )
          )
          shiny::div(otext)
        })
      
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