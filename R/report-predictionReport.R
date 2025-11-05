predictionReportViewer <- function(
    id = 'predictionReport'
){
  
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    title = 'Prediction report inputs: ', 
    solidHeader = TRUE, 
    width = 12,
    
    tableSelectionViewer(id = ns('prediction-report')),
    
    shiny::conditionalPanel(
      condition = "output.showGenerate != 0", 
      ns = ns,
      shiny::uiOutput(ns("generateSelection"))
    )
  )
  
}

predictionReportServer <- function(
    id = 'predictionReport',
    connectionHandler = NULL,
    resultDatabaseSettings = NULL,
    server = Sys.getenv("RESULTS_SERVER"), 
    username = Sys.getenv("RESULTS_USER"), 
    password = Sys.getenv("RESULTS_PASSWORD"), 
    dbms = Sys.getenv("RESULTS_DBMS")
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      modelDesignRow <- shiny::reactiveVal(0)
      output$showGenerate <- shiny::reactive(0)
      shiny::outputOptions(output, "showGenerate", suspendWhenHidden = FALSE)
      showDownload <- shiny::reactiveVal(FALSE)
      fileDir <- shiny::reactiveVal('')
      
      modelDesigns <- shiny::reactive(
        OhdsiReportGenerator::getPredictionModelDesigns(
          connectionHandler = connectionHandler, 
          schema = resultDatabaseSettings$schema, 
          plpTablePrefix = resultDatabaseSettings$plpTablePrefix, 
          cgTablePrefix = resultDatabaseSettings$cgTablePrefix
          )
        )
      
     # add code here
      tableSelectionServer(
        id = 'prediction-report', 
        table = modelDesigns, 
        selectedRowId = modelDesignRow,
        helpText = 'Click button to select a model design for the report',
        inputColumns = list(
          modelDesignId = reactable::colDef(show = FALSE),
          modelType = reactable::colDef(show = TRUE),
          developmentTargetId = reactable::colDef(show = FALSE),
          developmentTargetName = reactable::colDef(
            show = TRUE, 
            name = 'Target', 
            defaultSortOrder = 'desc'
          ),
          developmentTargetJson = reactable::colDef(show = FALSE),
          developmentOutcomeId = reactable::colDef(show = FALSE),
          developmentOutcomeName = reactable::colDef(
            show = TRUE, 
            name = 'Outcome', 
            defaultSortOrder = 'desc'
          ),
          timeAtRisk = reactable::colDef(show = TRUE),
          developmentOutcomeJson = reactable::colDef(show = FALSE),
          covariateSettingsJson = reactable::colDef(show = FALSE),
          populationSettingsJson = reactable::colDef(show = FALSE),
          tidyCovariatesSettingsJson = reactable::colDef(show = FALSE),
          plpDataSettingsJson = reactable::colDef(show = FALSE),
          featureEngineeringSettingsJson = reactable::colDef(show = FALSE),
          splitSettingsJson = reactable::colDef(show = FALSE),
          sampleSettingsJson = reactable::colDef(show = FALSE),
          modelSettingsJson = reactable::colDef(show = FALSE)
        ),
        elementId = 'prediction-report-table',
        selectButtonText = 'Select Model Design',
        )
      
      
      shiny::observeEvent(modelDesignRow(), {
        
        showDownload(FALSE)
        
        if(modelDesignRow() != 0){
          output$showGenerate <- shiny::reactive(1)
          fileDirTemp <- paste0('prediction-report-',
                                 modelDesigns()$modelDesignId[modelDesignRow()]
                                 )
          fileDir(fileDirTemp)
        } else{
          output$showGenerate <- shiny::reactive(0)
        }
        
      }
      )
      
      
      # GENERATE 
      output$generateSelection <- shiny::renderUI({
        
          shinydashboard::box(
            title = 'Generate Report', 
            width = 12,
    
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::helpText('First generate the report (this can take 1+ minutes) and then click the download button that appears once the report is ready to download.')
            )
          ),
          
          shiny::fluidRow(
            shiny::column(
              width = 5,
              shiny::actionButton(
                inputId = session$ns("generate"), 
                label = "Generate", 
                shiny::icon('circle-plus')
              )
            ),
            shiny::column(
              width = 5,
              shiny::uiOutput(session$ns('downloadButton'))
            )
          )
          
        ) # end box
      })
      
      output$downloadButton <- shiny::renderUI(
        expr = if(showDownload()) {
          shiny::downloadButton(
            outputId = session$ns("download"), 
            label = "Download"
          )
        } else {
          NULL
        })
      
      
      
      # Downloadable presentation ----
      shiny::observeEvent(
        eventExpr = input$generate, 
        handlerExpr = {
              result <- tryCatch({generatePredictionReport(
                connectionHandler = connectionHandler,
                resultDatabaseSettings = resultDatabaseSettings,
                modelDesignId = modelDesigns()$modelDesignId[modelDesignRow()],
                fileDir = fileDir()
              )}, error = function(e){print(e); return(NULL)})
          if(!is.null(result)){
            showDownload(TRUE)
          } else{
            shiny::showNotification('Error rendering report - please check logs')
            showDownload(FALSE)
          }
        })
      
      output$download <- shiny::downloadHandler(
        filename = function() {
          paste("prediction-report-", Sys.Date(), ".html", sep="")
        }, 
        content = function(file){
          if(file.exists(file.path(tempdir(), fileDir(), 'main.html'))){
            file.copy(
              from = file.path(tempdir(), fileDir(),'main.html'), 
              to = file
            )
          }
        }
      )
    
      
    }
  )
}


generatePredictionReport <- function(
    connectionHandler,
    resultDatabaseSettings,
    modelDesignId,
    fileDir
    ){
  
  shiny::withProgress(
    message = 'Generating prediction report', value = 0, {
      if(file.exists(file.path(tempdir(), fileDir, 'main.html'))){
        file.remove(file.path(tempdir(), fileDir, 'main.html'))
      }
      
      shiny::incProgress(0.2, detail = "Generating report")

      # wrap in a tryCatch??
  OhdsiReportGenerator::createPredictionReport(
    connectionHandler, 
    schema = resultDatabaseSettings$schema,
    plpTablePrefix = resultDatabaseSettings$plpTablePrefix,
    databaseTablePrefix = resultDatabaseSettings$databaseTablePrefix,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    modelDesignId = modelDesignId,
    output = file.path(tempdir(), fileDir),
    intermediatesDir = file.path(tempdir(), fileDir)
)
  
  shiny::incProgress(1, detail = "Done")
  
    })
  
  return(invisible(TRUE))

}
