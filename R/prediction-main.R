# @file prediction-main.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' The location of the prediction module helper file
#'
#' @details
#' Returns the location of the prediction helper file
#' 
#' @return
#' string location of the prediction helper file
#'
#' @export
predictionHelperFile <- function(){
  fileLoc <- system.file('prediction-www', "prediction.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The module viewer for exploring PatientLevelPrediction
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the PatientLevelPrediction viewer module
#'
#' @export
predictionViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    status = 'info', width = 12,
    title =  shiny::span( shiny::icon("chart-line"), "Prediction Viewer"),
    solidHeader = TRUE,
    
    shiny::tabsetPanel(
      type = 'hidden',#'pills',
      id = ns('allView'),
      
      shiny::tabPanel(
        "Model Designs Summary",  
        
        shinydashboard::box(
          collapsible = TRUE,
          collapsed = TRUE,
          title = "Model Designs Summary",
          width = "100%",
          shiny::htmlTemplate(system.file("prediction-www", "help-designSummary.html", package = utils::packageName()))
        ),
        
        predictionDesignSummaryViewer(ns('designSummaryTab'))
      ),
      
      shiny::tabPanel(
        "Models Summary",  
        shiny::actionButton(
          inputId = ns("backToDesignSummary"), 
          label = "Back To Design Summary",
          shiny::icon("arrow-left"), 
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ),
        predictionModelSummaryViewer(ns('modelSummaryTab'))
      ),
      
      shiny::tabPanel(
        "Explore Selected Model",
        
        shiny::actionButton(
          inputId = ns("backToModelSummary"), 
          label = "Back To Models Summary",
          shiny::icon("arrow-left"), 
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ),
        
        shinydashboard::box(
          collapsible = TRUE,
          collapsed = TRUE,
          title = "Full Result Explorer",
          width = "100%",
          shiny::htmlTemplate(system.file("prediction-www", "help-fullResults.html", package = utils::packageName()))
        ),
        
        shinydashboard::box(
          status = "warning",
          width = "100%",
          shiny::uiOutput(outputId = ns("resultSelectText"))
        ),
        
        shiny::tabsetPanel(
          type = 'pills',
          id = ns('singleView'),
          shiny::tabPanel(
            "Design Settings",
            predictionSettingsViewer(ns('settings'))
          ),
          
          shiny::tabPanel(
            "Model",
            predictionCovariateSummaryViewer(ns('covariateSummary'))
          ),
          
          shiny::tabPanel(
            "Threshold Dependant", 
            predictionCutoffViewer(ns('cutoff'))
          ), 
          
          shiny::tabPanel(
            "Discrimination",  
            predictionDiscriminationViewer(ns('discrimination'))
          ),
          
          shiny::tabPanel(
            "Calibration", 
            predictionCalibrationViewer(ns('calibration'))
          ),
          
          shiny::tabPanel(
            "Net Benefit", 
            predictionNbViewer(ns('netBenefit'))
          ),
          
          
          shiny::tabPanel(
            "Validation",
            predictionValidationViewer(ns('validation'))
          )
          
          
        )
      )
      
    )
    
  ) # end box
  
}

#' The module server for exploring PatientLevelPrediction
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
predictionServer <- function(
  id, 
  connectionHandler,
  resultDatabaseSettings = list(port = 1)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      #   VIEW SETTINGS
      # =============================

      # when going to the all model design hide tabs
      shiny::observeEvent(input$allView, {
        
        
        if(!is.null(input$allView)){
          tempView <- input$allView

          if(tempView != 'Explore Selected Model'){
            shiny::updateTabsetPanel(
              session = session,
              inputId = 'singleView',
              selected = 'Design Settings'
            )
            
            # 
          }
        }
      }
      
      )
      
      # go back button 
      shiny::observeEvent(input$backToModelSummary, {
        shiny::updateTabsetPanel(
          session = session,
          inputId = 'allView',
          selected = 'Models Summary'
        )

            })
      
      shiny::observeEvent(input$backToDesignSummary, {
        shiny::updateTabsetPanel(
          session = session,
          inputId = 'allView',
          selected = 'Model Designs Summary'
        )
      })
      
      # keep a reactive variable tracking the active tab
      singleViewValue <- shiny::reactive({
        input$singleView
      })
      
      
      # =============================
      #   MODEL DESIGNS
      # =============================
      
      # the first server is the design server
      # this shows the number of developed models
      # per prediction model design and lets the user
      # select a model design id to explore
      modelDesignId <- shiny::reactiveVal()
      designSummary <- predictionDesignSummaryServer(
        id = 'designSummaryTab',
        connectionHandler = connectionHandler, 
        mySchema = resultDatabaseSettings$schema, 
        myTableAppend = resultDatabaseSettings$tablePrefix
      )
      
      
      # change to model summary tab when 
      # a model design id is select that shows 
      # a summary table with all the models 
      # developed using the chosen model design
      shiny::observeEvent(designSummary$modelDesignId(), {
        modelDesignId(designSummary$modelDesignId())
        if(!is.null(designSummary$modelDesignId())){
          #shiny::showTab(inputId = "allView", session = session, target = "Models Summary")
          shiny::updateTabsetPanel(session, "allView", selected = "Models Summary")
          #shiny::hideTab(inputId = "allView", session = session, target = "Explore Selected Model")
        }
      })
      
      
      # =============================
      #   MODEL SUMMARY
      # =============================
      
      # this server lets the user select a model
      # of the chosen model design to explore in detail
      # this results a performance id and 
      # development database id
      performanceId <- shiny::reactiveVal()
      developmentDatabaseId <- shiny::reactiveVal()
      performance <- predictionModelSummaryServer(
          id = 'modelSummaryTab', 
          connectionHandler = connectionHandler,  
          mySchema = resultDatabaseSettings$schema, 
          myTableAppend = resultDatabaseSettings$tablePrefix,
          modelDesignId = modelDesignId,
          databaseTableAppend = ifelse(
            !is.null(resultDatabaseSettings$databaseTablePrefix), 
            resultDatabaseSettings$databaseTablePrefix,
            resultDatabaseSettings$tablePrefix
          )
        )

      
      # change to single model explore tab when 
      # a performance id is selected that shows 
      # the internal valdiation and external
      # validation details of the selected model
      shiny::observeEvent(performance$performanceId(), {
        performanceId(performance$performanceId())
        developmentDatabaseId(performance$developmentDatabaseId())
        if(!is.null(performance$performanceId())){
          #shiny::showTab(inputId = "allView", session = session, target = "Explore Selected Model")
          shiny::updateTabsetPanel(session, "allView", selected = "Explore Selected Model")
          #shiny::hideTab(inputId = "allView", session = session, target = "Models Summary")
        }
        
        # hide validation tab if non internal val
        if(performance$modelDevelopment() == 1){
          shiny::showTab(inputId = "singleView", session = session, target = "Validation")
        } else{
          shiny::hideTab(inputId = "singleView", session = session, target = "Validation")
        }
        
      })
      
      
      # =============================
      #   DIAGNOSTICS
      # =============================
      # diagnostic viewer - show model diagnostic results
      shiny::observeEvent(designSummary$diagnosticId(), {
        shiny::showModal(shiny::modalDialog(
          title = "Diagnostic",
          predictionDiagnosticsViewer(session$ns('diagnostics'))
        ))
      })
      predictionDiagnosticsServer(
        id = 'diagnostics', 
        modelDesignId = designSummary$diagnosticId, 
        mySchema = resultDatabaseSettings$schema, 
        connectionHandler = connectionHandler, 
        myTableAppend = resultDatabaseSettings$tablePrefix, 
        databaseTableAppend = ifelse(
          !is.null(resultDatabaseSettings$databaseTablePrefix), 
          resultDatabaseSettings$databaseTablePrefix,
          resultDatabaseSettings$tablePrefix
        )
      )
      
      # =============================
      #   PROTOCOL
      # =============================
      # protocol viewer - show protocol 
      shiny::observeEvent(designSummary$reportId(), {
        
        if(!is.null(designSummary$reportId())){
          
          #check protocol generator packages installed for this
          # could make this interactive in shiny
          if(!is_installed("CirceR")){
            shiny::showNotification("Need to install CirceR for this to work: remotes::install_github('OHDSI/CirceR')")
          }
          if(!is_installed("kableExtra")){
            shiny::showNotification("Need to install kableExtra for this to work: install.packages('kableExtra')")
          }
          if(!is_installed("knitr")){
            shiny::showNotification("Need to install knitr for this to work: install.packages('kableExtra')")
          }
          
          #protocolOutputLoc <- tempdir()
          protocolOutputLoc <- getwd()
          
          if(file.exists(file.path(protocolOutputLoc, 'main.html'))){
            file.remove(file.path(protocolOutputLoc, 'main.html'))
          }
          tryCatch(
            {createPredictionProtocol( # add database_table_append and cohort_table_append
              connectionHandler = connectionHandler, 
              mySchema = resultDatabaseSettings$schema, 
              myTableAppend = resultDatabaseSettings$tablePrefix,
              databaseTableAppend = ifelse(
                !is.null(resultDatabaseSettings$databaseTablePrefix), 
                resultDatabaseSettings$databaseTablePrefix,
                resultDatabaseSettings$tablePrefix
              ),
              cohortTableAppend = ifelse(
                !is.null(resultDatabaseSettings$cohortTablePrefix), 
                resultDatabaseSettings$cohortTablePrefix,
                resultDatabaseSettings$tablePrefix
              ),
              modelDesignId = designSummary$reportId(),
              output = protocolOutputLoc,
              intermediatesDir = file.path(protocolOutputLoc, 'plp-prot')
            )
            }, error = function(e){
              shiny::showNotification(
                paste('error generating protocol:',e)
              )
            }
          )
             
          if(file.exists(file.path(protocolOutputLoc, 'main.html'))){
            # display the generated html report
            shiny::showModal(shiny::modalDialog(
              title = "Report",
              shiny::div(
                shiny::textInput(
                  inputId = session$ns('plpProtocolDownload'), 
                  label = 'Download Protocol Location:', 
                  placeholder = '/Users/jreps/Documents'
                ),
                shiny::actionButton(
                  inputId = session$ns('downloadButton'), 
                  label = 'Download'
                ),
                shiny::includeHTML(file.path(protocolOutputLoc, 'main.html'))
              ), 
              size = "l",
              easyClose = T
            ))
          }
        }
        
      })
      
      # add code to save protocol to file based on input$downloadButton
      shiny::observeEvent(input$downloadButton, {
        
        if(!dir.exists(input$plpProtocolDownload)){
          dir.create(input$plpProtocolDownload, recursive = T)
        }
        
        rmarkdown::render(
          input = system.file(
            'prediction-document', 
            "export-main.Rmd", 
            package = "OhdsiShinyModules"
          ),  
          intermediates_dir = file.path(tempdir(), 'plp-prot'),
          output_dir = file.path(input$plpProtocolDownload, paste0('plp_report',designSummary$reportId())), 
          params = list(
            connectionHandler = connectionHandler, 
            resultSchema = resultDatabaseSettings$schema, 
            myTableAppend = resultDatabaseSettings$tablePrefix,
            databaseTableAppend = ifelse(
              !is.null(resultDatabaseSettings$databaseTablePrefix), 
              resultDatabaseSettings$databaseTablePrefix,
              resultDatabaseSettings$tablePrefix
            ),
            cohortTableAppend = ifelse(
              !is.null(resultDatabaseSettings$cohortTablePrefix), 
              resultDatabaseSettings$cohortTablePrefix,
              resultDatabaseSettings$tablePrefix
            ),
            modelDesignIds = designSummary$reportId()
          )
        )
      }
      )
      
      # ===========================================
      #  Single Result Exploring Modules
      # ===========================================
      
      output$resultSelectText <- shiny::renderUI(
          getResultSelection(
            connectionHandler = connectionHandler, 
            mySchema = resultDatabaseSettings$schema, 
            myTableAppend = resultDatabaseSettings$tablePrefix,
            modelDesignId = modelDesignId,
            performanceId = performanceId,
            cohortTableAppend = ifelse(
              !is.null(resultDatabaseSettings$cohortTablePrefix), 
              resultDatabaseSettings$cohortTablePrefix,
              resultDatabaseSettings$tablePrefix
            ),
            databaseTableAppend = ifelse(
              !is.null(resultDatabaseSettings$databaseTablePrefix), 
              resultDatabaseSettings$databaseTablePrefix,
              resultDatabaseSettings$tablePrefix
            )
          )
      )
      
      predictionCovariateSummaryServer(
        id = 'covariateSummary',
        modelDesignId = modelDesignId, # reactive
        developmentDatabaseId = developmentDatabaseId, # reactive
        performanceId = performanceId, # reactive
        connectionHandler = connectionHandler,
        inputSingleView = singleViewValue,
        mySchema = resultDatabaseSettings$schema, 
        myTableAppend = resultDatabaseSettings$tablePrefix
      ) 
      
      predictionSettingsServer(
        id = 'settings', 
        modelDesignId = modelDesignId, # reactive
        developmentDatabaseId = developmentDatabaseId, # reactive
        performanceId = performanceId, # reactive
        mySchema = resultDatabaseSettings$schema, 
        connectionHandler = connectionHandler, 
        inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$tablePrefix, 
        cohortTableAppend = ifelse(
          !is.null(resultDatabaseSettings$cohortTablePrefix), 
          resultDatabaseSettings$cohortTablePrefix,
          resultDatabaseSettings$tablePrefix
        ),
        databaseTableAppend = ifelse(
          !is.null(resultDatabaseSettings$databaseTablePrefix), 
          resultDatabaseSettings$databaseTablePrefix,
          resultDatabaseSettings$tablePrefix
        )
      )
      
      predictionCutoffServer(
        id = 'cutoff', 
        performanceId = performanceId, 
        mySchema = resultDatabaseSettings$schema, 
        connectionHandler = connectionHandler, 
        inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$tablePrefix
      )
      
      predictionDiscriminationServer(
        id = 'discrimination', 
        performanceId = performanceId, 
        mySchema = resultDatabaseSettings$schema, 
        connectionHandler = connectionHandler, 
        inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$tablePrefix
      )
      
      predictionCalibrationServer(
        id = 'calibration', 
        performanceId = performanceId, 
        mySchema = resultDatabaseSettings$schema, 
        connectionHandler = connectionHandler, 
        inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$tablePrefix
      ) 
      
      predictionNbServer(
        id = 'netBenefit', 
        performanceId = performanceId, 
        mySchema = resultDatabaseSettings$schema, 
        connectionHandler = connectionHandler, 
        inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$tablePrefix
      ) 
      
      predictionValidationServer(
        id = 'validation', 
        modelDesignId = modelDesignId, # reactive
        developmentDatabaseId = developmentDatabaseId, # reactive
        performanceId = performanceId, # reactive
        connectionHandler = connectionHandler, 
        inputSingleView = singleViewValue,
        mySchema = resultDatabaseSettings$schema,
        myTableAppend = resultDatabaseSettings$tablePrefix, 
        databaseTableAppend = ifelse(
          !is.null(resultDatabaseSettings$databaseTablePrefix), 
          resultDatabaseSettings$databaseTablePrefix,
          resultDatabaseSettings$tablePrefix
        )
      ) 
      
    }
  )
}



getResultSelection <- function(
  connectionHandler, 
  mySchema, 
  myTableAppend,
  modelDesignId,
  performanceId,
  cohortTableAppend,
  databaseTableAppend
){
  if(!is.null(modelDesignId()) & !is.null(performanceId())){
  
  modelType <- connectionHandler$queryDb(
    'select distinct model_type from @my_schema.@my_table_appendmodels where model_design_id = @model_design_id;',
    my_schema = mySchema,
    my_table_append = myTableAppend,
    model_design_id = modelDesignId()
  )
  
  print(modelType)
  
  developmentDb = connectionHandler$queryDb(
    'select distinct d.cdm_source_abbreviation from 
    @my_schema.@database_table_appenddatabase_meta_data d
    inner join
    @my_schema.@my_table_appenddatabase_details dd
    on dd.database_meta_data_id = d.database_id
    inner join
    @my_schema.@my_table_appendperformances p 
    on dd.database_id = p.development_database_id
    where p.performance_id = @performance_id;',
    my_schema = mySchema,
    my_table_append = myTableAppend,
    performance_id = performanceId(),
    database_table_append = databaseTableAppend
  )
  
  print(developmentDb)
  
  validationDb = connectionHandler$queryDb(
    'select distinct d.cdm_source_abbreviation from 
    @my_schema.@database_table_appenddatabase_meta_data d
    inner join
    @my_schema.@my_table_appenddatabase_details dd
    on dd.database_meta_data_id = d.database_id
    inner join
    @my_schema.@my_table_appendperformances p 
    on dd.database_id = p.validation_database_id
    where p.performance_id = @performance_id;',
    my_schema = mySchema,
    my_table_append = myTableAppend,
    performance_id = performanceId(),
    database_table_append = databaseTableAppend
  )
  print(validationDb)
  
  target <- connectionHandler$queryDb(
    'select distinct c.cohort_name from 
    @my_schema.@my_table_appendcohorts c
    inner join
    @my_schema.@my_table_appendperformances p 
    on c.cohort_id = p.target_id
    where p.performance_id = @performance_id;',
    my_schema = mySchema,
    my_table_append = myTableAppend,
    performance_id = performanceId()
  )
  print(target)
  outcome <- connectionHandler$queryDb(
    'select distinct c.cohort_name from 
    @my_schema.@my_table_appendcohorts c
    inner join
    @my_schema.@my_table_appendperformances p 
    on c.cohort_id = p.outcome_id
    where p.performance_id = @performance_id;',
    my_schema = mySchema,
    my_table_append = myTableAppend,
    performance_id = performanceId()
  )
  print(outcome)
  
  return(
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::tags$b("modelDesignId :"),
          modelDesignId()
        ),
        shiny::column(
          width = 4,
          shiny::tags$b("modelType :"),
          modelType
        ),
        shiny::column(
          width = 4,
          shiny::tags$b("Target :"),
          target
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::tags$b("developmentDb :"),
          developmentDb
        ),
        shiny::column(
          width = 4,
          shiny::tags$b("validationDb :"),
          validationDb
        ),
        shiny::column(
          width = 4,
          shiny::tags$b("outcome :"),
          outcome
        )
      )
    )
  )
  }
}
