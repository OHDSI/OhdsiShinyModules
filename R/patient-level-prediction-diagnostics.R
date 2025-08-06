# @file patient-level-prediction-diagnostics.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of OhdsiShinyModules
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


#' The module viewer for exploring prediction diagnostic results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @family PatientLevelPrediction
#' @return
#' The user interface to the prediction diagnostic module
#'
#' @export
patientLevelPredictionDiagnosticsViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    infoHelperViewer(
      id = "helper",
      helpLocation= system.file("patient-level-prediction-www", "main-diagnosticsSummaryHelp.html", package = utils::packageName())
    ),
    
    inputSelectionDfViewer(
      id = ns("df-output-selection-diag"),
      title = 'Model Design Selected'
    ),
    shinydashboard::box(
      width = "100%",
      shiny::div(
        resultTableViewer(ns('diagnosticSummaryTable')),
        shiny::uiOutput(ns('main'))
      )
    )
  )
  
  
}

#' The module server for exploring prediction diagnostic results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param modelDesignId the unique id for the model design
#' @param connectionHandler the connection to the prediction result database
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#' @family PatientLevelPrediction
#' @return
#' The server to the prediction diagnostic module
#'
#' @export
patientLevelPredictionDiagnosticsServer <- function(
  id,
  modelDesignId, 
  connectionHandler,
  resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      selectedModelDesign <- shiny::reactive(
        getModelDesignInfo(
          connectionHandler = connectionHandler, 
          resultDatabaseSettings = resultDatabaseSettings,
          modelDesignId = modelDesignId
        )
      )
      
      inputSelectionDfServer(
        id = "df-output-selection-diag", 
        dataFrameRow = selectedModelDesign
      )
      
      diagnosticTable <- shiny::reactive({
        getPredictionDiagnostics(
          modelDesignId = modelDesignId(),
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings
        )
      })
      
      colDefsInput <- list(
        '1.1' = reactable::colDef( 
          name = "1.1",
          header = withTooltip(
            "1.1", 
            "Participants: Were appropriate data sources used, e.g. cohort, RCT or nested case-control study data?"
          ),
          cell = reactable::JS("
    function(cellInfo) {
      // Render as an X mark or check mark
      if(cellInfo.value === 'Fail'){return '\u274c Fail'} else if(cellInfo.value === 'Pass'){return '\u2714\ufe0f Pass'} else{return '? Unkown'}
    }
  ")),
        '1.2' = reactable::colDef(
          name =  "1.2", 
          header = withTooltip(
            "1.2", 
            "Participants: Were all inclusions and exclusions of participants appropriate?"
          ),
          cell = reactable::JS("
    function(cellInfo) {
      // Render as an X mark or check mark
      if(cellInfo.value === 'Fail'){return '\u274c Fail'} else if(cellInfo.value === 'Pass'){return '\u2714\ufe0f Pass'} else{return '? Unkown'}
    }
  ")),
        '2.1' = reactable::colDef(
          name = "2.1", 
          header = withTooltip(
            "2.1", 
            "Predictors: Were predictors defined and assessed in a similar way for all participants?"
          ),
          cell = reactable::JS("
    function(cellInfo) {
      // Render as an X mark or check mark
      if(cellInfo.value === 'Fail'){return '\u274c Fail'} else if(cellInfo.value === 'Pass'){return '\u2714\ufe0f Pass'} else{return '? Unkown'}
    }
  ")),   
        '2.2' = reactable::colDef(
          name =  "2.2",
          header = withTooltip(
            "2.2", 
            "Predictors: Were predictor assessments made without knowledge of outcome data?"
          ),
          cell = reactable::JS("
    function(cellInfo) {
      // Render as an X mark or check mark
      if(cellInfo.value === 'Fail'){return '\u274c Fail'} else if(cellInfo.value === 'Pass'){return '\u2714\ufe0f Pass'} else{return '? Unkown'}
    }
  ")),
        '2.3' = reactable::colDef(
          name = "2.3", 
          header = withTooltip(
            "2.3", 
            "Predictors: Are all predictors available at the time the model is intended to be used?"
          ),
          cell = reactable::JS("
    function(cellInfo) {
      // Render as an X mark or check mark
      if(cellInfo.value === 'Fail'){return '\u274c Fail'} else if(cellInfo.value === 'Pass'){return '\u2714\ufe0f Pass'} else{return '? Unkown'}
    }
  ")),
        '3.4' = reactable::colDef(
          name = "3.4", 
          header = withTooltip(
            "3.4", 
            "Outcome: Was the outcome defined and determined in a similar way for all participants?"
          ),
          cell = reactable::JS("
    function(cellInfo) {
      // Render as an X mark or check mark
      if(cellInfo.value === 'Fail'){return '\u274c Fail'} else if(cellInfo.value === 'Pass'){return '\u2714\ufe0f Pass'} else{return '? Unkown'}
    }
  ")),
        '3.6' = reactable::colDef(
          name = "3.6",
          header = withTooltip(
            "3.6", 
            "Outcome: Was the time interval between predictor assessment and outcome determination appropriate?"
          ),
          cell = reactable::JS("
    function(cellInfo) {
      // Render as an X mark or check mark
      if(cellInfo.value === 'Fail'){return '\u274c Fail'} else if(cellInfo.value === 'Pass'){return '\u2714\ufe0f Pass'} else{return '? Unkown'}
    }
  ")),
        '4.1' = reactable::colDef(
          name = "4.1", 
          header = withTooltip(
            "4.1", 
            "Design: Were there a reasonable number of participants with the outcome?"
          ),
          cell = reactable::JS("
    function(cellInfo) {
      // Render as an X mark or check mark
      if(cellInfo.value === 'Fail'){return '\u274c Fail'} else if(cellInfo.value === 'Pass'){return '\u2714\ufe0f Pass'} else{return '? Unkown'}
    }
  "))
      )
      
      modelTableOutputs <- resultTableServer(
        id = "diagnosticSummaryTable",
        df = diagnosticTable,
        colDefsInput = colDefsInput,
        addActions = c('participants','predictors', 'outcomes'),
        elementId = session$ns('diagnosticSummaryTable')
      )

      
          # listen
          # PARTICIPANTS
          #============
      shiny::observeEvent(modelTableOutputs$actionCount(), {

        if(modelTableOutputs$actionType() == 'participants'){
          {
            participants <- getPredictionDiagnosticParticipants(
              diagnosticId = diagnosticTable()$diagnosticId[modelTableOutputs$actionIndex()$index],
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings
            )
            
            output$participants <- reactable::renderReactable({
              reactable::reactable(
                data = participants %>% 
                  dplyr::filter(.data$parameter == ifelse(is.null(input$participantParameters), unique(participants$parameter)[1], input$participantParameters)) %>%
                  dplyr::select(
                    c(
                      "probastId",
                      "paramvalue",
                      "metric", 
                      "value"
                    )
                  ) %>%
                  dplyr::mutate(
                    value = format(.data$value, nsmall = 2, )
                  )  %>%
                  tidyr::pivot_wider(
                    names_from = "paramvalue", #.data$paramvalue, 
                    values_from = "value" #.data$value
                  )
              )
            })
            
            
            shiny::showModal(
              shiny::modalDialog(
                title = "Participant Diagnostics",
                shiny::basicPage(
                  shiny::tags$head(shiny::tags$style(".modal-dialog{ width:95%}")),
                  shiny::div(
                    shiny::selectInput(
                      inputId = session$ns('participantParameters'),
                      label = 'Select Parameter',
                      multiple = F, 
                      choices = unique(participants$parameter)
                    ),
                    reactable::reactableOutput(session$ns('participants'))
                  )
                ),
                size = "l",
                easyClose = T
              ))

          }
 
        }
      })
      
    
          #  PREDICTOR
          #==================
      shiny::observeEvent(modelTableOutputs$actionCount(), {
        if(modelTableOutputs$actionType() == 'predictors'){
              predTable <- getPredictionDiagnosticPredictors(
                diagnosticId = diagnosticTable()$diagnosticId[modelTableOutputs$actionIndex()$index],
                connectionHandler = connectionHandler,
                resultDatabaseSettings = resultDatabaseSettings
              )
              
              output$predictorPlot <- plotly::renderPlotly({
                
                tempPredTable <-  predTable %>% 
                  dplyr::filter(
                    .data$inputType == ifelse(
                      is.null(input$predictorParameters), 
                      unique(predTable$inputType)[1],
                      input$predictorParameters
                    )
                  ) %>%
                  dplyr::select(
                    c(
                    "daysToEvent", 
                    "outcomeAtTime", 
                    "observedAtStartOfDay"
                    )
                  ) %>%
                  dplyr::mutate(
                    survivalT = (.data$observedAtStartOfDay -.data$outcomeAtTime)/.data$observedAtStartOfDay
                  ) %>%
                  dplyr::filter(
                    !is.na(.data$daysToEvent)
                  )
                
                tempPredTable$probSurvT  <- unlist(
                  lapply(
                    1:length(tempPredTable$daysToEvent), 
                    function(x){prod(tempPredTable$survivalT[tempPredTable$daysToEvent <= tempPredTable$daysToEvent[x]])}
                  )
                )
                
                plotly::plot_ly(x = ~ tempPredTable$daysToEvent) %>% 
                  plotly::add_lines(
                    y = tempPredTable$probSurvT, 
                    name = "hv", 
                    line = list(shape = "hv")
                  ) %>%
                  plotly::layout(
                    title = 'Outcome survival', 
                    plot_bgcolor = "#e5ecf6", 
                    xaxis = list(title = 'Time (days)'), 
                    yaxis = list(title = 'Outcome free (0 = 0%, 1 = 100%)')
                  )
              })
              
              
              shiny::showModal(
                shiny::modalDialog(
                  title = "Predictor Diagnostics",
                  shiny::basicPage(
                    shiny::tags$head(shiny::tags$style(".modal-dialog{ width:95%}")),
                    shiny::div(
                      shiny::p('Were predictor assessments made without knowledge of outcome data? (if outcome occur shortly after index this may be problematic)'),
                      shiny::p(''),
                      
                      shiny::selectInput(
                        inputId = session$ns('predictorParameters'),
                        label = 'Select Parameter',
                        multiple = F, 
                        choices = unique(predTable$inputType)
                      ),
                      
                      plotly::plotlyOutput(session$ns('predictorPlot'))
                    )
                  ),
                  size = "l",
                  easyClose = T
                ))

            }
        })
          
          # OUTCOME
          # =================
      shiny::observeEvent(modelTableOutputs$actionCount(), {
        if(modelTableOutputs$actionType() == 'outcomes'){
         
              outcomeTable <- getPredictionDiagnosticOutcomes(
                diagnosticId = diagnosticTable()$diagnosticId[modelTableOutputs$actionIndex()$index],
                connectionHandler = connectionHandler,
                resultDatabaseSettings = resultDatabaseSettings  
              )
              
              #output$predictorPlot <-  
              output$outcomePlot <- plotly::renderPlotly({
                plotly::plot_ly(
                  data = outcomeTable %>%
                    dplyr::filter(
                      .data$aggregation == ifelse(
                        is.null(input$outcomeParameters),
                        unique(outcomeTable$aggregation)[1],
                        input$outcomeParameters
                      )
                    ) %>% 
                    dplyr::group_by(.data$inputType), # dep fix
                  x = ~ xvalue, 
                  y = ~ outcomePercent, 
                  #group = ~ inputType,
                  color = ~ inputType,
                  type = 'scatter', 
                  mode = 'lines'
                ) %>%
                  plotly::layout(
                    title = "Outcome rate",
                    xaxis = list(title = "Value"),
                    yaxis = list (title = "Percent of cohort with outcome")
                  )
              })
              
              
              shiny::showModal(
                shiny::modalDialog(
                  title = "Outcome Diagnostics",
                  shiny::basicPage(
                    shiny::tags$head(shiny::tags$style(".modal-dialog{ width:95%}")),
                    shiny::div(
                      shiny::p('Was the outcome determined appropriately? (Are age/sex/year/month trends expected?)'),
                      shiny::p(''),
                      
                      shiny::selectInput(
                        inputId = session$ns('outcomeParameters'),
                        label = 'Select Parameter',
                        multiple = F, 
                        choices = unique(outcomeTable$aggregation)
                      ),
                      
                      plotly::plotlyOutput(session$ns('outcomePlot'))
                    )
                  ),
                  size = "l",
                  easyClose = T
                ))
              
            }
          })
          
        
    }
  ) # server
}


# helpers


# get the data
getPredictionDiagnostics <- function(
  modelDesignId,
  connectionHandler,
  resultDatabaseSettings,
  threshold1_2 = 0.9
){
  if(!is.null(modelDesignId)){
    print(paste0('model design: ', modelDesignId))
  }
  
  sql <- "SELECT distinct design.MODEL_DESIGN_ID,
          diagnostics.diagnostic_id,
          database.DATABASE_NAME,
          cohortT.COHORT_NAME target_name,
          cohortO.COHORT_NAME outcome_name,
          summary.PROBAST_ID,
          summary.RESULT_VALUE
          
          from 
          (select * from @schema.@plp_table_prefixDIAGNOSTICS where MODEL_DESIGN_ID = @model_design_id) as diagnostics 
          inner join
          @schema.@plp_table_prefixMODEL_DESIGNS design 
          on diagnostics.MODEL_DESIGN_ID = design.MODEL_DESIGN_ID 
          
          inner join
          @schema.@plp_table_prefixDIAGNOSTIC_SUMMARY summary 
          on diagnostics.DIAGNOSTIC_ID = summary.DIAGNOSTIC_ID 

          inner join      
          (select dd.database_id, md.cdm_source_abbreviation as database_name
                   from @schema.@database_table_prefixdatabase_meta_data md inner join 
                   @schema.@plp_table_prefixdatabase_details dd 
                   on md.database_id = dd.database_meta_data_id) as database 
          on database.database_id = diagnostics.database_id

         inner join  
          @schema.@plp_table_prefixCOHORTS cohortT 
         on cohortT.cohort_id = design.target_id 

          inner join
          @schema.@plp_table_prefixCOHORTS cohortO 
          on cohortO.cohort_id = design.outcome_id;
  "
  
  summaryTable <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
    model_design_id = modelDesignId,
    database_table_prefix = resultDatabaseSettings$databaseTablePrefix
  )
  
  if(nrow(summaryTable)==0){
    ParallelLogger::logInfo("No diagnostic summary")
    return(NULL)
  }
  
  summary <- summaryTable %>% tidyr::pivot_wider(
    id_cols = c(
      'diagnosticId', 
      'databaseName', 
      'targetName', 
      'outcomeName'
    ),
    names_from = 'probastId',
    values_from = 'resultValue'
  )
  
  summary$`1.2` <- ifelse(
    apply(summary[,grep('1.2.', colnames(summary))] > threshold1_2, 1, sum) == length(grep('1.2.', colnames(summary))),
    'Pass', 
    'Fail'
  )
  
  summary <- summary[, - grep('1.2.', colnames(summary))] %>%
    dplyr::relocate("1.2", .after = "1.1")
  ParallelLogger::logInfo("got summary")
  return(summary)
}


getPredictionDiagnosticParticipants <- function(
  diagnosticId,
  connectionHandler,
  resultDatabaseSettings
){
  
  sql <- "SELECT * FROM @schema.@plp_table_prefix@table_name WHERE diagnostic_id = @diagnostic_id;"

  participants <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    table_name = 'diagnostic_participants',
    plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
    diagnostic_id = diagnosticId
  )
  
  participants$parameter <- unlist(
    lapply(
      participants$design, 
      function(x){strsplit(x, ':')[[1]][1]}
    )
  )
  participants$paramvalue <- unlist(
    lapply(
      participants$design, 
      function(x){gsub(' ', '', strsplit(x, ':')[[1]][2])}
    )
  )
  
  return(participants)
  
}

getPredictionDiagnosticPredictors <- function(
  diagnosticId,
  connectionHandler,
  resultDatabaseSettings
){
  
  sql <- "SELECT * FROM @schema.@plp_table_prefix@table_name WHERE diagnostic_id = @diagnostic_id;"

  predictors <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    table_name = 'diagnostic_predictors',
    plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
    diagnostic_id = diagnosticId
  )
  
  return(predictors)
}

getPredictionDiagnosticOutcomes <- function(
  diagnosticId,
  connectionHandler,
  resultDatabaseSettings
){
  
  sql <- "SELECT * FROM @schema.@plp_table_prefix@table_name WHERE diagnostic_id = @diagnostic_id;"

  outcomes <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    table_name = 'diagnostic_outcomes',
    plp_table_prefix = resultDatabaseSettings$plpTablePrefix,
    diagnostic_id = diagnosticId
  )
  
  return(outcomes)
  
}
