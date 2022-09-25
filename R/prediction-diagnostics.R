# @file prediction-diagnostics.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
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
#' 
#' @return
#' The user interface to the prediction diagnostic module
#'
#' @export
predictionDiagnosticsViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    reactable::reactableOutput(ns('diagnosticSummaryTable')),
    shiny::uiOutput(ns('main'))
  )
  
}

#' The module server for exploring prediction diagnostic results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param modelDesignId the unique id for the model design
#' @param mySchema the database schema for the model results
#' @param con the connection to the prediction result database
#' @param myTableAppend a string that appends the tables in the result schema
#' @param targetDialect the database management system for the model results
#' @param databaseTableAppend a string that appends the database_meta_data table
#' 
#' @return
#' The server to the predcition diagnostic module
#'
#' @export
predictionDiagnosticsServer <- function(
  id,
  modelDesignId, 
  mySchema, 
  con,
  myTableAppend, 
  targetDialect,
  databaseTableAppend
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      shiny::observe({
        if(!is.null(modelDesignId()) ){
          
          diagnosticTable <- getDiagnostics(
            modelDesignId = modelDesignId(),
            mySchema, 
            con,
            myTableAppend, 
            targetDialect,
            databaseTableAppend = databaseTableAppend
          )
          # input tables
          output$diagnosticSummaryTable <- reactable::renderReactable({
            reactable::reactable(
              data = cbind(
                diagnosticTable,
                participants = rep("",nrow(diagnosticTable)),
                predictors = rep("",nrow(diagnosticTable)),
                outcomes = rep("",nrow(diagnosticTable))
              ),
              columns = list(
                '1.1' = reactable::colDef(
                  cell = reactable::JS("
    function(cellInfo) {
      // Render as an X mark or check mark
      if(cellInfo.value === 'Fail'){return '\u274c Fail'} else if(cellInfo.value === 'Pass'){return '\u2714\ufe0f Pass'} else{return '? Unkown'}
    }
  ")),
                '1.2' = reactable::colDef(
                  cell = reactable::JS("
    function(cellInfo) {
      // Render as an X mark or check mark
      if(cellInfo.value === 'Fail'){return '\u274c Fail'} else if(cellInfo.value === 'Pass'){return '\u2714\ufe0f Pass'} else{return '? Unkown'}
    }
  ")),
                '2.1' = reactable::colDef(
                  cell = reactable::JS("
    function(cellInfo) {
      // Render as an X mark or check mark
      if(cellInfo.value === 'Fail'){return '\u274c Fail'} else if(cellInfo.value === 'Pass'){return '\u2714\ufe0f Pass'} else{return '? Unkown'}
    }
  ")),   
                '2.2' = reactable::colDef(
                  cell = reactable::JS("
    function(cellInfo) {
      // Render as an X mark or check mark
      if(cellInfo.value === 'Fail'){return '\u274c Fail'} else if(cellInfo.value === 'Pass'){return '\u2714\ufe0f Pass'} else{return '? Unkown'}
    }
  ")),
                '2.3' = reactable::colDef(
                  cell = reactable::JS("
    function(cellInfo) {
      // Render as an X mark or check mark
      if(cellInfo.value === 'Fail'){return '\u274c Fail'} else if(cellInfo.value === 'Pass'){return '\u2714\ufe0f Pass'} else{return '? Unkown'}
    }
  ")),
                '3.4' = reactable::colDef(
                  cell = reactable::JS("
    function(cellInfo) {
      // Render as an X mark or check mark
      if(cellInfo.value === 'Fail'){return '\u274c Fail'} else if(cellInfo.value === 'Pass'){return '\u2714\ufe0f Pass'} else{return '? Unkown'}
    }
  ")),
                '3.6' = reactable::colDef(
                  cell = reactable::JS("
    function(cellInfo) {
      // Render as an X mark or check mark
      if(cellInfo.value === 'Fail'){return '\u274c Fail'} else if(cellInfo.value === 'Pass'){return '\u2714\ufe0f Pass'} else{return '? Unkown'}
    }
  ")),
                '4.1' = reactable::colDef(
                  cell = reactable::JS("
    function(cellInfo) {
      // Render as an X mark or check mark
      if(cellInfo.value === 'Fail'){return '\u274c Fail'} else if(cellInfo.value === 'Pass'){return '\u2714\ufe0f Pass'} else{return '? Unkown'}
    }
  ")),
                participants = reactable::colDef(
                  name = "",
                  sortable = FALSE,
                  cell = function() htmltools::tags$button("View Participants")
                ),
                predictors = reactable::colDef(
                  name = "",
                  sortable = FALSE,
                  cell = function() htmltools::tags$button("View Predictors")
                ),
                outcomes = reactable::colDef(
                  name = "",
                  sortable = FALSE,
                  cell = function() htmltools::tags$button("View Outcomes")
                )
              ),
              
              onClick = reactable::JS(
                paste0(
                  "function(rowInfo, column) {
    // Only handle click events on the 'details' column
    if (column.id !== 'participants' & column.id !== 'predictors' & column.id !== 'outcomes') {
      return
    }

    // Display an alert dialog with details for the row
    //window.alert('Details for row ' + rowInfo.index + ':\\n' + JSON.stringify(rowInfo.values, null, 2))

    // Send the click event to Shiny, which will be available in input$show_details
    // Note that the row index starts at 0 in JavaScript, so we add 1
    if (window.Shiny) {
    if(column.id == 'participants'){
      Shiny.setInputValue('",session$ns('show_participants'),"', { index: rowInfo.index + 1 }, { priority: 'event' })
    }
    if(column.id == 'predictors'){
      Shiny.setInputValue('",session$ns('show_predictors'),"', { index: rowInfo.index + 1 }, { priority: 'event' })
    }
    if(column.id == 'outcomes'){
      Shiny.setInputValue('",session$ns('show_outcomes'),"', { index: rowInfo.index + 1 }, { priority: 'event' })
    }
    }
  }"
                )
                
              )
            )
            
          }) # end reactable
          
          
          # listen
          # PARTICIPANTS
          #============
          shiny::observeEvent(
            input$show_participants,
            {
              participants <- getDiagnosticParticipants(
                diagnosticId = diagnosticTable$diagnosticId[input$show_participants$index],
                mySchema, 
                con,
                myTableAppend, 
                targetDialect  
              )
              
              output$participants <- reactable::renderReactable({
                reactable::reactable(
                  data = participants %>% 
                    dplyr::filter(.data$parameter == ifelse(is.null(input$participantParameters), unique(participants$parameter)[1], input$participantParameters)) %>%
                    dplyr::select(
                      .data$probastId,
                      .data$paramvalue,
                      .data$metric, 
                      .data$value
                    ) %>%
                    dplyr::mutate(
                      value = format(.data$value, nsmall = 2, )
                    )  %>%
                    tidyr::pivot_wider(
                      names_from = .data$paramvalue, 
                      values_from = .data$value
                    )
                )
              })
              output$main <- shiny::renderUI({
                shiny::div(
                  shiny::selectInput(
                    inputId = session$ns('participantParameters'),
                    label = 'Select Parameter',
                    multiple = F, 
                    choices = unique(participants$parameter)
                  ),
                  reactable::reactableOutput(session$ns('participants'))
                )
              }) # renderUI
            }
          ) # end observed event
          
          
          
          #  PREDICTOR
          #==================
          shiny::observeEvent(
            input$show_predictors,
            {
              
              predTable <- getDiagnosticPredictors(
                diagnosticId = diagnosticTable$diagnosticId[input$show_predictors$index],
                mySchema, 
                con,
                myTableAppend, 
                targetDialect  
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
                    .data$daysToEvent, 
                    .data$outcomeAtTime, 
                    .data$observedAtStartOfDay
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
              
              output$main <- shiny::renderUI({
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
                
              }) # renderUI
            }
          )
          
          # OUTCOME
          # =================
          shiny::observeEvent(
            input$show_outcomes,
            {
              
              outcomeTable <- getDiagnosticOutcomes(
                diagnosticId = diagnosticTable$diagnosticId[input$show_outcomes$index],
                mySchema, 
                con,
                myTableAppend, 
                targetDialect  
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
                    ), 
                  x = ~ xvalue, 
                  y = ~ outcomePercent, 
                  group = ~ inputType,
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
              
              output$main <- shiny::renderUI({
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
                
              }) # renderUI
            }
          )
          
          
          
        } # not null
      }) # observe
    }
  ) # server
}


# helpers


# get the data
getDiagnostics <- function(
  modelDesignId,
  mySchema, 
  con,
  myTableAppend, 
  targetDialect,
  databaseTableAppend = myTableAppend,
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
          @my_schema.@my_table_appendDIAGNOSTICS diagnostics inner join
          @my_schema.@my_table_appendMODEL_DESIGNS design inner join
          @my_schema.@my_table_appendDIAGNOSTIC_SUMMARY summary inner join
          
          (select dd.database_id, md.cdm_source_abbreviation as database_name
                   from @my_schema.@database_table_appenddatabase_meta_data md inner join 
                   @my_schema.@my_table_appenddatabase_details dd 
                   on md.database_id = dd.database_meta_data_id) 
          as database inner join
          
          @my_schema.@my_table_appendCOHORTS cohortT inner join
          @my_schema.@my_table_appendCOHORTS cohortO 
          
          on diagnostics.DIAGNOSTIC_ID = summary.DIAGNOSTIC_ID and
          diagnostics.MODEL_DESIGN_ID = design.MODEL_DESIGN_ID and
          cohortT.cohort_id = design.target_id and
          cohortO.cohort_id = design.outcome_id and
          database.database_id = diagnostics.database_id
          
          where diagnostics.MODEL_DESIGN_ID = @model_design_id
  "
  
  sql <- SqlRender::render(
    sql = sql, 
    my_schema = mySchema,
    my_table_append = myTableAppend,
    model_design_id = modelDesignId,
    database_table_append = databaseTableAppend
  )
  
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  
  summaryTable <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(summaryTable) <- SqlRender::snakeCaseToCamelCase(colnames(summaryTable))
  
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
    dplyr::relocate(.data$`1.2`, .after = .data$`1.1`)
  ParallelLogger::logInfo("got summary")
  return(summary)
}


getDiagnosticParticipants <- function(
  diagnosticId,
  mySchema, 
  con,
  myTableAppend, 
  targetDialect  
){
  
  sql <- "SELECT * FROM @my_schema.@table_name WHERE diagnostic_id = @diagnostic_id"
  sql <- SqlRender::render(
    sql = sql, 
    my_schema = mySchema,
    table_name = 'diagnostic_participants',
    diagnostic_id = diagnosticId
  )
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  participants <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(participants) <- SqlRender::snakeCaseToCamelCase(colnames(participants))
  
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

getDiagnosticPredictors <- function(
  diagnosticId,
  mySchema, 
  con,
  myTableAppend, 
  targetDialect  
){
  
  sql <- "SELECT * FROM @my_schema.@table_name WHERE diagnostic_id = @diagnostic_id"
  sql <- SqlRender::render(
    sql = sql, 
    my_schema = mySchema,
    table_name = 'diagnostic_predictors',
    diagnostic_id = diagnosticId
  )
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  predictors <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(predictors) <- SqlRender::snakeCaseToCamelCase(colnames(predictors))
  
  return(predictors)
}

getDiagnosticOutcomes <- function(
  diagnosticId,
  mySchema, 
  con,
  myTableAppend, 
  targetDialect  
){
  
  sql <- "SELECT * FROM @my_schema.@table_name WHERE diagnostic_id = @diagnostic_id"
  sql <- SqlRender::render(
    sql = sql, 
    my_schema = mySchema,
    table_name = 'diagnostic_outcomes',
    diagnostic_id = diagnosticId
  )
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  outcomes <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(outcomes) <- SqlRender::snakeCaseToCamelCase(colnames(outcomes))
  
  return(outcomes)
  
}

