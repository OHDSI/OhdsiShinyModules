# @file description-aggregateFeatures.R
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


#' The module viewer for exploring aggregate feature results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the description aggregate feature module
#'
#' @export
descriptionAggregateFeaturesViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    
    # summary table
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info', 
        width = 12,
        title = 'Options',
        solidHeader = TRUE,
        shiny::p('Click view to see the plot options for the selection:'),
        reactable::reactableOutput(ns('optionsTable'))
      )
    ),
    
    
    # COV: RUN_ID	DATABASE_ID	COHORT_DEFINITION_ID	COVARIATE_ID	SUM_VALUE	AVERAGE_VALUE
    # COV REF: RUN_ID	DATABASE_ID	COVARIATE_ID	COVARIATE_NAME	ANALYSIS_ID	CONCEPT_ID
    # settings: RUN_ID	DATABASE_ID	COVARIATE_SETTING_JSON	RISK_WINDOW_START	START_ANCHOR	RISK_WINDOW_END	END_ANCHOR	COMBINED_COHORT_ID	TARGET_COHORT_ID	OUTCOME_COHORT_ID	COHORT_TYPE
    # analysis_ref: RUN_ID	DATABASE_ID	ANALYSIS_ID	ANALYSIS_NAME	DOMAIN_ID	START_DAY	END_DAY	IS_BINARY	MISSING_MEANS_ZERO
    # cov cont: RUN_ID	DATABASE_ID	COHORT_DEFINITION_ID	COVARIATE_ID	COUNT_VALUE	MIN_VALUE	MAX_VALUE	AVERAGE_VALUE	STANDARD_DEVIATION	MEDIAN_VALUE	P_10_VALUE	P_25_VALUE	P_75_VALUE	P_90_VALUE
    
    
    # add table with options to select T, O and TAR

    shiny::fluidRow(
      # add UI to pick database/type 1 and database/type 2
      shinydashboard::box(
        title = 'Select database and cohort types:',
        status = 'primary',
        width = 12,
        solidHeader = TRUE,
        shiny::uiOutput(ns("inputsDesc"))
      )
    ),
    
    shinydashboard::tabBox(
      width = 12,
      # Title can include an icon
      title = shiny::tagList(shiny::icon("gear"), "Plots"),
      shiny::tabPanel("Binary Features",
                      plotly::plotlyOutput(ns("binaryPlot"))
      ),
      shiny::tabPanel("Continuous Features", 
                      plotly::plotlyOutput(ns("continuousPlot"))
      )
    )
    
    # add table
    #reactable::reactableOutput(outputId = ns('binaryTable')),
    #reactable::reactableOutput(outputId = ns('continuousTable'))
    )
}


#' The module server for exploring aggregate features results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param con the connection to the prediction result database
#' @param mainPanelTab the current tab 
#' @param schema the database schema for the model results
#' @param dbms the database management system for the model results
#' @param tablePrefix a string that appends the tables in the result schema
#' @param tempEmulationSchema  The temp schema (optional)
#' 
#' @return
#' The server to the description aggregate features module
#'
#' @export
descriptionAggregateFeaturesServer <- function(
  id, 
  con,
  mainPanelTab,
  schema, 
  dbms,
  tablePrefix,
  tempEmulationSchema
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      #if(mainPanelTab() != 'Feature Comparison'){
      #  return(invisible(NULL))
      #}
      
      targetId <- shiny::reactiveVal(NULL)
      outcomeId <- shiny::reactiveVal(NULL)
      riskWindowStart <- shiny::reactiveVal(NULL)
      riskWindowEnd <- shiny::reactiveVal(NULL)
      endAnchor <- shiny::reactiveVal(NULL)
      startAnchor <- shiny::reactiveVal(NULL)
      
      types <- c(
        'Target',
        'Outcome',
        'Target with outcome during TAR (T index)',
        'Target with outcome during TAR (O index)',
        'Target without outcome during TAR'
      )
      typesTranslate <- c(
        'T',
        'O',
        'TnO',
        'OnT',
        'TnOc'
      )
      
      # get the possible options
      options <- getAggregateFeatureOptions(
        con = con ,
        schema = schema, 
        dbms = dbms,
        tablePrefix = tablePrefix,
        tempEmulationSchema = tempEmulationSchema
      )
      
      
      # add buttons
      output$optionsTable <- reactable::renderReactable({
        reactable::reactable(
          data = cbind(
            options,
            view = rep("",nrow(options))
          ),
          columns = list(  
            view = reactable::colDef(
              name = "",
              sortable = FALSE,
              cell = function() htmltools::tags$button("Select")
            )
          ),
          onClick = reactable::JS(paste0("function(rowInfo, column) {
    // Only handle click events on the 'details' column
    if (column.id !== 'view') {
      return
    }

    if (window.Shiny) {
    if(column.id == 'view'){
      Shiny.setInputValue('",session$ns('descAgSelect'),"', { index: rowInfo.index + 1 }, { priority: 'event' })
    }
    }
  }")
          ),
          filterable = TRUE
        )
      })
      
      # set the reactive vars
      shiny::observeEvent(
        eventExpr = input$descAgSelect,{
          
          targetId(options$targetCohortId[input$descAgSelect$index])
          outcomeId(options$outcomeCohortId[input$descAgSelect$index])
          riskWindowStart(options$riskWindowStart[input$descAgSelect$index])
          riskWindowEnd(options$riskWindowEnd[input$descAgSelect$index])
          startAnchor(options$startAnchor[input$descAgSelect$index])
          endAnchor(options$endAnchor[input$descAgSelect$index])
          
          databases <- getAggregateFeatureDatabases(
            con,
            schema, 
            dbms,
            tablePrefix,
            tempEmulationSchema,
            targetId = targetId(),
            outcomeId = outcomeId(),
            riskWindowStart = riskWindowStart(),
            riskWindowEnd = riskWindowEnd(),
            startAnchor = startAnchor(),
            endAnchor = endAnchor()
          )
          
          output$inputsDesc <- shiny::renderUI({
            
            shiny::fluidPage(
            fluidRow(
              column(width = 3,
                     shiny::selectInput(
                       inputId = session$ns('database1'), 
                       label = 'Database 1: ', 
                       choices = databases, 
                       selected = 1
                     ),
                     shiny::selectInput(
                       inputId = session$ns('database2'), 
                       label = 'Database 2: ', 
                       choices = databases, 
                       selected = 1
                     )
              ),
              column(width = 7, 
                     shiny::selectInput(
                       inputId = session$ns('type1'), 
                       label = 'Type 1: ', 
                       choices = types, 
                       selected = 3
                     ),
                     shiny::selectInput(
                       inputId = session$ns('type2'), 
                       label = 'Type 2: ', 
                       choices = types, 
                       selected = 4
                     )
              ),
              
              column(width = 2, 
                     shiny::actionButton(
                       inputId = session$ns('ag_plot'), 
                       label = 'Click'
                     )
              )
            )
            
            )
          })
          
        })
      
      
      
      shiny::observeEvent(
        eventExpr = input$ag_plot,
        {
          
          allData <- descriptiveGetAggregateData(
            con = con,
            schema = schema, 
            dbms = dbms,
            tablePrefix = tablePrefix,
            tempEmulationSchema = tempEmulationSchema,
            targetId = targetId(),
            outcomeId = outcomeId(),
            riskWindowStart = riskWindowStart(),
            riskWindowEnd = riskWindowEnd(),
            startAnchor = startAnchor(),
            endAnchor = endAnchor(),
            database1 = input$database1,
            database2 = input$database2,
            type1 = typesTranslate[types == input$type1],
            type2 = typesTranslate[types == input$type2]
          )
          
          output$binaryPlot <- plotly::renderPlotly(
            descriptiveFeaturePlot(
              data = allData$binary,
              valueColumn = 'averageValue'
            )
          )
          output$continuousPlot <- plotly::renderPlotly(
            descriptiveFeaturePlot(
              data = allData$continuous,
              valueColumn = 'averageValue'
            )
          )
          
        }
      )
      

      return(invisible(NULL))
      
    }
  )
}

getAggregateFeatureOptions <- function(
  con,
  schema, 
  dbms,
  tablePrefix,
  tempEmulationSchema
){
  
 
  shiny::withProgress(message = 'Getting feature comparison options', value = 0, {
  
  sql <- "SELECT DISTINCT TARGET_COHORT_ID, OUTCOME_COHORT_ID, RISK_WINDOW_START,	START_ANCHOR,	RISK_WINDOW_END,	END_ANCHOR  
          FROM @result_database_schema.@table_prefixSETTINGS
          WHERE TARGET_COHORT_ID != 0 AND OUTCOME_COHORT_ID != 0;"
  sql <- SqlRender::render(
    sql = sql, 
    result_database_schema = schema,
    table_prefix = tablePrefix
  )
  
  shiny::incProgress(1/3, detail = paste("Rendering and translating sql"))
  
  sql <- SqlRender::translate(
    sql = sql, 
    targetDialect = dbms, 
    tempEmulationSchema = tempEmulationSchema
  )
  
  shiny::incProgress(2/3, detail = paste("Extracting options"))
  
  options <- DatabaseConnector::querySql(
    connection = con, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  shiny::incProgress(3/3, detail = paste("Finished"))
  
  })
  
  return(
    options
  )
}


getAggregateFeatureDatabases <- function(
  con,
  schema, 
  dbms,
  tablePrefix,
  tempEmulationSchema,
  targetId,
  outcomeId,
  riskWindowStart,
  riskWindowEnd,
  startAnchor,
  endAnchor
){
  
  shiny::withProgress(message = 'Finding databases with data', value = 0, {
  sql <- "SELECT DISTINCT DATABASE_ID  
          FROM @result_database_schema.@table_prefixSETTINGS
          WHERE TARGET_COHORT_ID = @target_id and OUTCOME_COHORT_ID = @outcome_id
          and RISK_WINDOW_START = @risk_window_start and START_ANCHOR = '@start_anchor'
          and RISK_WINDOW_END = @risk_window_end and	END_ANCHOR = '@end_anchor';"
  
  sql <- SqlRender::render(
    sql = sql, 
    result_database_schema = schema,
    table_prefix = tablePrefix,
    target_id = targetId,
    outcome_id = outcomeId,
    risk_window_start = riskWindowStart,
    start_anchor = startAnchor,
    risk_window_end = riskWindowEnd,
    end_anchor = endAnchor
  )
  shiny::incProgress(1/3, detail = paste("Rendering and translating sql"))
  
  sql <- SqlRender::translate(
    sql = sql, 
    targetDialect = dbms, 
    tempEmulationSchema = tempEmulationSchema
  )
  
  shiny::incProgress(2/3, detail = paste("Extracting databases"))
  
  databases <- DatabaseConnector::querySql(
    connection = con, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )$databaseId
  
  shiny::incProgress(3/3, detail = paste("Finished"))
  
  }
  )
  
  return(databases)
}


addTypeEnd <- function(x){
  if(x == 'TnO'){
    return(1)
  }
  if(x == 'TnOc'){
    return(2)
  }
  if(x == 'OnT'){
    return(3)
  }
return(0)
}

# pulls all data for a target and outcome
descriptiveGetAggregateData <- function(
  con = con,
  schema = schema, 
  dbms = dbms,
  tablePrefix = tablePrefix,
  tempEmulationSchema = tempEmulationSchema,
  targetId,
  outcomeId,
  riskWindowStart,
  riskWindowEnd,
  startAnchor,
  endAnchor,
  database1,
  database2,
  type1,
  type2
){
  
  shiny::withProgress(message = 'Getting Feature Comparison Data', value = 0, {
  sql <- "SELECT RUN_ID
          FROM @result_database_schema.@table_prefixSETTINGS
          WHERE TARGET_COHORT_ID = @target_id and OUTCOME_COHORT_ID = @outcome_id
          and RISK_WINDOW_START = @risk_window_start and START_ANCHOR = '@start_anchor'
          and RISK_WINDOW_END = @risk_window_end and	END_ANCHOR = '@end_anchor'
          and DATABASE_ID  = '@database_id' and COHORT_TYPE = '@type';"
  sql <- SqlRender::render(
    sql = sql, 
    result_database_schema = schema,
    table_prefix = tablePrefix,
    target_id = ifelse(type1 == 'O', 0, targetId),
    outcome_id = ifelse(type1 == 'T', 0, outcomeId),
    risk_window_start = riskWindowStart,
    start_anchor = startAnchor,
    risk_window_end = riskWindowEnd,
    end_anchor = endAnchor,
    database_id = database1,
    type = type1
  )
  
  sql <- SqlRender::translate(
    sql = sql, 
    targetDialect = dbms, 
    tempEmulationSchema = tempEmulationSchema
  )
  
  runId1 <- DatabaseConnector::querySql(
    connection = con, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )$runId
  
  shiny::incProgress(1/5, detail = paste("Got first runIds"))
  
  
  sql <- "SELECT RUN_ID
          FROM @result_database_schema.@table_prefixSETTINGS
          WHERE TARGET_COHORT_ID = @target_id and OUTCOME_COHORT_ID = @outcome_id
          and RISK_WINDOW_START = @risk_window_start and START_ANCHOR = '@start_anchor'
          and RISK_WINDOW_END = @risk_window_end and	END_ANCHOR = '@end_anchor'
          and DATABASE_ID  = '@database_id' and COHORT_TYPE = '@type';"
  sql <- SqlRender::render(
    sql = sql, 
    result_database_schema = schema,
    table_prefix = tablePrefix,
    target_id = ifelse(type2 == 'O', 0, targetId),
    outcome_id = ifelse(type2 == 'T', 0, outcomeId),
    risk_window_start = riskWindowStart,
    start_anchor = startAnchor,
    risk_window_end = riskWindowEnd,
    end_anchor = endAnchor,
    database_id = database2,
    type = type2
  )
  
  sql <- SqlRender::translate(
    sql = sql, 
    targetDialect = dbms, 
    tempEmulationSchema = tempEmulationSchema
  )
  
  runId2 <- DatabaseConnector::querySql(
    connection = con, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )$runId
  
  shiny::incProgress(2/5, detail = paste("Got second runIds"))
  
  sql <- "SELECT cov.*, cov_ref.COVARIATE_NAME, cov_ref.ANALYSIS_ID,
  case when (cov.DATABASE_ID  = '@database_id1' and cov.COHORT_DEFINITION_ID = @cohortDef1 and cov.RUN_ID in (@run_id1)) then 'comp1' else 'comp2' end label
          FROM @result_database_schema.@table_prefixCOVARIATES cov 
          INNER JOIN
          @result_database_schema.@table_prefixCOVARIATE_REF cov_ref
          ON cov.covariate_id = cov_ref.covariate_id 
          and cov.run_id = cov_ref.run_id
          and cov.database_id = cov_ref.database_id
          WHERE 
          (
          (cov.DATABASE_ID  = '@database_id1' and cov.COHORT_DEFINITION_ID = @cohortDef1 and cov.RUN_ID in (@run_id1))
          OR
          (cov.DATABASE_ID  = '@database_id2' and cov.COHORT_DEFINITION_ID = @cohortDef2 and cov.RUN_ID in (@run_id2))
          );"
  sql <- SqlRender::render(
    sql = sql, 
    result_database_schema = schema,
    table_prefix = tablePrefix,
    cohortDef1 = ifelse(type1 == 'O', outcomeId, targetId)*100000 +ifelse(type1 %in% c('T','O'), 0, outcomeId)*10 + addTypeEnd(type1),
    cohortDef2 = ifelse(type2 == 'O', outcomeId, targetId)*100000 +ifelse(type2 %in% c('T','O'), 0, outcomeId)*10 + addTypeEnd(type2),
    database_id1 = database1,
    database_id2 = database2,
    run_id1 = paste(runId1, collapse = ','),
    run_id2 = paste(runId2, collapse = ',')
  )
  sql <- SqlRender::translate(
    sql = sql, 
    targetDialect = dbms, 
    tempEmulationSchema = tempEmulationSchema
  )
  
  shiny::incProgress(3/5, detail = paste("Getting binary data"))
  
  binary <- DatabaseConnector::querySql(
    connection = con, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  shiny::incProgress(4/5, detail = paste("Getting continuous data"))
  
  sql <- "SELECT cov.*, cov_ref.COVARIATE_NAME, cov_ref.ANALYSIS_ID,
  case when (cov.DATABASE_ID  = '@database_id1' and cov.COHORT_DEFINITION_ID = @cohortDef1 and cov.RUN_ID in (@run_id1)) then 'comp1' else 'comp2' end label
          FROM @result_database_schema.@table_prefixCOVARIATES_CONTINUOUS cov 
          INNER JOIN
          @result_database_schema.@table_prefixCOVARIATE_REF cov_ref
          ON cov.covariate_id = cov_ref.covariate_id 
          and cov.run_id = cov_ref.run_id
          and cov.database_id = cov_ref.database_id
          WHERE 
          (
          (cov.DATABASE_ID  = '@database_id1' and cov.COHORT_DEFINITION_ID = @cohortDef1 and cov.RUN_ID in (@run_id1))
          OR
          (cov.DATABASE_ID  = '@database_id2' and cov.COHORT_DEFINITION_ID = @cohortDef2 and cov.RUN_ID in (@run_id2))
          );"
  sql <- SqlRender::render(
    sql = sql, 
    result_database_schema = schema,
    table_prefix = tablePrefix,
    cohortDef1 = ifelse(type1 == 'O', 0, targetId)*100000 +ifelse(type1 == 'T', 0, outcomeId)*10 + addTypeEnd(type1),
    cohortDef2 = ifelse(type2 == 'O', 0, targetId)*100000 +ifelse(type2 == 'T', 0, outcomeId)*10 + addTypeEnd(type2),
    database_id1 = database1,
    database_id2 = database2,
    run_id1 = paste(runId1, collapse = ','),
    run_id2 = paste(runId2, collapse = ',')
  )
  sql <- SqlRender::translate(
    sql = sql, 
    targetDialect = dbms, 
    tempEmulationSchema = tempEmulationSchema
  )
  
  continuous <- DatabaseConnector::querySql(
    connection = con, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  shiny::incProgress(5/5, detail = paste("Finished"))
  }
  )
  
  return(list(
    binary = binary,
    continuous = continuous
  ))
}

descriptiveFeaturePlot <- function(
  data,
  valueColumn = 'averageValue'
){
  
  if(is.null(data)){
    return(NULL)
  }
  
  
  shiny::withProgress(message = 'Generating plots', value = 0, {
  
  comp1 <- data %>% 
    dplyr::filter(.data$label == 'comp1') %>%
    dplyr::select(.data$covariateName, .data[[valueColumn]]) %>%
    dplyr::rename(comp1 = .data[[valueColumn]])
  
  shiny::incProgress(1/5, detail = paste("Filtered comparision 1"))
    
  comp2 <- data %>% 
    dplyr::filter(.data$label == 'comp2') %>%
    dplyr::select(.data$covariateName, .data[[valueColumn]]) %>%
    dplyr::rename(comp2 = .data[[valueColumn]])
  
  shiny::incProgress(2/5, detail = paste("Filtered comparision 2"))
  
  analysisIds <- data %>%
    dplyr::select(.data$covariateName, .data$analysisId) %>%
    dplyr::distinct()
  
  shiny::incProgress(3/5, detail = paste("Extracting analysisIds"))
  
  maxval <- max(max(comp1$comp1, na.rm = T),  max(comp2$comp2, na.rm = T))
  
  allData <- merge(comp1, comp2, by = 'covariateName', all = T)
  allData[is.na(allData)] <- 0
  allData <- merge(allData, analysisIds,  by = 'covariateName', all.x = T)
  
  shiny::incProgress(4/5, detail = paste("Merged data"))
  
  plot <- plotly::plot_ly(x = allData$comp1,
                  showlegend = F) %>%
    plotly::add_markers(y = allData$comp2,
                        color=factor(allData$analysisId),
                        hoverinfo = 'text',
                        text = ~paste(allData$covariateName),
                        showlegend = T
    ) %>%
    plotly::add_trace(x= c(0,maxval), y = c(0,maxval),mode = 'lines',
                      line = list(dash = "dash"), color = I('black'),
                      type='scatter', showlegend = FALSE) %>%
    plotly::layout(#title = 'Prevalance of baseline predictors in persons with and without outcome',
      xaxis = list(title = "Prevalance in selection 1"),
      yaxis = list(title = "Prevalance in selection 2"),
      #legend = l, showlegend = T,
      legend = list(orientation = 'h', y = -0.3), showlegend = T)
  
  shiny::incProgress(5/5, detail = paste("Finished"))
  
  })
  
    return(plot)
}
