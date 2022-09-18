# @file description-timeToEvent.R
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


#' The module viewer for exploring 1 or more cohorts features
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the description cohorts features
#'
#' @export
descriptionTableViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info', width = 12,
        title = 'Options',
        solidHeader = TRUE,
        shiny::p('Select settings:'),
        shiny::uiOutput(ns('cohortInputs'))
        ),
      
    shinydashboard::box(
      status = 'info',
      width = 12,
      # Title can include an icon
      title = shiny::tagList(shiny::icon("gear"), "Table"),
      reactable::reactableOutput(ns('feTable'))
    )
    )
    
    
  )
}


#' The module server for exploring 1 or more cohorts features
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
#' @param cohortTablePrefix a string that appends the cohort table in the result schema
#' @param tempEmulationSchema  The temp schema (optional)
#' @param databaseTable  name of the database table
#' 
#' @return
#' The server to the cohorts features server
#'
#' @export
descriptionTableServer <- function(
  id, 
  con,
  mainPanelTab,
  schema, 
  dbms,
  tablePrefix,
  cohortTablePrefix,
  tempEmulationSchema,
  databaseTable = 'DATABASE_META_DATA'
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      #if(mainPanelTab() != 'Time To Event'){
      #  return(invisible(NULL))
      #}
      
      inputVals <- getDecCohortsInputs(
        con,
        schema, 
        dbms,
        tablePrefix,
        cohortTablePrefix,
        databaseTable,
        tempEmulationSchema
      )
      
      # update UI
      output$cohortInputs <- shiny::renderUI({
        
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::selectInput(
              inputId = session$ns('targetIds'), 
              label = 'Target: ', 
              choices = inputVals$cohortIds, 
              multiple = T
            ),
            
              shiny::selectInput(
                inputId = session$ns('databaseId'), 
                label = 'Database: ', 
                choices = inputVals$databaseIds, 
                multiple = F
              ),
            
            #sidebarPanel(
            #  pickerInput("locInput","Location", choices=c("New Mexico", "Colorado", "California"), options = list(`actions-box` = TRUE),multiple = T)
            #)
            
            shiny::actionButton(
              inputId = session$ns('fetchData'),
              label = 'Select'
            )
          )
        )
      }
      )
      


      # fetch data when targetId changes
      shiny::observeEvent(
        eventExpr = input$fetchData,
        {
          if(is.null(input$targetIds) | is.null(input$databaseId)){
            print('Null ids value')
            return(invisible(NULL))
          }
          allData <- getDesFEData(
            targetIds = input$targetIds,
            databaseId = databaseId,
            con = con,
            schema = schema, 
            dbms = dbms,
            tablePrefix = tablePrefix,
            cohortTablePrefix = cohortTablePrefix,
            tempEmulationSchema = tempEmulationSchema
          )
          
          # do the plots reactively
          output$feTable <- reactable::renderReactable(
            {
              reactable::reactable(
                data = allData
              )
            }
          )
          
        }
      )
    
      return(invisible(NULL))
      
    }
  )
}


getDesFEData <- function(
  targetIds,
  databaseId,
  con,
  schema, 
  dbms,
  tablePrefix,
  cohortTablePrefix,
  tempEmulationSchema
){
  
  
  sql <- "select distinct ref.covariate_id, ref.covariate_name, c.cohort_name, covs.COUNT_VALUE, covs.AVERAGE_VALUE
  from
  (
  select RUN_ID, COHORT_DEFINITION_ID, COVARIATE_ID,	SUM_VALUE as COUNT_VALUE,	AVERAGE_VALUE from
   @result_schema.@table_prefixCOVARIATES
   where DATABASE_ID = '@database_id' and 
   COHORT_DEFINITION_ID in (@cohort_ids)
  union
  select RUN_ID, COHORT_DEFINITION_ID, COVARIATE_ID,	COUNT_VALUE,	AVERAGE_VALUE from
    @result_schema.@table_prefixCOVARIATES_continuous
    where DATABASE_ID = '@database_id' and 
    COHORT_DEFINITION_ID in (@cohort_ids)
  ) covs
  inner join
  @result_schema.@table_prefixcovariate_ref ref
  on covs.RUN_ID = ref.RUN_ID and 
  covs.COVARIATE_ID = ref.COVARIATE_ID
  inner join @result_schema.@cohort_table_prefixcohort_definition c
  on c.cohort_definition_id = covs.COHORT_DEFINITION_ID/100000
  ;
  "

  sql <- SqlRender::render(
    sql = sql, 
    result_schema = schema,
    table_prefix = tablePrefix,
    cohort_table_prefix = cohortTablePrefix,
    cohort_ids = paste(targetIds*100000, collapse = ','),
    database_id = databaseId
  )
  
  resultTable <- DatabaseConnector::querySql(con, sql, snakeCaseToCamelCase = T)
  
  #format
  resultTable <- resultTable %>% 
    tidyr::pivot_wider(
      names_from = .data$cohortName, 
      values_from = c(.data$averageValue, .data$countValue), 
      id_cols = c(.data$covariateId, .data$covariateName)
        )
  
  return(resultTable)
}


getDecCohortsInputs <- function(
  con,
  schema, 
  dbms,
  tablePrefix,
  cohortTablePrefix,
  databaseTable,
  tempEmulationSchema
){
  
  sql <- ' select distinct c.cohort_definition_id, c.cohort_name from
  @result_schema.@cohort_table_prefixcohort_definition c
  inner join
  (select distinct TARGET_COHORT_ID as id
  from @result_schema.@table_prefixsettings
  ) ids
  on ids.id = c.cohort_definition_id
  ;'
  
  sql <- SqlRender::render(
    sql = sql, 
    result_schema = schema,
    table_prefix = tablePrefix,
    cohort_table_prefix = cohortTablePrefix
  )

  idVals <- DatabaseConnector::querySql(con, sql, snakeCaseToCamelCase = T)
  ids <- idVals$cohortDefinitionId
  names(ids) <- idVals$cohortName
  
  sql <- 'select d.database_id, d.cdm_source_abbreviation as database_name
  from @result_schema.@database_table d;'
  
  sql <- SqlRender::render(
    sql = sql, 
    result_schema = schema,
    database_table = databaseTable
  )
  
  database <- DatabaseConnector::querySql(con, sql, snakeCaseToCamelCase = T)
  databaseIds <- database$databaseId
  names(databaseIds) <- database$databaseName
  
  return(
    list(
      cohortIds = ids,
      databaseIds = databaseIds
    )
  )
  
}
