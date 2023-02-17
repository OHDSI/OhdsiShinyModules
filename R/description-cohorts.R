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
      
      shiny::checkboxGroupInput(
        inputId = ns("columnSelect"), 
        label = "Columns to show:",
        inline = T,
        choices = c(
          "Mean" = "averageValue",
          "Count" = "countValue"
        ), 
        selected = c("averageValue", "countValue")
      ),
      
      shiny::downloadButton(
        ns('downloadCohorts'), 
        label = "Download"
      ),
      
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
#' @param connectionHandler the connection to the prediction result database
#' @param mainPanelTab the current tab 
#' @param schema the database schema for the model results
#' @param tablePrefix a string that appends the tables in the result schema
#' @param cohortTablePrefix a string that appends the cohort table in the result schema
#' @param databaseTable  name of the database table
#' 
#' @return
#' The server to the cohorts features server
#'
#' @export
descriptionTableServer <- function(
  id, 
  connectionHandler,
  mainPanelTab,
  schema, 
  tablePrefix,
  cohortTablePrefix,
  databaseTable = 'DATABASE_META_DATA'
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      #if(mainPanelTab() != 'Time To Event'){
      #  return(invisible(NULL))
      #}
      
      inputVals <- getDecCohortsInputs(
        connectionHandler,
        schema, 
        tablePrefix,
        cohortTablePrefix,
        databaseTable
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
      
      reactiveAllData <- shiny::reactiveVal(NULL)
      
      
      shiny::observeEvent(
        eventExpr = input$fetchData,
        {
          if(length(input$targetIds) == 0 | is.null(input$databaseId)){
            print('Null ids value')
            return(invisible(NULL))
          }
          
          # hide/show columns - make allData react
          
          allData <- tryCatch({
            getDesFEData(
              targetIds = input$targetIds,
              databaseId = input$databaseId,
              connectionHandler = connectionHandler,
              schema = schema, 
              tablePrefix = tablePrefix,
              cohortTablePrefix = cohortTablePrefix
            )}, 
            error = function(e){
              shiny::showNotification(paste0('Error: ', e)); return(NULL)
            })
          
          reactiveAllData(allData)
          
          if(!is.null(allData)){
            
            # do the plots reactively
            output$feTable <- reactable::renderReactable(
              {
                reactable::reactable(
                  data = allData,
                  filterable = TRUE,
                  showPageSizeOptions = TRUE,
                  pageSizeOptions = c(10, 50, 100,1000),
                  defaultPageSize = 50,
                  striped = TRUE,
                  highlight = TRUE,
                  
                  columns = list(
                    analysisName = reactable::colDef(
                      filterInput = function(values, name) {
                        shiny::tags$select(
                          # Set to undefined to clear the filter
                          onchange = sprintf("Reactable.setFilter('desc-analysis-select', '%s', event.target.value || undefined)", name),
                          # "All" has an empty value to clear the filter, and is the default option
                          shiny::tags$option(value = "", "All"),
                          lapply(unique(values), shiny::tags$option),
                          "aria-label" = sprintf("Filter %s", name),
                          style = "width: 100%; height: 28px;"
                        )
                      }
                    )
                  ),
                  elementId = "desc-analysis-select"
                  
                  
                )
              }
            )
        } else{
          shiny::showNotification('data NULL')
        }
          
        }
      )
      
      
      # observed the choices to update table
      shiny::observeEvent(
        eventExpr = input$columnSelect,
        {
          
          if(!is.null(reactiveAllData())){
          # filter columns
          columnInd <- input$columnSelect # this tells us whether to include count/mean
          
          inds <- c()
          if(!'countValue' %in% columnInd){
            #remove counts
            inds <- c(inds, grep('countValue', colnames(reactiveAllData())))
          }
          if(!'averageValue' %in% columnInd){
            #remove averages
            inds <- c(inds, grep('averageValue', colnames(reactiveAllData())))
          }
          
          if(length(inds)>0){
            allData <- reactiveAllData()[, -inds]
          } else{
            allData <- reactiveAllData()
          }
            
            # do the plots reactively
            output$feTable <- reactable::renderReactable(
              {
                reactable::reactable(
                  data = allData,
                  filterable = TRUE,
                  showPageSizeOptions = TRUE,
                  pageSizeOptions = c(10, 50, 100,1000),
                  defaultPageSize = 50,
                  striped = TRUE,
                  highlight = TRUE,
                  
                  columns = list(
                    analysisName = reactable::colDef(
                      filterInput = function(values, name) {
                        shiny::tags$select(
                          # Set to undefined to clear the filter
                          onchange = sprintf("Reactable.setFilter('desc-analysis-select', '%s', event.target.value || undefined)", name),
                          # "All" has an empty value to clear the filter, and is the default option
                          shiny::tags$option(value = "", "All"),
                          lapply(unique(values), shiny::tags$option),
                          "aria-label" = sprintf("Filter %s", name),
                          style = "width: 100%; height: 28px;"
                        )
                      }
                    )
                  ),
                  elementId = "desc-analysis-select"
                  
                )
              }
            )
          } else{
            shiny::showNotification('data NULL')
          }
          
        }
      )
      
      
      # download button
      output$downloadCohorts <- shiny::downloadHandler(
        filename = function() {
          paste('cohort-data-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          utils::write.csv(reactiveAllData(), con)
        }
      )
      
    
      return(invisible(NULL))
      
    }
  )
}


getDesFEData <- function(
  targetIds,
  databaseId,
  connectionHandler,
  schema, 
  tablePrefix,
  cohortTablePrefix
){
  
  
  shiny::withProgress(message = 'Getting target comparison data', value = 0, {
    
  sql <- "select distinct ref.covariate_id, ref.covariate_name, an.analysis_name, c.cohort_name, covs.COUNT_VALUE, covs.AVERAGE_VALUE
  from
  (
  select co.RUN_ID, cd.TARGET_COHORT_ID as COHORT_DEFINITION_ID, co.COVARIATE_ID,	
  co.SUM_VALUE as COUNT_VALUE,	co.AVERAGE_VALUE*100 as AVERAGE_VALUE from
   @result_schema.@table_prefixCOVARIATES co
   inner join 
   (select * from @result_schema.@table_prefixcohort_details 
   where DATABASE_ID = '@database_id' and 
   TARGET_COHORT_ID in (@cohort_ids) and COHORT_TYPE = 'T'
   ) as cd
   on co.COHORT_DEFINITION_ID = cd.COHORT_DEFINITION_ID
   and co.DATABASE_ID = cd.DATABASE_ID
  union
  select cc.RUN_ID, cds.TARGET_COHORT_ID as COHORT_DEFINITION_ID, cc.COVARIATE_ID,	cc.COUNT_VALUE,	cc.AVERAGE_VALUE from
    @result_schema.@table_prefixCOVARIATES_continuous cc
    inner join 
    (select * from @result_schema.@table_prefixcohort_details 
   where DATABASE_ID = '@database_id' and 
   TARGET_COHORT_ID in (@cohort_ids) and COHORT_TYPE = 'T'
   ) as cds
   on cc.COHORT_DEFINITION_ID = cds.COHORT_DEFINITION_ID
    and cc.DATABASE_ID = cds.DATABASE_ID
  ) covs
  inner join
  @result_schema.@table_prefixcovariate_ref ref
  on covs.RUN_ID = ref.RUN_ID and 
  covs.COVARIATE_ID = ref.COVARIATE_ID
  inner join @result_schema.@table_prefixanalysis_ref an
  on an.RUN_ID = ref.RUN_ID and 
  an.analysis_id = ref.analysis_id
  inner join @result_schema.@cohort_table_prefixcohort_definition c
  on c.cohort_definition_id = covs.COHORT_DEFINITION_ID
  ;
  "
  
  shiny::incProgress(1/3, detail = paste("Created SQL - Extracting..."))
  
  resultTable <- connectionHandler$queryDb(
    sql = sql, 
    result_schema = schema,
    table_prefix = tablePrefix,
    cohort_table_prefix = cohortTablePrefix,
    cohort_ids = paste(as.double(targetIds), collapse = ','),
    database_id = databaseId
  )
  
  shiny::incProgress(2/3, detail = paste("Formating"))
  
  #format
  resultTable$averageValue <- round(resultTable$averageValue, digits = 2)

  resultTable <- resultTable %>% 
    tidyr::pivot_wider(
      names_from = "cohortName", #.data$cohortName, 
      values_from = c("averageValue", "countValue"), #c(.data$averageValue, .data$countValue), 
      id_cols = c("covariateId", "covariateName", "analysisName") #c(.data$covariateId, .data$covariateName, .data$analysisName)
        )
  
  resultTable$analysisName <- as.factor(resultTable$analysisName)
  
  shiny::incProgress(3/3, detail = paste("Done"))
  
  })
  
  return(resultTable)
}


getDecCohortsInputs <- function(
    connectionHandler,
    schema, 
    tablePrefix,
    cohortTablePrefix,
    databaseTable
){
  
  
  shiny::withProgress(message = 'Getting target comparison inputs', value = 0, {
    
    
  sql <- ' select distinct c.cohort_definition_id, c.cohort_name from
  @result_schema.@cohort_table_prefixcohort_definition c
  inner join
  (select distinct TARGET_COHORT_ID as id
  from @result_schema.@table_prefixcohort_details
  ) ids
  on ids.id = c.cohort_definition_id
  ;'

  shiny::incProgress(1/4, detail = paste("Extracting targetIds"))

  idVals <- connectionHandler$queryDb(
    sql = sql, 
    result_schema = schema,
    table_prefix = tablePrefix,
    cohort_table_prefix = cohortTablePrefix
  )
  ids <- idVals$cohortDefinitionId
  names(ids) <- idVals$cohortName
  
  shiny::incProgress(2/4, detail = paste("Extracted targetIds"))
  
  
  sql <- 'select d.database_id, d.cdm_source_abbreviation as database_name
  from @result_schema.@database_table d;'
  
  shiny::incProgress(3/4, detail = paste("Extracting databaseIds"))
  
  database <- connectionHandler$queryDb(
    sql = sql, 
    result_schema = schema,
    database_table = databaseTable
  )
  databaseIds <- database$databaseId
  names(databaseIds) <- database$databaseName
  
  shiny::incProgress(4/4, detail = paste("Done"))
  
  })
  
  return(
    list(
      cohortIds = ids,
      databaseIds = databaseIds
    )
  )
  
}
