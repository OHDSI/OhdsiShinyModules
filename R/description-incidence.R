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


#' The module viewer for exploring incidence results 
#'
#' @details
#' The user specifies the id for the module
#'+
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the description incidence module
#'
#' @export
descriptionIncidenceViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    
    shinydashboard::box(
      collapsible = TRUE,
      collapsed = TRUE,
      title = "Incidence Rates",
      width = "100%",
      shiny::htmlTemplate(system.file("description-www", "help-incidenceRate.html", package = utils::packageName()))
    ),
    
    shinydashboard::box(
      width = "100%",
      title = 'Options',
      collapsible = TRUE,
      collapsed = F,
      shiny::uiOutput(ns('cohortInputs'))
    ),
    
    shiny::conditionalPanel(
      condition = "input.generate != 0",
      ns = ns,
      
      shiny::uiOutput(ns("IRinputsText")),
    
    shinydashboard::box(
      width = "100%",
      # Title can include an icon
      title = shiny::tagList(shiny::icon("gear"), "Table"),
      
      shiny::downloadButton(
        ns('downloadInc'), 
        label = "Download"
      ),
      shinycssloaders::withSpinner(
        reactable::reactableOutput(ns('incTable'))
      )
    )
    )
    
    
  )
}


#' The module server for exploring incidence results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler the connection to the prediction result database
#' @param mainPanelTab the current tab 
#' @param schema the database schema for the model results
#' @param incidenceTablePrefix a string that appends the incidence table in the result schema
#' @param databaseTable  name of the database table
#' 
#' @return
#' The server to the prediction incidence module
#'
#' @export
descriptionIncidenceServer <- function(
  id, 
  connectionHandler,
  mainPanelTab,
  schema, 
  incidenceTablePrefix,
  databaseTable = 'DATABASE_META_DATA'
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      #if(mainPanelTab() != 'Time To Event'){
      #  return(invisible(NULL))
      #}
      
      cohorts <- getTargetOutcomes(
        connectionHandler,
        schema, 
        incidenceTablePrefix
      )
      

      # update UI
      output$cohortInputs <- shiny::renderUI({
        
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shinyWidgets::pickerInput(
                inputId = session$ns('targetId'), 
                label = 'Target id: ', 
                choices = cohorts$targetIds, 
                multiple = FALSE,
                choicesOpt = list(style = rep_len("color: black;", 999)),
                selected = 1,
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
            shiny::column(
              width = 6,
              shinyWidgets::pickerInput(
                inputId = session$ns('outcomeId'), 
                label = 'Outcome id: ', 
                choices = cohorts$outcomeIds,
                selected = 1,
                choicesOpt = list(style = rep_len("color: black;", 999)),
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
          ),
          
          shiny::actionButton(
            inputId = session$ns('generate'),
            label = 'Generate Report'
          )
        )
      })
      
      allDataDownload <- shiny::reactiveVal(data.frame())
      selectedInputs <- shiny::reactiveVal()
      output$IRinputsText <- shiny::renderUI(selectedInputs())
      

      # fetch data when targetId changes
      shiny::observeEvent(
        eventExpr = input$generate,
        {
          if(is.null(input$targetId) | is.null(input$outcomeId)){
            return(invisible(NULL))
          }
          
          selectedInputs(
            shinydashboard::box(
              status = 'warning', 
              width = "100%",
              title = 'Selected:',
              shiny::div(
                shiny::fluidRow(
                  shiny::column(
                    width = 6,
                    shiny::tags$b("Target:"),
                    names(cohorts$targetIds)[cohorts$targetIds == input$targetId]
                  ),
                  shiny::column(
                    width = 6,
                    shiny::tags$b("Outcome:"),
                    names(cohorts$outcomeIds)[cohorts$outcomeIds == input$outcomeId]
                  )
                )
                
              )
            )
          )
          
          
          allData <- getIncidenceData(
            targetId = input$targetId,
            outcomeId = input$outcomeId,
            connectionHandler = connectionHandler,
            schema = schema, 
            incidenceTablePrefix = incidenceTablePrefix,
            databaseTable = databaseTable
          )
          
          allDataDownload(allData )
          
          # do the plots reactively
          output$incTable <- reactable::renderReactable(
            {
              reactable::reactable(
                data = allData %>% 
                  dplyr::relocate("tarId", .after = "cdmSourceAbbreviation") %>%
                  dplyr::relocate("personsAtRisk", .after = "tarEndOffset") %>% 
                  dplyr::relocate("personDays", .after = "personsAtRisk") %>% 
                  dplyr::relocate("personOutcomes", .after = "personDays") %>% 
                  dplyr::relocate("incidenceProportionP100p", .after = "personOutcomes") %>% 
                  dplyr::relocate("incidenceRateP100py", .after = "incidenceProportionP100p") 
                  ,
                filterable = TRUE,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(10, 50, 100,1000),
                defaultPageSize = 50,
                striped = TRUE,
                highlight = TRUE,
                elementId = "desc-incidence-select",
                
                columns = list(
                  cdmSourceAbbreviation = reactable::colDef( 
                    name = 'Database',
                    filterInput = function(values, name) {
                      shiny::tags$select(
                        # Set to undefined to clear the filter
                        onchange = sprintf("Reactable.setFilter('desc-incidence-select', '%s', event.target.value || undefined)", name),
                        # "All" has an empty value to clear the filter, and is the default option
                        shiny::tags$option(value = "", "All"),
                        lapply(unique(values), shiny::tags$option),
                        "aria-label" = sprintf("Filter %s", name),
                        style = "width: 100%; height: 28px;"
                      )
                    }
                  ),
                  tarId = reactable::colDef( 
                    filterInput = function(values, name) {
                      shiny::tags$select(
                        # Set to undefined to clear the filter
                        onchange = sprintf("Reactable.setFilter('desc-incidence-select', '%s', event.target.value || undefined)", name),
                        # "All" has an empty value to clear the filter, and is the default option
                        shiny::tags$option(value = "", "All"),
                        lapply(unique(values), shiny::tags$option),
                        "aria-label" = sprintf("Filter %s", name),
                        style = "width: 100%; height: 28px;"
                      )
                    }
                  ),
                  refId = reactable::colDef(show = F),
                  databaseId = reactable::colDef(show = F),
                  sourceName = reactable::colDef(show = F),
                  targetCohortDefinitionId = reactable::colDef(show = F),
                  targetName = reactable::colDef(show = F),
                  outcomeId = reactable::colDef(show = F),
                  outcomeCohortDefinitionId = reactable::colDef(show = F),
                  outcomeName = reactable::colDef(show = F),
                  outcomeId = reactable::colDef(show = F),
                  ageId = reactable::colDef(show = F),
                  genderId = reactable::colDef(show = F),
                  subgroupId = reactable::colDef(show = F),
                  incidenceProportionP100p = reactable::colDef(
                    format = reactable::colFormat(digits = 4)
                  ),
                  incidenceRateP100py = reactable::colDef(
                    format = reactable::colFormat(digits = 4)
                  )
                )
                
                
                
                
              )
            }
          )
          
        }
      )
      
      # download
      output$downloadInc <- shiny::downloadHandler(
        filename = function() {
          paste('incidence-data-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          utils::write.csv(allDataDownload(), con)
        }
      )
    
      
      return(invisible(NULL))
      
    }
  )
}

getIncidenceData <- function(
  targetId,
  outcomeId,
  connectionHandler,
  schema, 
  incidenceTablePrefix,
  databaseTable
){
  
  shiny::withProgress(message = 'Getting incidence data', value = 0, {
    
  sql <- 'select d.cdm_source_abbreviation, i.* 
    from @result_schema.@incidence_table_prefixINCIDENCE_SUMMARY i
    inner join @result_schema.@database_table_name d
    on d.database_id = i.database_id
    where target_cohort_definition_id = @target_id
    and outcome_cohort_definition_id = @outcome_id
    ;'
  
  shiny::incProgress(1/2, detail = paste("Created SQL - Extracting..."))
  
  resultTable <- connectionHandler$queryDb(
    sql = sql, 
    result_schema = schema,
    incidence_table_prefix = incidenceTablePrefix,
    target_id = targetId,
    outcome_id = outcomeId,
    database_table_name = databaseTable
  )
  
  shiny::incProgress(2/2, detail = paste("Done..."))
  
  })
  
  
  return(resultTable)
}


getTargetOutcomes <- function(
    connectionHandler,
  schema, 
  incidenceTablePrefix
){
  
  shiny::withProgress(message = 'Getting incidence inputs', value = 0, {
  
  sql <- 'select distinct target_cohort_definition_id, target_name 
  from @result_schema.@incidence_table_prefixINCIDENCE_SUMMARY;'
  
  shiny::incProgress(1/3, detail = paste("Created SQL - Extracting targets"))

  targets <- connectionHandler$queryDb(
    sql = sql, 
    result_schema = schema,
    incidence_table_prefix = incidenceTablePrefix
  )
  targetIds <- targets$targetCohortDefinitionId
  names(targetIds) <- targets$targetName
  
  sql <- 'select distinct outcome_cohort_definition_id, outcome_name 
  from @result_schema.@incidence_table_prefixINCIDENCE_SUMMARY;'

  shiny::incProgress(2/3, detail = paste("Created SQL - Extracting outcomes"))
  
  outcomes <- connectionHandler$queryDb(
    sql = sql, 
    result_schema = schema,
    incidence_table_prefix = incidenceTablePrefix
  )
  
  outcomeIds <- outcomes$outcomeCohortDefinitionId
  names(outcomeIds) <- outcomes$outcomeName
  
  shiny::incProgress(3/3, detail = paste("Done"))
  })
  
  return(
    list(
      targetIds = targetIds,
      outcomeIds = outcomeIds
    )
  )
  
}
