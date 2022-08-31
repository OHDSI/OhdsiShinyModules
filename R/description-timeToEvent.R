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


#' The module viewer for exploring time to event results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the description time to event module
#'
#' @export
descriptionTimeToEventViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info', width = 12,
        title = 'Options',
        solidHeader = TRUE,
        shiny::p('Select settings:'),
        shiny::uiOutput(ns('timeToEventInputs'))
        )
    ),
    
    shiny::fluidRow(
    shinydashboard::tabBox(
      width = 12,
      # Title can include an icon
      title = shiny::tagList(shiny::icon("gear"), "Plots"),
      shiny::tabPanel(
        "Time To Event Plot",
         shiny::plotOutput(ns('timeToEvent')) #shinycssloaders::withSpinner()
      )
    )
    )
    
    
  )
}


#' The module server for exploring time to event results 
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
#' The server to the prediction time to event module
#'
#' @export
descriptionTimeToEventServer <- function(
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
      
      #if(mainPanelTab() != 'Time To Event'){
      #  return(invisible(NULL))
      #}
      
      # get the possible target ids
      bothIds <- timeToEventGetIds(
        con,
        schema, 
        dbms,
        tablePrefix,
        tempEmulationSchema
      )

      shiny::observeEvent(
        input$targetId,{
          val <- bothIds$outcomeIds[[which(names(bothIds$outcomeIds) == input$targetId)]]
          shiny::updateSelectInput(
            session = session,
            inputId = 'outcomeId', 
            label = 'Outcome id: ',
            choices = val
          )
        }
      )
      
      # update UI
      output$timeToEventInputs <- shiny::renderUI({
        
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::selectInput(
              inputId = session$ns('targetId'), 
              label = 'Target id: ', 
              choices = bothIds$targetIds, 
              multiple = FALSE
            ),
            
            shiny::selectInput(
              inputId = session$ns('outcomeId'), 
              label = 'Outcome id: ', 
              choices = bothIds$outcomeIds[[1]],
              selected = 1
            ),
            
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
          if(is.null(input$targetId) | is.null(input$outcomeId)){
            print('Null ids value')
            return(invisible(NULL))
          }
          allData <- getTimeToEventData(
            targetId = input$targetId,
            outcomeId = input$outcomeId,
            con = con,
            schema = schema, 
            dbms = dbms,
            tablePrefix = tablePrefix,
            tempEmulationSchema = tempEmulationSchema
          )
          
          # TODO: create  NEW UI FOR SELECTING DATABASES
          # find databases and set to UI
          databases <- unique(allData$databaseId)
          
          # do the plots reactively
          output$timeToEvent <- shiny::renderPlot(
            plotTimeToEvent(
              timeToEventData = allData
            )
          )
          
        }
      )
    
      
      return(invisible(NULL))
      
    }
  )
}

timeToEventGetIds <- function(
  con,
  schema, 
  dbms,
  tablePrefix,
  tempEmulationSchema
){
  
  shiny::withProgress(message = 'Getting time to event T and O ids', value = 0, {
  
  sql <- "SELECT DISTINCT TARGET_COHORT_DEFINITION_ID, OUTCOME_COHORT_DEFINITION_ID FROM @result_database_schema.@table_prefixTIME_TO_EVENT;"
  sql <- SqlRender::render(
    sql = sql, 
    result_database_schema = schema,
    table_prefix = tablePrefix
  )
  
  shiny::incProgress(1/4, detail = paste("Rendering and translating sql"))
  
  sql <- SqlRender::translate(
    sql = sql, 
    targetDialect = dbms, 
    tempEmulationSchema = tempEmulationSchema
  )
  
  shiny::incProgress(2/4, detail = paste("Fetching ids"))
  
  bothIds <- DatabaseConnector::querySql(
    connection = con, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  shiny::incProgress(3/4, detail = paste("Processing ids"))
  
  targetIds <- unique(bothIds$targetCohortDefinitionId)
  
  outcomeIds <- lapply(targetIds, function(x){unique(bothIds$outcomeCohortDefinitionId[bothIds$targetCohortDefinitionId == x])})
  names(outcomeIds) <- targetIds
  
  shiny::incProgress(4/4, detail = paste("Finished"))
  
  })
  
  return(
    list(
      targetIds = targetIds, 
      outcomeIds = outcomeIds
      )
  )
}

# pulls all data for a target and outcome
getTimeToEventData <- function(
  targetId,
  outcomeId,
  con,
  schema, 
  dbms,
  tablePrefix,
  tempEmulationSchema
){
  
  
  shiny::withProgress(message = 'Extracting time to event data', value = 0, {
  
  sql <- "SELECT * FROM @result_database_schema.@table_prefixTIME_TO_EVENT 
          where TARGET_COHORT_DEFINITION_ID = @target_id
          and OUTCOME_COHORT_DEFINITION_ID = @outcome_id;"
  sql <- SqlRender::render(
    sql = sql, 
    result_database_schema = schema,
    table_prefix = tablePrefix,
    target_id = targetId,
    outcome_id = outcomeId
  )
  
  shiny::incProgress(1/3, detail = paste("Rendering and translating sql"))
  
  sql <- SqlRender::translate(
    sql = sql, 
    targetDialect = dbms, 
    tempEmulationSchema = tempEmulationSchema
  )
  
  shiny::incProgress(2/3, detail = paste("Fetching data"))
  
  data <- DatabaseConnector::querySql(
    connection = con, 
    sql = sql, 
    snakeCaseToCamelCase = T
  )
  
  shiny::incProgress(3/3, detail = paste("Finished"))
  
  })
  
  return(data)
}

plotTimeToEvent <- function(
  timeToEventData
){
  
  if(is.null(timeToEventData)){
    return(NULL)
  }
  
  nDatabases <- length(unique(timeToEventData$databaseId))
  
  shiny::withProgress(message = 'Plotting time to event', value = 0, {
  
  shiny::incProgress(1/2, detail = paste("Generating plot"))
  
  plot <- ggplot2::ggplot(
    data = timeToEventData %>% dplyr::mutate(fillGroup = paste0(.data$outcomeType, '-', .data$targetOutcomeType)), 
    ggplot2::aes(
      x = .data$timeToEvent, 
      y = .data$numEvents,
      fill = .data$fillGroup,
      width = as.double(gsub('-day','',gsub('per ','',.data$timeScale)))
      )
    ) +
    ggplot2::geom_bar(
      #position="stacked",
      stat = "identity"
      ) +
    #ggplot2::geom_text(
    #  ggplot2::aes(
    #    label = .data$numEvents
    #    ), 
    #  vjust = 1.6, 
    #  color = "white", 
    #  size = 3.5
    #  ) +
    ggplot2::facet_wrap(ncol = nDatabases ,
      .data$timeScale ~ .data$databaseId , scales = 'free'
        ) +
    ggplot2::theme_minimal()
  
  shiny::incProgress(2/2, detail = paste("Finished"))
  
  })
  
  
    return(plot)
}
