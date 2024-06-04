# @file characterization-timeToEvent.R
#
# Copyright 2024 Observational Health Data Sciences and Informatics
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
#' The user interface to the characterization time to event module
#'
#' @export
characterizationTimeToEventViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
  
    shinydashboard::box(
      width = "100%",
      title = "",
      
      shiny::uiOutput(ns('timeToEventPlotInputs')),
      shinycssloaders::withSpinner(
        shiny::plotOutput(ns('timeToEvent'))
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
#' @param connectionHandler the connection to the prediction result database
#' @param resultDatabaseSettings a list containing the characterization result schema, dbms, tablePrefix, databaseTable and cgTablePrefix
#' 
#' @return
#' The server to the prediction time to event module
#'
#' @export
characterizationTimeToEventServer <- function(
  id, 
  connectionHandler,
  resultDatabaseSettings,
  targetId,
  outcomeId
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      allData <- shiny::reactive({
        getTimeToEventData(
          targetId = targetId(),
          outcomeId = outcomeId(),
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings
        )
      })
      
      output$timeToEventPlotInputs <- shiny::renderUI({
        
        shiny::fluidPage(
          shiny::fluidRow(
            
            shiny::selectInput(
              inputId = session$ns("databases"),
              label = "Databases:",
              multiple = T, 
              choices = unique(allData()$databaseName),
              selected = unique(allData()$databaseName)
              ),
            
            shiny::fluidRow(
              shiny::column(
                width = 3,
                shiny::selectInput(
                  inputId = session$ns("times"), 
                  label = "Timespan:",
                  multiple = T, 
                  choices =  unique(allData()$timeScale),
                  selected =  unique(allData()$timeScale)
                )
              ),
              
              shiny::column(
                width = 3,
                shiny::selectInput(
                  inputId = session$ns("outcomeTypes"), 
                  label = "Outcome occurrence type:",
                  multiple = T, 
                  choices =  unique(allData()$outcomeType),
                  selected =  unique(allData()$outcomeType)
                )
              ),
              
              shiny::column(
                width = 6,
                shiny::selectInput(
                  inputId = session$ns("targetOutcomeTypes"), 
                  label = "Timing of outcome:",
                  multiple = T, 
                  choices =  unique(allData()$targetOutcomeType),
                  selected =  unique(allData()$targetOutcomeType)
                )
              )
            )
            
            
          )
        )
      }
      )
      
      output$timeToEvent <- shiny::renderPlot(
          plotTimeToEvent(
            timeToEventData = allData, # reactive
            databases = input$databases,
            times = input$times,
            outcomeType = input$outcomeTypes,
            targetOutcomeType = input$targetOutcomeTypes
          )
        )
    
      
      return(invisible(NULL))
      
    }
  )
}

# pulls all data for a target and outcome
getTimeToEventData <- function(
  targetId,
  outcomeId,
  connectionHandler,
  resultDatabaseSettings
){
  if(is.null(targetId)){
    return(NULL)
  }
  
  shiny::withProgress(message = 'Extracting time to event data', value = 0, {
  
  sql <- "SELECT tte.*, d.CDM_SOURCE_ABBREVIATION as database_name 
          FROM @schema.@c_table_prefixTIME_TO_EVENT tte
          inner join @schema.@database_table d
          on tte.database_id = d.database_id
          where tte.TARGET_COHORT_DEFINITION_ID = @target_id
          and tte.OUTCOME_COHORT_DEFINITION_ID = @outcome_id;"

  shiny::incProgress(1/3, detail = paste("Fetching data"))
  
  data <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    target_id = targetId,
    outcome_id = outcomeId,
    database_table = resultDatabaseSettings$databaseTable
  )
  
  shiny::incProgress(3/3, detail = paste("Finished"))
  
  })
  
  #write.csv(data,'/Users/jreps/Documents/tte_data.csv')
  
  return(data)
}

plotTimeToEvent <- function(
  timeToEventData,
  databases,
  times,
  outcomeTypes,
  targetOutcomeTypes
){
  
  if(is.null(timeToEventData())){
    return(NULL)
  }
  
  timeToEventData <- timeToEventData() %>% 
    dplyr::filter(.data$databaseName %in% databases)
  
  if(nrow(timeToEventData) == 0){
    shiny::showNotification('No results for selected databases')
    return(NULL)
  }
  
  timeToEventData <- timeToEventData %>% 
    dplyr::filter(.data$timeScale %in% times)
  
  if(nrow(timeToEventData) == 0){
    shiny::showNotification('No results for selected databases and times')
    return(NULL)
  }
  
  timeToEventData <- timeToEventData %>% 
    dplyr::filter(
      .data$outcomeType %in% outcomeTypes &
      .data$targetOutcomeType %in% targetOutcomeTypes
      )
  
  if(nrow(timeToEventData) == 0){
    shiny::showNotification('No results for selection')
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
      .data$timeScale ~ .data$databaseName , scales = 'free'
        ) +
    ggplot2::theme_minimal() + 
    ggplot2::guides(fill=ggplot2::guide_legend(title="Outcome Type")) + 
    ggplot2::labs(y= "# of Events", x = "Time (days) to Event")
  
  shiny::incProgress(2/2, detail = paste("Finished"))
  
  })
  
  
    return(plot)
}
