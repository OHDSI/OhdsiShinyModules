# @file characterization-timeToEvent.R
#
# Copyright 2023 Observational Health Data Sciences and Informatics
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
    
    infoHelperViewer(
      id = "helper",
      helpLocation= system.file("characterization-www", "help-timeToEvent.html", package = utils::packageName())
    ),
    
    # input component module
    inputSelectionViewer(id = ns('input-selection')),
    
    shiny::conditionalPanel(
      condition = 'input.generate != 0',
      ns = shiny::NS(ns("input-selection")),
      
      shinydashboard::box(
        width = "100%",
        title = shiny::tagList(shiny::icon("gear"), "Results"),
        
        shiny::fluidRow(
          shiny::column(
            width = 2,
            shiny::uiOutput(ns('timeToEventPlotInputs'))
          ),
          shiny::column(
            width = 10,
            shinycssloaders::withSpinner(
              shiny::plotOutput(ns('timeToEvent'))
            )
          )
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
  resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # get the possible target ids
      bothIds <- timeToEventGetIds(
        connectionHandler,
        resultDatabaseSettings
      )

      
      # input selection component
      inputSelected <- inputSelectionServer(
        id = "input-selection", 
        inputSettingList = list(
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 6,
            varName = 'targetId',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Target: ',
              choices = bothIds$targetIds,
              #choicesOpt = list(style = rep_len("color: black;", 999)),
              selected = bothIds$targetIds[1],
              multiple = F,
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
          
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 6,
            varName = 'outcomeId',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Outcome: ',
              choices = bothIds$outcomeIds,
              #choicesOpt = list(style = rep_len("color: black;", 999)),
              selected = bothIds$outcomeIds[1],
              multiple = F,
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
        )
      )
      
      allData <- shiny::reactive({
        getTimeToEventData(
          targetId = inputSelected()$targetId,
          outcomeId = inputSelected()$outcomeId,
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings
        )
      })
      
      output$timeToEventPlotInputs <- shiny::renderUI({
        
        shiny::fluidPage(
          shiny::fluidRow(
            
            shiny::checkboxGroupInput(
              inputId = session$ns("databases"), 
              label = "Databases:",
              choiceNames = unique(allData()$databaseName), 
              choiceValues = unique(allData()$databaseName),
              selected = unique(allData()$databaseName)
            ),
            shiny::checkboxGroupInput(
              inputId = session$ns("times"), 
              label = "Timespan:",
              choiceNames = unique(allData()$timeScale), 
              choiceValues = unique(allData()$timeScale),
              selected = unique(allData()$timeScale)
            )
            
          )
        )
      }
      )
      
      output$timeToEvent <- shiny::renderPlot(
          plotTimeToEvent(
            timeToEventData = allData, # reactive
            databases = input$databases,
            times = input$times
          )
        )
    
      
      return(invisible(NULL))
      
    }
  )
}

timeToEventGetIds <- function(
    connectionHandler,
    resultDatabaseSettings
){
  
  shiny::withProgress(message = 'Getting time to event T and O ids', value = 0, {
  
  sql <- "SELECT DISTINCT 
     t.COHORT_NAME as target, TARGET_COHORT_DEFINITION_ID, 
     o.COHORT_NAME as outcome, OUTCOME_COHORT_DEFINITION_ID 
  FROM @schema.@c_table_prefixTIME_TO_EVENT tte
 inner join @schema.@cg_table_prefixCOHORT_DEFINITION t
          on tte.TARGET_COHORT_DEFINITION_ID = t.COHORT_DEFINITION_ID
   inner join @schema.@cg_table_prefixCOHORT_DEFINITION o
          on tte.OUTCOME_COHORT_DEFINITION_ID = o.COHORT_DEFINITION_ID
  ;"


  shiny::incProgress(1/4, detail = paste("Fetching ids"))
  
  bothIds <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix
  )
  
  shiny::incProgress(3/4, detail = paste("Processing ids"))
  
  targetUnique <- bothIds %>% 
    dplyr::select(c("targetCohortDefinitionId", "target")) %>%
    dplyr::distinct()
  
  targetIds <- targetUnique$targetCohortDefinitionId
  names(targetIds) <- targetUnique$target
  
  outcomeUnique <- bothIds %>% 
    dplyr::select(c("outcomeCohortDefinitionId", "outcome")) %>%
    dplyr::distinct()
  
  outcomeIds <- outcomeUnique$outcomeCohortDefinitionId
  names(outcomeIds) <- outcomeUnique$outcome
  
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
  
  return(data)
}

plotTimeToEvent <- function(
  timeToEventData,
  databases,
  times
){
  
  if(is.null(timeToEventData())){
    return(NULL)
  }
  
  timeToEventData <- timeToEventData() %>% 
    dplyr::filter(.data$databaseName %in% databases)
  
  if(is.null(timeToEventData)){
    return(NULL)
  }
  
  timeToEventData <- timeToEventData %>% 
    dplyr::filter(.data$timeScale %in% times)
  
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
      .data$timeScale ~ .data$databaseName , scales = 'free'
        ) +
    ggplot2::theme_minimal()
  
  shiny::incProgress(2/2, detail = paste("Finished"))
  
  })
  
  
    return(plot)
}
