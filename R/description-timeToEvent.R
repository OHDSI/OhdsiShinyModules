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
    
    shinydashboard::box(
      collapsible = TRUE,
      collapsed = TRUE,
      title = "Time-to-events",
      width = "100%"#,
      #shiny::htmlTemplate(system.file("description-www", "help-timeToEvent.html", package = utils::packageName()))
    ),
    
    shinydashboard::box(
      width = "100%",
      title = 'Options',
      collapsible = TRUE,
      collapsed = F,
      shiny::uiOutput(ns('timeToEventInputs'))
    ),
    
    
    shiny::conditionalPanel(
      condition = "input.generate != 0",
      ns = ns,
      
      shiny::uiOutput(ns("TTEinputsText")),
      
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
#' @param mainPanelTab the current tab 
#' @param schema the database schema for the model results
#' @param tablePrefix a string that appends the tables in the result schema
#' @param cohortTablePrefix a string that appends the cohort table in the result schema
#' @param databaseTable  name of the database table
#' 
#' @return
#' The server to the prediction time to event module
#'
#' @export
descriptionTimeToEventServer <- function(
  id, 
  connectionHandler,
  mainPanelTab,
  schema, 
  tablePrefix,
  cohortTablePrefix = 'cg_',
  databaseTable = 'DATABASE_META_DATA'
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      #if(mainPanelTab() != 'Time To Event'){
      #  return(invisible(NULL))
      #}
      
      # get the possible target ids
      bothIds <- timeToEventGetIds(
        connectionHandler,
        schema, 
        tablePrefix,
        cohortTablePrefix
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
            
            shiny::column(
              width = 6,
              shinyWidgets::pickerInput(
                inputId = session$ns('targetId'), 
                label = 'Target id: ', 
                choices = bothIds$targetIds, 
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
                choices = bothIds$outcomeIds[[1]],
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
              ),
            
            shiny::actionButton(
              inputId = session$ns('generate'),
              label = 'Generate Report'
            )
          )
        )
      })
      
      
      allData <- shiny::reactiveVal(NULL)
      databaseNames <- shiny::reactiveVal(c('none'))
      timespans <- shiny::reactiveVal(c('none'))
      
      selectedInputs <- shiny::reactiveVal()
      output$TTEinputsText <- shiny::renderUI(selectedInputs())
      
      
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
                    names(bothIds$targetIds)[bothIds$targetIds == input$targetId]
                  ),
                  shiny::column(
                    width = 6,
                    shiny::tags$b("Outcome:"),
                    names(bothIds$outcomeIds[[1]])[bothIds$outcomeIds[[1]] == input$outcomeId]
                  )
                )
                
              )
            )
          )
          
          tempData <- tryCatch({
            getTimeToEventData(
              targetId = input$targetId,
              outcomeId = input$outcomeId,
              connectionHandler = connectionHandler,
              schema = schema, 
              tablePrefix = tablePrefix,
              databaseTable = databaseTable
            )
          }, 
          error = function(e){shiny::showNotification(paste0('Error: ', e));return(NULL)}
          )
          
          if(is.null(tempData)){
            shiny::showNotification('No data...')
          } else{
            shiny::showNotification(paste0('Data with ', nrow(tempData),' rows returned'))
          }
          
          allData(tempData)
          databaseNames(unique(tempData$databaseName))  
          timespans(unique(tempData$timeScale))  
          
        }
      )
      
      output$timeToEventPlotInputs <- shiny::renderUI({
        
        shiny::fluidPage(
          shiny::fluidRow(
            
            shiny::checkboxGroupInput(
              inputId = session$ns("databases"), 
              label = "Databases:",
              choiceNames = databaseNames(), 
              choiceValues = databaseNames(),
              selected = databaseNames()
            ),
            shiny::checkboxGroupInput(
              inputId = session$ns("times"), 
              label = "Timespan:",
              choiceNames = timespans(), 
              choiceValues = timespans(),
              selected = timespans()
            )
            
          )
        )
      }
      )
      
      output$timeToEvent <- shiny::renderPlot(
          plotTimeToEvent(
            timeToEventData = allData,
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
  schema, 
  tablePrefix,
  cohortTablePrefix
){
  
  shiny::withProgress(message = 'Getting time to event T and O ids', value = 0, {
  
  sql <- "SELECT DISTINCT 
     t.COHORT_NAME as target, TARGET_COHORT_DEFINITION_ID, 
     o.COHORT_NAME as outcome, OUTCOME_COHORT_DEFINITION_ID 
  FROM @result_database_schema.@table_prefixTIME_TO_EVENT tte
 inner join @result_database_schema.@cohort_table_prefixCOHORT_DEFINITION t
          on tte.TARGET_COHORT_DEFINITION_ID = t.COHORT_DEFINITION_ID
   inner join @result_database_schema.@cohort_table_prefixCOHORT_DEFINITION o
          on tte.OUTCOME_COHORT_DEFINITION_ID = o.COHORT_DEFINITION_ID
  ;"


  shiny::incProgress(1/4, detail = paste("Fetching ids"))
  
  bothIds <- connectionHandler$queryDb(
    sql = sql, 
    result_database_schema = schema,
    table_prefix = tablePrefix,
    cohort_table_prefix = cohortTablePrefix
  )
  
  shiny::incProgress(3/4, detail = paste("Processing ids"))
  
  targetUnique <- bothIds %>% 
    dplyr::select(c("targetCohortDefinitionId", "target")) %>%
    dplyr::distinct()
  
  targetIds <- targetUnique$targetCohortDefinitionId
  names(targetIds) <- targetUnique$target
  
  outcomeIds <- lapply(targetIds, function(x){
    
    outcomeUnique <- bothIds %>% 
      dplyr::filter(.data$targetCohortDefinitionId == x) %>%
      dplyr::select(c("outcomeCohortDefinitionId", "outcome")) %>%
      dplyr::distinct()
    
    outcomeIds <- outcomeUnique$outcomeCohortDefinitionId
    names(outcomeIds) <- outcomeUnique$outcome
    
    return(outcomeIds)
    
    })
  
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
  connectionHandler,
  schema, 
  tablePrefix,
  databaseTable
){
  
  
  shiny::withProgress(message = 'Extracting time to event data', value = 0, {
  
  sql <- "SELECT tte.*, d.CDM_SOURCE_ABBREVIATION as database_name 
          FROM @result_database_schema.@table_prefixTIME_TO_EVENT tte
          inner join @result_database_schema.@database_table d
          on tte.database_id = d.database_id
          where tte.TARGET_COHORT_DEFINITION_ID = @target_id
          and tte.OUTCOME_COHORT_DEFINITION_ID = @outcome_id;"

  shiny::incProgress(1/3, detail = paste("Fetching data"))
  
  data <- connectionHandler$queryDb(
    sql = sql, 
    result_database_schema = schema,
    table_prefix = tablePrefix,
    target_id = targetId,
    outcome_id = outcomeId,
    database_table = databaseTable
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
