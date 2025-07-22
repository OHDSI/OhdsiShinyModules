# @file characterization-timeToEvent.R
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


characterizationTimeToEventViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    
    shiny::uiOutput(ns("inputs")),
    
    shiny::conditionalPanel(
      condition = 'output.showTimeToEvent != 0', 
      ns = ns,
      
      inputSelectionDfViewer(id = ns('inputSelected'), title = 'Selected'),
      
      shiny::tabsetPanel(
        type = 'pills',
        id = ns('tteMainPanel'),
        
        shiny::tabPanel(
          title = "Time-to-event Plots",
          
          shinydashboard::box(
            width = "100%",
            title = "",
            
            shiny::uiOutput(ns('timeToEventPlotInputs')),
            shinycssloaders::withSpinner(
              shiny::plotOutput(ns('timeToEvent'))
            )
          )
        ),
        
        shiny::tabPanel(
          title = "Time-to-event Table",
          
          shinydashboard::box(
            status = 'info', 
            width = '100%',
            solidHeader = TRUE,
            resultTableViewer(ns('tableResults'))
          )
        )
      )
    )
  )
}


characterizationTimeToEventServer <- function(
  id, 
  connectionHandler,
  resultDatabaseSettings,
  reactiveTargetRow,
  outcomeTable,
  reactiveOutcomeRowId,
  outcomeRow
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$showTimeToEvent <- shiny::reactive(0)
      shiny::outputOptions(output, "showTimeToEvent", suspendWhenHidden = FALSE)
      allData <- shiny::reactiveVal(NULL)
      
      # if target or outcome changes hide results
      shiny::observeEvent(reactiveTargetRow(), {
        output$showTimeToEvent <- shiny::reactive(0)
      })
      shiny::observeEvent(reactiveOutcomeRowId(), {
        output$showTimeToEvent <- shiny::reactive(0)
      })
      
      # inputs
      output$inputs <- shiny::renderUI({ # need to make reactive?
        
        shiny::div(
          shiny::helpText('View the timing of all outcomes relative to the target index date and whether the outcome was the frist or subsequent.'),
          
          tableSelectionViewer(id = session$ns('outcome-table-select-tte')),

          shiny::actionButton(
            inputId = session$ns('generate'), 
            label = 'Generate'
            ),
        )})
      
      
      # server for outcome seleciton table
      tableSelectionServer(
        id = 'outcome-table-select-tte',
        table = shiny::reactive(outcomeTable() %>%
                                  dplyr::select("parentName", "cohortName", "cohortId") %>%
                                  dplyr::relocate("parentName", .before = "cohortName") %>%
                                  dplyr::relocate("cohortId", .after = "cohortName")
        ), 
        selectedRowId = reactiveOutcomeRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('table-outcome-selector'),
        inputColumns = characterizationOutcomeDisplayColumns(),
        displayColumns = characterizationOutcomeDisplayColumns(), 
        selectButtonText = 'Select Outcome'
      )
      
      # show selected inputs to user
      selected <- shiny::reactiveVal()
      inputSelectionDfServer(
        id = 'inputSelected', 
        dataFrameRow = selected,
        ncol = 1
      )
      
      # wait for generate to extract data
      shiny::observeEvent(input$generate, {
        
        reactiveOutcomeRow <- outcomeTable()[reactiveOutcomeRowId(),]
        
        if(is.null(reactiveTargetRow()) | is.null(reactiveOutcomeRow)){
          output$showTimeToEvent <- shiny::reactive(0)
          allData(NULL)
          return(NULL)
        } else{
          
          if(nrow(reactiveTargetRow()) > 0 & nrow(reactiveOutcomeRow) > 0 ){
            
            selected(
              data.frame(
                Target = reactiveTargetRow()$cohortName,
                Outcome = reactiveOutcomeRow$cohortName
              )
            )
            
            # add code to show T and O selected 
            
            output$showTimeToEvent <- shiny::reactive(1)
            
            allData(getTimeToEventData(
              targetId = reactiveTargetRow()$cohortId,
              outcomeId = reactiveOutcomeRow$cohortId,
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings
            ) %>%
              dplyr::mutate(targetName = reactiveTargetRow()$cohortName,
                            outcomeName = reactiveOutcomeRow$cohortName) %>%
              dplyr::relocate("databaseName", .before = "databaseId") %>%
              dplyr::relocate("targetName", .after = "databaseName") %>%
              dplyr::relocate("outcomeName", .after = "targetName")
            )
            
            # make details reactive and this can move outside the observe
            resultTableServer(
              id = "tableResults", 
              df = allData,
              details = data.frame( # PROBLEM this is not reactive
                target = reactiveTargetRow()$cohortName,
                outcome = reactiveOutcomeRow$cohortName,
                Analysis = 'Exposed Cases Summary - Time-to-event'
              ),
              downloadedFileName = 'time_to_event',
              colDefsInput = characterizationTimeToEventColDefs()
            )
            
          } else{
            shiny::showNotification('Must have target and outcome set')
          }
        }
      }
      )
      
      
      output$timeToEventPlotInputs <- shiny::renderUI({
        
        shiny::fluidPage(
          shiny::fluidRow(
            
            shinyWidgets::pickerInput(
              inputId = session$ns("databases"),
              label = "Databases:",
              multiple = T, 
              choices = unique(allData()$databaseName),
              selected = unique(allData()$databaseName),
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                dropupAuto = TRUE,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
                )
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
            outcomeTypes = input$outcomeTypes,
            targetOutcomeTypes = input$targetOutcomeTypes
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
  
  # remove censored data
  timeToEventData <- timeToEventData %>% 
    dplyr::filter(
      .data$outcomeType %in% outcomeTypes &
      .data$targetOutcomeType %in% targetOutcomeTypes &
      .data$numEvents > 0
      )
  
  # TODO plot censored as black?
  
  if(nrow(timeToEventData) == 0){
    shiny::showNotification('No results for selection')
    return(NULL)
  }
  
  nDatabases <- length(unique(timeToEventData$databaseId))
  
  shiny::withProgress(message = 'Plotting time to event', value = 0, {
  
  shiny::incProgress(1/2, detail = paste("Generating plot"))
  
  plot <- ggplot2::ggplot(
    data = timeToEventData %>% 
      dplyr::mutate(
        fillGroup = paste0(.data$outcomeType, '-', .data$targetOutcomeType)
        ), 
    ggplot2::aes(
      x = .data$timeToEvent, 
      y = .data$numEvents,
      fill = .data$fillGroup,
      width = as.double(gsub('-day','',gsub('per ','',.data$timeScale)))
      )
    ) +
    ggplot2::geom_bar(
      stat = "identity"
      ) +
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


characterizationTimeToEventColDefs <- function(){
  result <- list(
    databaseName = reactable::colDef(
      header = withTooltip("Database",
                           "Name of the database"),
      filterable = T
    ),
    databaseId = reactable::colDef(
      header = withTooltip("Database ID",
                           "Unique ID of the database"),
      filterable = T,
      show = F
    ),
    targetCohortDefinitionId = reactable::colDef(
      header = withTooltip("Target ID",
                           "Unique ID of the target cohort"),
      filterable = T,
      show = F
    ),
    targetName = reactable::colDef(
      header = withTooltip("Target Name",
                           "Name of the target cohort"),
      filterable = T
    ),
    outcomeCohortDefinitionId = reactable::colDef(
      header = withTooltip("Outcome ID",
                           "Unique ID of the outcome cohort"),
      filterable = T,
      show = F
    ),
    outcomeName = reactable::colDef(
      header = withTooltip("Outcome Name",
                           "Name of the outcome cohort"),
      filterable = T
    ),
    outcomeType = reactable::colDef(
      header = withTooltip("Outcome Type",
                           "Type of the outcome, either first or subsequent occurrence"),
      filterable = T
    ),
    targetOutcomeType = reactable::colDef(
      header = withTooltip("Target-Outcome Type",
                           "The timing of the event relative to the target era"),
      filterable = T
    ),
    timeToEvent = reactable::colDef(
      header = withTooltip("Time (in days) To Event",
                           "The time in days relative to target index until the event occurred"),
      filterable = T
    ),
    numEvents = reactable::colDef(
      header = withTooltip("# of Events",
                           "The number of events that occurred"),
      filterable = T,
      cell = function(value) {
        # Add < if cencored
        if (value < 0 ) paste("<", abs(value)) else value
      }
    ),
    timeScale = reactable::colDef(
      header = withTooltip("Time Scale",
                           "The time scale in which the events occurred"),
      filterable = T
    )
  )
  return(result)
}
