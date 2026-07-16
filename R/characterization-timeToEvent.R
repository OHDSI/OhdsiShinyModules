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
    shiny::tags$style(
      '
      .tte-viewer-shell {
        display: grid;
        gap: 16px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
        box-sizing: border-box;
        overflow-x: hidden;
      }
      .tte-hero {
        border-radius: 22px;
        padding: 22px 24px;
        background: linear-gradient(135deg, #f8fbff 0%, #eef6ff 45%, #fdfcff 100%);
        border: 1px solid #dbe6f3;
        box-shadow: 0 14px 30px rgba(15, 23, 42, 0.08);
      }
      .tte-hero-top {
        display: flex;
        align-items: center;
        gap: 14px;
        margin-bottom: 8px;
      }
      .tte-hero-icon {
        width: 52px;
        height: 52px;
        border-radius: 16px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: #ffffff;
        background: linear-gradient(135deg, #f59e0b, #fb923c);
        box-shadow: 0 12px 20px rgba(245, 158, 11, 0.24);
        flex: 0 0 auto;
      }
      .tte-hero-title {
        font-size: 24px;
        font-weight: 800;
        letter-spacing: -0.02em;
        color: #102033;
        margin: 0;
      }
      .tte-hero-copy {
        color: #526173;
        margin: 0;
        line-height: 1.5;
        max-width: 880px;
      }
      .tte-options-box.box {
        border-radius: 18px;
        border-top: 4px solid #2563eb;
        box-shadow: 0 12px 26px rgba(15, 23, 42, 0.06);
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      .tte-results-card {
        border-radius: 20px;
        overflow: hidden;
        box-shadow: 0 16px 32px rgba(15, 23, 42, 0.08);
        border: 1px solid #dbe5f1;
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      .tte-tabs,
      .tte-tabs .tab-content,
      .tte-tabs .tab-pane {
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      .tte-tabs .nav-tabs,
      .tte-tabs .nav-pills {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        border-bottom: none;
      }
      .tte-tabs .nav > li {
        float: none;
        margin: 0;
      }
      .tte-results-card .box-header {
        background: linear-gradient(135deg, #123a63 0%, #1d4ed8 100%);
        color: #ffffff;
        border-bottom: none;
      }
      .tte-results-card .box-title {
        font-weight: 700;
      }
      .tte-tabs .nav > li > a {
        border-radius: 999px;
        padding: 10px 16px;
        font-weight: 700;
        white-space: nowrap;
      }
      .tte-tabs .nav-pills > li.active > a,
      .tte-tabs .nav-pills > li.active > a:focus,
      .tte-tabs .nav-pills > li.active > a:hover {
        background: linear-gradient(135deg, #2563eb 0%, #7c3aed 100%);
        box-shadow: 0 10px 18px rgba(37, 99, 235, 0.22);
      }
      .tte-tab-panel-wrap {
        margin-top: 14px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      .tte-filters-card {
        background: #f8fbff;
        border: 1px solid #dde7f2;
        border-radius: 16px;
        padding: 14px 16px 6px 16px;
        margin-bottom: 16px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      .tte-options-box .box-body,
      .tte-results-card .box-body {
        width: 100%;
        max-width: 100%;
        min-width: 0;
      }
      '
    ),
    shiny::div(
      class = 'tte-viewer-shell',
      shiny::div(
        class = 'tte-hero',
        shiny::div(
          class = 'tte-hero-top',
          shiny::div(
            class = 'tte-hero-icon',
            shiny::icon('clock')
          ),
          shiny::div(
            shiny::tags$h2(class = 'tte-hero-title', 'Time-to-event'),
            shiny::tags$p(
              class = 'tte-hero-copy',
              'Explore when outcomes occur relative to the target index date, compare timing patterns across databases, and review the underlying counts in a polished, easy-to-scan view.'
            )
          )
        )
      ),
      shinydashboard::box(
        collapsible = TRUE,
        title = shiny::tagList(shiny::icon('sliders'), 'Analysis options'),
        width = '100%',
        class = 'tte-options-box',
        shiny::div(
          class = 'tte-filters-card',
          shiny::uiOutput(ns('inputs'))
        )
      ),
      shiny::conditionalPanel(
        condition = 'output.showTimeToEvent != 0',
        ns = ns,
        shiny::div(
          class = 'tte-tabs',
          shiny::tabsetPanel(
            type = 'pills',
            id = ns('tteMainPanel'),
            shiny::tabPanel(
              title = 'Time-to-event Plots',
              shiny::div(
                class = 'tte-tab-panel-wrap',
                shinydashboard::box(
                  width = '100%',
                  class = 'tte-results-card',
                  title = '',
                  shiny::uiOutput(ns('timeToEventPlotInputs')),
                  shinycssloaders::withSpinner(
                    shiny::plotOutput(ns('timeToEvent'))
                  )
                )
              )
            ),
            shiny::tabPanel(
              title = 'Time-to-event Table',
              shiny::div(
                class = 'tte-tab-panel-wrap',
                shinydashboard::box(
                  class = 'tte-results-card',
                  status = 'info',
                  width = '100%',
                  solidHeader = TRUE,
                  resultTableViewer(ns('tableResults'))
                )
              )
            )
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
  reactiveCharacterizationTargetTable,
  reactiveOutcomeTable
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # moving the selections within module rather than shared across
      reactiveOutcomeRowId <- shiny::reactiveVal(NULL)
      reactiveCharacterizationTargetRowId <- shiny::reactiveVal(NULL)
      
      # restrict to populations with cohort comp data
      moduleCharacterizationTargetTable <- shiny::reactive({
        if(!is.null(reactiveCharacterizationTargetTable())){
          reactiveCharacterizationTargetTable() %>%
            dplyr::filter(.data$timeToEvent == 1)
        } else{
          NULL
        }
      })
      
      reactiveTargetRow <- shiny::reactive({
        rowId <- reactiveCharacterizationTargetRowId()
        targetTable <- moduleCharacterizationTargetTable()
        
        if (is.null(rowId) || length(rowId) == 0 || is.null(targetTable) || nrow(targetTable) == 0) {
          return(data.frame())
        }
        
        targetTable[rowId, , drop = FALSE]
      })
      
      tableSelectionServer(
        id = 'char-pop-select-tte',
        table = moduleCharacterizationTargetTable, 
        selectedRowId = reactiveCharacterizationTargetRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('table-selector-tte'),
        inputColumns = characterizationTargetsColumns(),
        displayColumns = characterizationTargetsColumns(), 
        selectButtonText = 'Select Population'
      )
      
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
        targetRowId <- reactiveCharacterizationTargetRowId()
        outcomeRowId <- reactiveOutcomeRowId()
        outcomeTable <- reactiveOutcomeTable()

        hasTarget <- !is.null(targetRowId) && length(targetRowId) > 0 && all(targetRowId > 0)
        hasOutcome <- !is.null(outcomeRowId) && length(outcomeRowId) > 0 &&
          all(outcomeRowId > 0) && !is.null(outcomeTable) &&
          nrow(outcomeTable) >= max(outcomeRowId)
        canGenerate <- hasTarget && hasOutcome
        
        shiny::div(
          
          tableSelectionViewer(id = session$ns('char-pop-select-tte')),

          if (hasTarget) {
            tableSelectionViewer(id = session$ns('outcome-table-select-tte'))
          },

          shiny::tags$button(
            id = session$ns('generate'),
            type = 'button',
            class = if (canGenerate) 'btn btn-primary action-button' else 'btn btn-default action-button',
            disabled = if (!canGenerate) 'disabled' else NULL,
            'Generate'
          ),

          if (!canGenerate) {
            shiny::helpText('Select both a population and an outcome to enable Generate.')
          }
        )
      })
      
      
      # server for outcome seleciton table
      tableSelectionServer(
        id = 'outcome-table-select-tte',
        table = reactiveOutcomeTable, 
        selectedRowId = reactiveOutcomeRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('table-outcome-selector'),
        inputColumns = characterizationOutcomeDisplayColumns(),
        displayColumns = characterizationOutcomeDisplayColumns(), 
        selectButtonText = 'Select Outcome'
      )
  
      
      # wait for generate to extract data
      shiny::observeEvent(input$generate, {
        
        reactiveOutcomeRow <- reactiveOutcomeTable()[reactiveOutcomeRowId(),]
        
        if(is.null(reactiveTargetRow()) | is.null(reactiveOutcomeRow)){
          output$showTimeToEvent <- shiny::reactive(0)
          allData(NULL)
          return(NULL)
        } else{
          
          if(nrow(reactiveTargetRow()) > 0 & nrow(reactiveOutcomeRow) > 0 ){
            
            # add code to show T and O selected 
            
            output$showTimeToEvent <- shiny::reactive(1)
            
            allData(getTimeToEventData(
              characterizationTargetId = reactiveTargetRow()$characterizationTargetId,
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
              colDefsInput = characterizationTimeToEventColDefs(),
              elementId = session$ns('tte-main')
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
                  multiple = FALSE,
                  choices =  unique(allData()$timeScale),
                  selected =  unique(allData()$timeScale)[1]
                )
              ),
              
              shiny::column(
                width = 3,
                shiny::checkboxInput(
                  inputId = session$ns("colorByOutcomeTypes"),
                  label = "Color by outcome occurrence type",
                  value = FALSE
                )
              ),
              
              shiny::column(
                width = 3,
                shiny::checkboxInput(
                  inputId = session$ns("colorByTargetOutcomeTypes"),
                  label = "Color by timing of outcome",
                  value = FALSE
                )
              ),

              shiny::column(
                width = 3,
                shiny::checkboxInput(
                  inputId = session$ns("freeYByDatabase"),
                  label = "Free y-axis by database",
                  value = TRUE
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
            colorByOutcomeTypes = input$colorByOutcomeTypes,
            colorByTargetOutcomeTypes = input$colorByTargetOutcomeTypes,
            freeYByDatabase = input$freeYByDatabase
          )
        )
    
      
      return(invisible(NULL))
      
    }
  )
}

# pulls all data for a target and outcome
getTimeToEventData <- function(
    characterizationTargetId,
  outcomeId,
  connectionHandler,
  resultDatabaseSettings
){
  if(is.null(characterizationTargetId)){
    return(NULL)
  }
  
  shiny::withProgress(message = 'Extracting time to event data', value = 0, {
    
  shiny::incProgress(1/3, detail = paste("Fetching data"))
  
    data <- OhdsiReportGenerator::getTimeToEvent(
      connectionHandler = connectionHandler, 
      schema = resultDatabaseSettings$schema, 
      cTablePrefix = resultDatabaseSettings$cTablePrefix, 
      cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
      databaseTable = resultDatabaseSettings$databaseTable, 
      characterizationTargetIds = characterizationTargetId, 
      outcomeIds = outcomeId
    ) 
  
  shiny::incProgress(3/3, detail = paste("Finished"))
  
  })

  return(data)
}

plotTimeToEvent <- function(
  timeToEventData,
  databases,
  times,
  colorByOutcomeTypes,
  colorByTargetOutcomeTypes,
  freeYByDatabase
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
    dplyr::filter(.data$numEvents > 0)
  
  # TODO plot censored as black?
  
  if(nrow(timeToEventData) == 0){
    shiny::showNotification('No results for selection')
    return(NULL)
  }
  
  nDatabases <- length(unique(timeToEventData$databaseId))
  facetScales <- if (isTRUE(freeYByDatabase)) "free_y" else "fixed"
  
  shiny::withProgress(message = 'Plotting time to event', value = 0, {
  
  shiny::incProgress(1/2, detail = paste("Generating plot"))

  fillGroup <- rep("All events", nrow(timeToEventData))
  if (isTRUE(colorByOutcomeTypes) && isTRUE(colorByTargetOutcomeTypes)) {
    fillGroup <- paste0(timeToEventData$outcomeType, " - ", timeToEventData$targetOutcomeType)
  } else if (isTRUE(colorByOutcomeTypes)) {
    fillGroup <- timeToEventData$outcomeType
  } else if (isTRUE(colorByTargetOutcomeTypes)) {
    fillGroup <- timeToEventData$targetOutcomeType
  }

  timeToEventData$fillGroup <- fillGroup

  legendTitle <- ""
  if (isTRUE(colorByOutcomeTypes) && isTRUE(colorByTargetOutcomeTypes)) {
    legendTitle <- "Outcome Type + Timing"
  } else if (isTRUE(colorByOutcomeTypes)) {
    legendTitle <- "Outcome Type"
  } else if (isTRUE(colorByTargetOutcomeTypes)) {
    legendTitle <- "Timing of Outcome"
  }
  
  plot <- ggplot2::ggplot(
    data = timeToEventData,
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
      .data$timeScale ~ .data$databaseName , scales = facetScales
        ) +
    ggplot2::theme_minimal() + 
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::labs(y= "# of Events", x = "Time (days) to Event") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  if (isTRUE(colorByOutcomeTypes) || isTRUE(colorByTargetOutcomeTypes)) {
    plot <- plot + ggplot2::guides(fill = ggplot2::guide_legend(title = legendTitle))
  } else {
    plot <- plot +
      ggplot2::scale_fill_manual(values = c("All events" = "black"), guide = "none")
  }
  
  shiny::incProgress(2/2, detail = paste("Finished"))
  
  })
  
  
    return(plot)
}


characterizationTimeToEventColDefs <- function(){
  result <- list(
    databaseName = reactable::colDef(
      name = "Database",
      header = withTooltip("Database",
                           "Name of the database"),
      filterable = TRUE
    ),
    databaseId = reactable::colDef(
      show = FALSE
    ),
    targetId = reactable::colDef(
      show = FALSE
    ),
    targetName = reactable::colDef(
      name = "Target Name",
      minWidth = 300,
      header = withTooltip("Target Name",
                           "Name of the target cohort"),
      filterable = TRUE
    ),
    outcomeId = reactable::colDef(
      show = FALSE
    ),
    outcomeName = reactable::colDef(
      name = "Outcome Name",
      header = withTooltip("Outcome Name",
                           "Name of the outcome cohort"),
      filterable = TRUE
    ),
    outcomeType = reactable::colDef(
      name = "Outcome Type",
      header = withTooltip("Outcome Type",
                           "Type of the outcome, either first or subsequent occurrence"),
      filterable = TRUE
    ),
    targetOutcomeType = reactable::colDef(
      name = "Target-Outcome Type",
      header = withTooltip("Target-Outcome Type",
                           "The timing of the event relative to the target era"),
      filterable = TRUE
    ),
    timeToEvent = reactable::colDef(
      name = "Time (in days) To Event",
      header = withTooltip("Time (in days) To Event",
                           "The time in days relative to target index until the event occurred"),
      filterable = TRUE
    ),
    numEvents = reactable::colDef(
      name = "# of Events",
      header = withTooltip("# of Events",
                           "The number of events that occurred"),
      filterable = TRUE,
      cell = function(value) {
        # Add < if cencored
        if (value < 0 ) paste("<", abs(value)) else value
      }
    ),
    timeScale = reactable::colDef(
      name = "Time Scale",
      header = withTooltip("Time Scale",
                           "The time scale in which the events occurred"),
      filterable = TRUE
    )
  )
  return(result)
}
