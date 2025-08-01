# @file characterization-incidence.R
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


#' The module viewer for exploring incidence results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @family Characterization
#' @return
#' The user interface to the description incidence module
#'
#' @export
characterizationIncidenceViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    
    shiny::uiOutput(ns("inputOptions")),
    
    shiny::conditionalPanel(
      condition = 'output.showIncidence != 0',
      ns = ns,
      
      shiny::tabsetPanel(
        type = 'pills',
        id = ns('incMainPanel'),
        
        shiny::tabPanel(
          title = "Incidence Rate Table",
          shiny::uiOutput(ns("tableFilter")),
          
          shiny::conditionalPanel(
            condition = 'output.showTable != 0',
            ns = ns,
            resultTableViewer(
              ns("incidenceRateTable"),
              downloadedFileName = "incidenceRateTable-"
            )
          )
        ),
        shiny::tabPanel(
          title = "Incidence Rate Plots",
          
          shiny::uiOutput(ns("plotFilter")),
          
          shiny::conditionalPanel(
            condition = 'output.showPlot != 0',
            ns = ns,
            shinycssloaders::withSpinner(
              shiny::plotOutput(
                ns('incidencePlot'),
                width="100%",
                height="600px"
              )
            )
          ) # end condition panel
        ) # end plot
      )
      
    ) # conditional results panel
        
  )
}


#' The module server for exploring incidence results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler the connection to the prediction result database
#' @param resultDatabaseSettings a list containing the characterization result schema, dbms, tablePrefix, databaseTable and cgTablePrefix
#' @param reactiveTargetRow a reactive data.frame with the target of interest details
#' @param outcomeTable A reactive data.frame with the outcome table for the target of interest
#' @family Characterization
#' @return
#' The server to the prediction incidence module
#'
#' @export
characterizationIncidenceServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings,
    reactiveTargetRow, # reactive
    outcomeTable # reactive
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$showIncidence <- shiny::reactive(0)
      shiny::outputOptions(output, "showIncidence", suspendWhenHidden = FALSE)
      
      output$showTable <- shiny::reactive(0)
      shiny::outputOptions(output, "showTable", suspendWhenHidden = FALSE)
      
      output$showPlot <- shiny::reactive(0)
      shiny::outputOptions(output, "showPlot", suspendWhenHidden = FALSE)
      
      # if target changes hide results
      shiny::observeEvent(reactiveTargetRow(), {
        output$showIncidence <- shiny::reactive(0)
      })
      
      # get the databases that the target cohort has data in
      databaseNames <- shiny::reactive(unlist(strsplit(x = reactiveTargetRow()$databaseString, split = ', ')))
      databaseIds <- shiny::reactive(unlist(strsplit(x = reactiveTargetRow()$databaseIdString, split = ', ')))
      
      output$inputOptions <- shiny::renderUI({
        shinydashboard::box(
          collapsible = TRUE,
          title = "Options",
          width = "100%",
          
          tableSelectionViewer(id = session$ns('outcome-table-select')),
          
          shiny::actionButton(
            inputId = session$ns('generate'), 
            label = 'Generate',
            icon = shiny::icon('redo') 
          )
        )
      })
      
      output$tableFilter <- shiny::renderUI({
        
        shiny::div(
          shinyWidgets::pickerInput(
            inputId = session$ns('databaseSelector'),
            label = 'Filter By Database: ',
            choices = sort(databaseNames()),
            selected = databaseNames()[1],
            multiple = T,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              size = 10,
              dropupAuto = FALSE,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Type here to search",
              virtualScroll = 50
            )
          ),
          
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::checkboxInput(
                inputId = session$ns('ageStratify'), 
                label = 'include age stratified incidence', 
                value = FALSE
              )
            ),
            shiny::column(
              width = 3,
              shiny::checkboxInput(
                inputId = session$ns('sexStratify'), 
                label = 'include sex stratified incidence', 
                value = FALSE
              )
            ),
            
            shiny::column(
              width = 3,
              shiny::checkboxInput(
                inputId = session$ns('yearStratify'), 
                label = 'include index year stratified incidence', 
                value = FALSE
              )
            )
          ),
          
          shiny::actionButton(
            inputId = session$ns('generateTable'), 
            label = 'View Table'
            )
        )
        
      })
      
      reactiveOutcomeRowIds <- shiny::reactiveVal(NULL)
      reactiveOutcomeRows <- shiny::reactive({
        outcomeTable()[reactiveOutcomeRowIds(),]
      })
      
      tableSelectionServer(
        id = 'outcome-table-select',
        table = shiny::reactive(outcomeTable() %>%
                                  dplyr::filter(.data$cohortIncidence == 1) %>%
                                  dplyr::select('parentName','cohortName','cohortId') %>%
                                  dplyr::relocate("parentName", .before = "cohortName") %>%
                                  dplyr::relocate("cohortId", .after = "cohortName")
        ), 
        selectedRowId = reactiveOutcomeRowIds,
        selectMultiple = TRUE, 
        #elementId = session$ns('table-selector'),
        inputColumns = characterizationOutcomeDisplayColumns(),
        selectButtonText = 'Select Outcomes'
      )
      
      # hide results if outcome changes
      shiny::observeEvent(reactiveOutcomeRows(), {
        output$showIncidence <- shiny::reactive(0)
      })
      
      incidenceFullData <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(input$generate,{
        output$showIncidence <- shiny::reactive(1)
        output$showTable <- shiny::reactive(0)
        output$showPlot <- shiny::reactive(0)
        
        # TODO add input checks 
        if (is.null(reactiveTargetRow()) |
            is.null(reactiveOutcomeRows())
        ) {
          shiny::validate("Error with selection")
        } else if(nrow(reactiveOutcomeRows()) == 0){
          shiny::validate("Error with selection")
          } else {
          
          # TODO check nrow > 0 for t and o
          data <- getCharacterizationIncidence(
            connectionHandler = connectionHandler, 
            schema = resultDatabaseSettings$schema, 
            ciTablePrefix = resultDatabaseSettings$incidenceTablePrefix,
            cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
            databaseTable = resultDatabaseSettings$databaseTable, 
            targetIds = reactiveTargetRow()$cohortId, 
            outcomeIds = reactiveOutcomeRows()$cohortId
          ) 
        
          incidenceFullData(data)
          
        }
        
      })
      
      incidenceTableData <- shiny::reactiveVal(NULL)
      
  shiny::observeEvent(input$generateTable,{ 
    output$showTable <- shiny::reactive(1)
    
      # restrict to databases 
      data <- incidenceFullData() %>%
        dplyr::filter(.data$databaseId %in% databaseIds()[databaseNames() %in% input$databaseSelector])
     
      # ageGroupName genderName startYear
      if(!input$ageStratify){
        data <- data %>%
          dplyr::filter(.data$ageGroupName == 'Any')
      }
      if(!input$sexStratify){
        data <- data %>%
          dplyr::filter(.data$genderName == 'Any')
      }
      if(!input$yearStratify){
        data <- data %>%
          dplyr::filter(.data$startYear == 'Any')
      }
      
      incidenceTableData(data)
      
    })
      
      # SHOW TABLE
      resultTableServer(
        id = "incidenceRateTable",
        df = incidenceTableData,
        selectedCols = c("databaseName", 
                         "outcomeName", "tar",
                         "ageGroupName", "genderName", "startYear", 
                         "cleanWindow", "tar", 
                         "personsAtRisk","personDays","outcomes",
                         "incidenceProportionP100p", 
                         "incidenceRateP100py"),
        sortedCols = c("ageGroupName", "genderName", "startYear", "incidenceRateP100py"),
        colDefsInput = characterizationIncidenceColumnDefs(session$ns('incidence-table')), 
        elementId = session$ns('incidence-table'),
        downloadedFileName = "incidenceRateTable-"
      )
      
      
      
      # PLOT FILTER
      output$plotFilter <- shiny::renderUI({
        
        shiny::div(
          shinyWidgets::pickerInput(
            inputId = session$ns('databaseSelectorPlot'),
            label = 'Database: ',
            choices = sort(databaseNames()),
            selected = databaseNames(),
            multiple = T,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              size = 10,
              dropupAuto = FALSE,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Type here to search",
              virtualScroll = 50
            )
          ),
          
          shinyWidgets::pickerInput(
            inputId = session$ns('outcomesPlot'),
            label = 'Outcome: ',
            choices = sort(unique(reactiveOutcomeRows()$cohortName)),
            selected = sort(unique(reactiveOutcomeRows()$cohortName)),
            multiple = T,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              size = 10,
              dropupAuto = FALSE,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Type here to search",
              virtualScroll = 50
            )
          ),
          
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::selectInput(
                inputId = session$ns('xAxis'), 
                label = 'Report Type:', 
                choices = c('Age', 'Year'), 
                selected = 'Age', 
                multiple = FALSE
                  )
            ),
            shiny::column(
              width = 4,
              shiny::checkboxInput(
                inputId = session$ns('sexStratifyPlot'), 
                label = 'sex stratify incidence', 
                value = FALSE
              )
            ),
            shiny::column(
              width = 4,
              shiny::checkboxInput(
                inputId = session$ns('scaleVal'), 
                label = 'Fixed y-scale', 
                value = TRUE
              )
            )
            
            #  shiny::column(width = 3,
            #                shiny::div(
            #                  style = "display:inline-block; float:right",
            #                  shiny::downloadButton(ns("downloadPlotStandardAge"),
            #                                        "Download Plot",
            #                                        icon = shiny::icon("download")
            #                  )
            #                )
            #  ) 
            
          ),
          
          shiny::actionButton(
            inputId = session$ns('generatePlot'), 
            label = 'View Plot'
          )
        )
        
      })
      
      # plot code
      shiny::observeEvent(input$generatePlot ,{
        output$showPlot <- shiny::reactive(1)
        
        plotData <- incidenceFullData() 
        
        # create logic to format data based on selected stratification
        xAxis <- 'startYear'
        xName <- 'Year'
        color <- 'databaseName'
        colorName <- "Data Source"
        
        scaleVal <- ifelse(input$scaleVal,'fixed','free_y')
        
        if(input$xAxis == 'Age'){
          plotData <- plotData %>% 
            dplyr::filter(.data$ageGroupName != 'Any' &
                          .data$startYear == 'Any')
          xAxis <- 'ageGroupName'
          xName <- 'Age'
        } else{
          plotData <- plotData %>% 
            dplyr::filter(.data$ageGroupName == 'Any' &
                            .data$startYear != 'Any')
        }
        
        if(!input$sexStratifyPlot){
          plotData <- plotData %>% 
            dplyr::filter(.data$genderName == 'Any')
        } else{
          plotData <- plotData %>% 
            dplyr::filter(.data$genderName != 'Any')
          color <- 'genderName'
          colorName <- "Sex"
        }
        

        plotData <- plotData %>% 
          dplyr::filter(
            .data$databaseName %in% input$databaseSelectorPlot
          ) %>%
          dplyr::filter(
            .data$outcomeName %in% input$outcomesPlot
          )
        
        if(nrow(plotData) > 0){
          
          output$incidencePlot <- shiny::renderPlot(
            ggplot2::ggplot(
              data = plotData  %>% 
                dplyr::select("databaseName", "outcomeName","ageGroupName","startYear","genderName","incidenceRateP100py", "tar", "cleanWindow"),
              mapping = ggplot2::aes(x = .data[[xAxis]],
                                     y = .data$incidenceRateP100py,
                                     color = .data[[color]],
                                     group = .data[[color]]
              )
            ) + 
              ggplot2::geom_point(
                size = 3
              ) + 
              ggplot2::geom_line() +
              ggplot2::facet_grid(
                .data$databaseName ~ paste0(.data$outcomeName," (clean win ",.data$cleanWindow,"): ",.data$tar),
                #. ~ paste0(.data$outcomeName," (clean win ",.data$cleanWindow,"): ",.data$tar),
                scales = scaleVal
                ) +
              ggplot2::scale_y_continuous(
                trans = scales::pseudo_log_trans(base = 10),
                n.breaks = 4
              ) + 
              ggplot2::labs(
                title = "Incidence Rates",
                x = xName,
                y = "Incidence rate per 100 patient years",
                color = colorName
              )  +
              ggplot2::theme(
                #strip.text.y = ggplot2::element_text(angle = 0, hjust = 0, vjust = 0),
                legend.position="bottom",
                strip.text.y = ggplot2::element_text(angle=0)
              )
          )
        } else{
          output$incidencePlot <- NULL
        }
        
      })
      
      
      
      return(invisible(NULL)) ############# end of server
      
    })
}

characterizationIncidenceColumnDefs <- function(elementId){
  
  list(
    databaseName = reactable::colDef(
      header = withTooltip("Database",
                           "The database name")
    ), 
    targetName = reactable::colDef(
      header = withTooltip("Target",
                           "The target cohort name")
    ), 
    targetId = reactable::colDef(
      header = withTooltip("Target ID",
                           "The target cohort unique identifier")
    ),
    outcomeName = reactable::colDef(
      header = withTooltip("Outcome",
                           "The outcome name"),
      filterable = TRUE,
      filterInput = function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", elementId, name),
          # "All" has an empty value to clear the filter, and is the default option
          shiny::tags$option(value = "", "All"),
          lapply(unique(values), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    ), 
    outcomeId = reactable::colDef(
      header = withTooltip("Outcome ID",
                           "The outcome cohort unique identifier")
    ),
    tar = reactable::colDef(
      header = withTooltip("TAR",
                           "Time-at-risk"),
      filterable = TRUE,
      filterInput = function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", elementId, name),
          # "All" has an empty value to clear the filter, and is the default option
          shiny::tags$option(value = "", "All"),
          lapply(unique(values), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    ), 
    ageGroupName = reactable::colDef(
      header = withTooltip("Age Group",
                           "The age group when stratifying by age")
    ), 
    genderName = reactable::colDef(
      header = withTooltip("Sex",
                           "The sex when stratifying by sex")
    ), 
    startYear = reactable::colDef(
      header = withTooltip("Index Year",
                           "The index year when stratifying by year")
    ), 
    cleanWindow = reactable::colDef(
      header = withTooltip("Clean Window",
                           "The time in days after an outcome that is ignored in the rate calculation"),
      filterable = TRUE,
      filterInput = function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", elementId, name),
          # "All" has an empty value to clear the filter, and is the default option
          shiny::tags$option(value = "", "All"),
          lapply(unique(values), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    ), 
    tar = reactable::colDef(
      header = withTooltip("Time-at-risk",
                           "The time interval where a patient is at risk used to calculate the incidence")
    ), 
    personsAtRisk = reactable::colDef(
      filterable = FALSE,
      header = withTooltip("No. Persons",
                           "The number of people at risk")
    ),
    personDays = reactable::colDef(
      filterable = FALSE,
      header = withTooltip("Person Days",
                           "The total number of days at risk for all the people at risk")
    ),
    outcomes = reactable::colDef(
      filterable = FALSE,
      header = withTooltip("No. Outcomes",
                           "The number of outcomes within the people at risk during time-at-risk")
    ),
    incidenceProportionP100p = reactable::colDef(
      filterable = FALSE,
      header = withTooltip("Incidence proportion per 100 persons",
                           "The number of outcomes divided by the number of patients multipled by 100"),
      format = reactable::colFormat(digits = 2)
    ), 
    incidenceRateP100py = reactable::colDef(
      filterable = FALSE,
      header = withTooltip("Incidence rate per 100 person years",
                           "The number of outcomes divided by the number of person days exposed multipled by 100 times 365"), 
      format = reactable::colFormat(digits = 2)
    )
  )
  
}



getCharacterizationIncidence <- function(
    connectionHandler, 
    schema, 
    ciTablePrefix,
    cgTablePrefix, 
    databaseTable, 
    targetIds, 
    outcomeIds
){
  
  shiny::withProgress(message = 'Incidence rate...', value = 0, {
    
    shiny::incProgress(1/4, detail = paste("Extracting data"))
    
    
    result <- OhdsiReportGenerator::getIncidenceRates(
      connectionHandler = connectionHandler, 
      schema = schema, 
      ciTablePrefix = ciTablePrefix,
      cgTablePrefix = cgTablePrefix, 
      databaseTable = databaseTable, 
      targetIds = targetIds, 
      outcomeIds = outcomeIds
    )
    
    shiny::incProgress(4/4, detail = paste("Done"))
  })
  
  return(result)
}