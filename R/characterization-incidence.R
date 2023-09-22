# @file characterization-timeToEvent.R
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

.irPlotCategoricalChoices <- c("cdmSourceAbbreviation",
                         "ageGroupName",
                         "genderName",
                         "startYear",
                         "targetName",
                         "outcomeName",
                         "tar",
                         "cleanWindow")
names(.irPlotCategoricalChoices) <- c("Data Source", "Age Group", "Sex", "Calendar Year", "Target Cohort",
            "Outcome Cohort", "TAR", "Clean Window")

.irPlotNumericChoices <- c("incidenceRateP100py",
                         "incidenceProportionP100p",
                         "outcomes",
                         "outcomesPe",
                         "personOutcomes",
                         "personOutcomesPe",
                         "personsAtRisk",
                         "personsAtRiskPe",
                         "personDays",
                         "personDaysPe")
names(.irPlotNumericChoices) <- c("Incidence Rate (per 100PY)", "Incidence Proportion (per 100P)", "Outcomes", "Outcomes PE",
                                "Person Outcomes", "Person Outcomes PE", "Persons At Risk", "Persons At Risk PE", "Person Days", 
                                "Person Days PE")

as_ggplot <- function(x){
  # Open null device to avoid blank page before plot------
  # see cowplot:::as_grob.ggplot
  null_device <- base::getOption(
    "ggpubr.null_device",
    default = cowplot::pdf_null_device
  )
  cur_dev <- grDevices::dev.cur()
  # Open null device to avoid blank page before plot
  null_device(width = 6, height = 6)
  null_dev <- grDevices::dev.cur()
  on.exit({
    grDevices::dev.off(null_dev)
    if (cur_dev > 1) grDevices::dev.set(cur_dev)
  })
  # Convert to ggplot-------------
  cowplot::ggdraw() +
    cowplot::draw_grob(grid::grobTree(x))
}

# Custom function that takes a ggplotly figure and its facets as arguments.
# The upper x-values for each domain is set programmatically, but you can adjust
# the look of the figure by adjusting the width of the facet domain and the 
# corresponding annotations labels through the domain_offset variable
fixfacets <- function(figure, facets, domain_offset){
  
  fig <- figure
  
  # split x ranges from 0 to 1 into
  # intervals corresponding to number of facets
  # xHi = highest x for shape
  xHi <- seq(0, 1, len = length(facets)+1)
  xHi <- xHi[2:length(xHi)]
  
  xOs <- domain_offset
  
  # Shape manipulations, identified by dark grey backround: "rgba(217,217,217,1)"
  # structure: p$x$layout$shapes[[2]]$
  shp <- fig$x$layout$shapes
  j <- 1
  for (i in seq_along(shp)){
    if (shp[[i]]$fillcolor=="rgba(217,217,217,1)" & (!is.na(shp[[i]]$fillcolor))){
      #$x$layout$shapes[[i]]$fillcolor <- 'rgba(0,0,255,0.5)' # optionally change color for each label shape
      fig$x$layout$shapes[[i]]$x1 <- xHi[j]
      fig$x$layout$shapes[[i]]$x0 <- (xHi[j] - xOs)
      #fig$x$layout$shapes[[i]]$y <- -0.05
      j<-j+1
    }
  }
  
  # annotation manipulations, identified by label name
  # structure: p$x$layout$annotations[[2]]
  ann <- fig$x$layout$annotations
  annos <- facets
  j <- 1
  for (i in seq_along(ann)){
    if (ann[[i]]$text %in% annos){
      # but each annotation between high and low x,
      # and set adjustment to center
      fig$x$layout$annotations[[i]]$x <- (((xHi[j]-xOs)+xHi[j])/2)
      fig$x$layout$annotations[[i]]$xanchor <- 'center'
      #print(fig$x$layout$annotations[[i]]$y)
      #fig$x$layout$annotations[[i]]$y <- -0.05
      j<-j+1
    }
  }
  
  # domain manipulations
  # set high and low x for each facet domain
  xax <- names(fig$x$layout)
  j <- 1
  for (i in seq_along(xax)){
    if (!is.na(pmatch('xaxis', xax[i]))){
      #print(p[['x']][['layout']][[lot[i]]][['domain']][2])
      fig[['x']][['layout']][[xax[i]]][['domain']][2] <- xHi[j]
      fig[['x']][['layout']][[xax[i]]][['domain']][1] <- xHi[j] - xOs
      j<-j+1
    }
  }
  
  return(fig)
}




#' The module viewer for exploring incidence results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the description incidence module
#'
#' @export
characterizationIncidenceViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    shinydashboard::box(
      collapsible = TRUE,
      collapsed = TRUE,
      title = "Incidence Rates",
      width = "100%",
      shiny::htmlTemplate(system.file("characterization-www", "help-incidenceRate.html", package = utils::packageName()))
      ),
    
    shinydashboard::box(
      width = "100%",
      title = "Options",
      collapsible = TRUE,
      collapsed = F,
      shiny::uiOutput(ns('cohortInputs'))
    ),
    
    shiny::conditionalPanel(
      condition = "input.generate != 0",
      ns = ns,
      
      shiny::uiOutput(ns("inputsText")),
      
      shiny::tabsetPanel(
        type = 'pills',
        id = ns('incMainPanel'),
        
        shiny::tabPanel(
          title = "Incidence Rate Table",
          resultTableViewer(ns("incidenceRateTable"),
                            downloadedFileName = "incidenceRateTable-")
        ),
        shiny::tabPanel(
          title = "Incidence Rate Plots",
            shiny::tabsetPanel(
              type = 'pills',
              id = ns('incPlotPanel'),
                shiny::tabPanel(
                  title = "Custom Plot",
                  shinycssloaders::withSpinner(
                    plotly::plotlyOutput(ns('incidencePlot'),
                                         height = "1600px",
                                         width = "90%")
                  ),
                  # shiny::br(),
                  # shiny::p("Legend"),
                  shiny::plotOutput(ns('incidencePlotLegend'),
                                        width="100%",
                                        height="300px"
                                        )
                  
                )
              # ,
              #   shiny::tabPanel(
              #     title = "Standardized Plot"
              #   )
          #code to view plot here
            )
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
#' @param resultDatabaseSettings a list containing the characterization result schema, dbms, tablePrefix, databaseTable and cgTablePrefix
#' 
#' @return
#' The server to the prediction incidence module
#'
#' @export
characterizationIncidenceServer <- function(
  id, 
  connectionHandler,
  mainPanelTab,
  resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
#' Title
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
    function(input, output, session) {
      
      #if(mainPanelTab() != 'Time To Event'){
      #  return(invisible(NULL))
      #}
      
      ns <- session$ns
      
      withTooltip <- function(value, tooltip, ...) {
        shiny::div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
                   tippy::tippy(value, tooltip, ...))
      }
      
      cohorts <- getTargetOutcomes(
        connectionHandler,
        resultDatabaseSettings
      )
      
      allData <- getIncidenceData(targetIds = cohorts$targetIds,
                           outcomeIds = cohorts$outcomeIds,
                           connectionHandler = connectionHandler,
                           resultDatabaseSettings = resultDatabaseSettings
          )
      

      # update UI
      output$cohortInputs <- shiny::renderUI({
        
        shiny::fluidPage(
          shiny::fluidRow(
            div("Select Your Results", style = "font-weight: bold; font-size: 20px; text-align: center; margin-bottom: 20px;"),  # Table Options text
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shinyWidgets::pickerInput(
                inputId = session$ns('targetIds'), 
                label = 'Targets: ', 
                choices = cohorts$targetIds, 
                multiple = T,
                choicesOpt = list(style = rep_len("color: black;", 999)),
                selected = cohorts$targetIds,
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
                inputId = session$ns('outcomeIds'), 
                label = 'Outcomes: ', 
                choices = cohorts$outcomeIds,
                multiple = T,
                selected = cohorts$outcomeIds,
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
          
          shiny::fluidRow(
              shiny::column(
                width = 4,
                shinyWidgets::pickerInput(
                  inputId = session$ns("incidenceRateAgeFilter"),
                  label = "Filter By Age",
                  choices = sort(unique(allData$ageGroupName)),
                  selected = unique(allData$ageGroupName),
                  multiple = TRUE,
                  choicesOpt = list(style = rep_len("color: black;", 999)),
                  options = shinyWidgets::pickerOptions(
                    actionsBox = TRUE,
                    liveSearch = TRUE,
                    size = 10,
                    dropupAuto = TRUE,
                    liveSearchStyle = "contains",
                    liveSearchPlaceholder = "Type here to search",
                    virtualScroll = 50
                  )
              )
            ),
              shiny::column(
                width = 4,
                shinyWidgets::pickerInput(
                  inputId = session$ns("incidenceRateGenderFilter"),
                  label = "Filter By Sex",
                  choices = sort(unique(allData$genderName)),
                  selected = unique(allData$genderName),
                  multiple = TRUE,
                  choicesOpt = list(style = rep_len("color: black;", 999)),
                  options = shinyWidgets::pickerOptions(
                    actionsBox = TRUE,
                    liveSearch = TRUE,
                    size = 10,
                    dropupAuto = TRUE,
                    liveSearchStyle = "contains",
                    liveSearchPlaceholder = "Type here to search",
                    virtualScroll = 50
                  )
              )
            ),
            shiny::column(
              width = 4,
              shinyWidgets::pickerInput(
                inputId = session$ns("incidenceRateCalendarFilter"),
                label = "Filter By Start Year",
                choices = sort(unique(allData$startYear), decreasing = T),
                selected = unique(allData$startYear),
                multiple = TRUE,
                choicesOpt = list(style = rep_len("color: black;", 999)),
                options = shinyWidgets::pickerOptions(
                  actionsBox = TRUE,
                  liveSearch = TRUE,
                  size = 10,
                  dropupAuto = TRUE,
                  liveSearchStyle = "contains",
                  liveSearchPlaceholder = "Type here to search",
                  virtualScroll = 50
                )
              )
            )
          ),
          
          shiny::fluidRow(
            shiny::column(
              width = 12,
              div("", style = "font-weight: bold; font-size: 20px; text-align: center; margin-top: 20px; margin-bottom: 20px;"),  # empty row
            )
          ),
          
          shiny::fluidRow(
            shiny::column(
              width = 12,
              div("Configure Your Plot", style = "font-weight: bold; font-size: 20px; text-align: center; margin-top: 20px; margin-bottom: 20px;"),  # Plot Options text
            )
          ),
        
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shinyWidgets::pickerInput(
                inputId = session$ns("plotXAxis"),
                label = "X Axis (Categorical)",
                choices = c(.irPlotCategoricalChoices, "None"),
                selected = "startYear",
                multiple = F,
                choicesOpt = list(style = rep_len("color: black;", 999)),
                options = shinyWidgets::pickerOptions(
                  actionsBox = TRUE,
                  liveSearch = TRUE,
                  size = 10,
                  dropupAuto = TRUE,
                  liveSearchStyle = "contains",
                  liveSearchPlaceholder = "Type here to search",
                  virtualScroll = 50
                )
              )
             ),
            shiny::column(
              width = 3,
              shinyWidgets::pickerInput(
                inputId = session$ns("plotYAxis"),
                label = "Y Axis (Numeric)",
                choices = c(.irPlotNumericChoices, "None"),
                selected = "incidenceRateP100py",
                multiple = F,
                choicesOpt = list(style = rep_len("color: black;", 999)),
                options = shinyWidgets::pickerOptions(
                  actionsBox = TRUE,
                  liveSearch = TRUE,
                  size = 10,
                  dropupAuto = TRUE,
                  liveSearchStyle = "contains",
                  liveSearchPlaceholder = "Type here to search",
                  virtualScroll = 50
                )
              )
            ),
            shiny::column(
              width = 3,
              shinyWidgets::pickerInput(
                inputId = session$ns("plotXTrellis"),
                label = "Row Trellis (Categorical)",
                choices = c(.irPlotCategoricalChoices, "None"),
                selected = "targetName",
                multiple = F,
                choicesOpt = list(style = rep_len("color: black;", 999)),
                options = shinyWidgets::pickerOptions(
                  actionsBox = TRUE,
                  liveSearch = TRUE,
                  size = 10,
                  dropupAuto = TRUE,
                  liveSearchStyle = "contains",
                  liveSearchPlaceholder = "Type here to search",
                  virtualScroll = 50
                )
              )
            ),
            shiny::column(
              width = 3,
              shinyWidgets::pickerInput(
                inputId = session$ns("plotYTrellis"),
                label = "Column Trellis (Categorical)",
                choices = c(.irPlotCategoricalChoices, "None"),
                selected = "outcomeName",
                multiple = F,
                choicesOpt = list(style = rep_len("color: black;", 999)),
                options = shinyWidgets::pickerOptions(
                  actionsBox = TRUE,
                  liveSearch = TRUE,
                  size = 10,
                  dropupAuto = TRUE,
                  liveSearchStyle = "contains",
                  liveSearchPlaceholder = "Type here to search",
                  virtualScroll = 50
                )
              )
            )
            ),
          
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shinyWidgets::pickerInput(
                inputId = session$ns("plotColor"),
                label = "Color (Categorical)",
                choices = c(.irPlotCategoricalChoices, "None"),
                selected = "cdmSourceAbbreviation",
                multiple = F,
                choicesOpt = list(style = rep_len("color: black;", 999)),
                options = shinyWidgets::pickerOptions(
                  actionsBox = TRUE,
                  liveSearch = TRUE,
                  size = 10,
                  dropupAuto = TRUE,
                  liveSearchStyle = "contains",
                  liveSearchPlaceholder = "Type here to search",
                  virtualScroll = 50
                )
              )
            ),
            shiny::column(
              width = 4,
              shinyWidgets::pickerInput(
                inputId = session$ns("plotSize"),
                label = "Plot Point Size (Numeric)",
                choices = c(.irPlotNumericChoices, "None"),
                selected = "outcomes",
                multiple = F,
                choicesOpt = list(style = rep_len("color: black;", 999)),
                options = shinyWidgets::pickerOptions(
                  actionsBox = TRUE,
                  liveSearch = TRUE,
                  size = 10,
                  dropupAuto = TRUE,
                  liveSearchStyle = "contains",
                  liveSearchPlaceholder = "Type here to search",
                  virtualScroll = 50
                )
              )
            ),
            shiny::column(
              width = 4,
              shinyWidgets::pickerInput(
                inputId = session$ns("plotShape"),
                label = "Plot Point Shape (Categorical)",
                choices = c(.irPlotCategoricalChoices, "None"),
                selected = "genderName",
                multiple = F,
                choicesOpt = list(style = rep_len("color: black;", 999)),
                options = shinyWidgets::pickerOptions(
                  actionsBox = TRUE,
                  liveSearch = TRUE,
                  size = 10,
                  dropupAuto = TRUE,
                  liveSearchStyle = "contains",
                  liveSearchPlaceholder = "Type here to search",
                  virtualScroll = 50
                )
              )
            )
          ),
          
          #   shiny::column(
          #     width = 4,
          #     shiny::checkboxGroupInput(
          #       inputId = session$ns("irStratification"),
          #       label = "Stratify plot by",
          #       choices = c("Age", "Sex", "Calendar Year"),
          #       selected = c("Age", "Sex", "Calendar Year"),
          #       inline = TRUE
          #     )
          #   ),
          shiny::fluidRow(
            shiny::column(
              width = 8,
              shinyWidgets::pickerInput(
                inputId = session$ns("incidenceRateTarFilter"),
                label = "Select Time at risk (TAR)",
                choices = sort(unique(allData$tar)),
                selected = sort(unique(allData$tar))[1],
                multiple = F,
                choicesOpt = list(style = rep_len("color: black;", 999)),
                options = shinyWidgets::pickerOptions(
                  actionsBox = TRUE,
                  liveSearch = TRUE,
                  size = 10,
                  dropupAuto = TRUE,
                  liveSearchStyle = "contains",
                  liveSearchPlaceholder = "Type here to search",
                  virtualScroll = 50
                )
              )
            ),
          shiny::column(
            width = 4,
            shiny::tags$br(),
            shiny::checkboxInput(
              inputId = session$ns("irYscaleFixed"),
              label = "Use same y-axis scale across plots?"),
          )
        ),
        shiny::fluidRow(
          align = "left",
          shiny::br(),
          shiny::column(
            width = 3,
          shiny::actionButton(
            inputId = session$ns('generate'),
            label = 'Generate Report',
            style = "width:300px"
          )
          ),
          shiny::column(
            width = 3,
          shiny::actionButton(
            inputId = session$ns("resetButton"),
            label = "Reset Plot Options to Default",
          style = "width:200px"
          )
          )
        )
        )
      })
      
      # allDataDownload <- shiny::reactiveVal(data.frame())
      # selectedInputs <- shiny::reactiveVal()
      # output$IRinputsText <- shiny::renderUI(selectedInputs())
      
      #if generate is pushed, extract the data
      filteredData <- shiny::reactive(         
        {
          if (is.null(input$targetIds) |
              is.null(input$outcomeIds)) {
            data.frame()
          }
          
          getIncidenceData(targetIds = input$targetIds,
                           outcomeIds = input$outcomeIds,
                           connectionHandler = connectionHandler,
                           resultDatabaseSettings = resultDatabaseSettings
                           ) %>%
            dplyr::relocate(tar, .before = outcomes) %>%
            dplyr::mutate(incidenceProportionP100p = as.numeric(incidenceProportionP100p),
                          incidenceRateP100py = as.numeric(incidenceRateP100py),
                          dplyr::across(dplyr::where(is.numeric), round, 4),
                          targetIdShort = paste("C", targetCohortDefinitionId, sep = ":"),
                          outcomeIdShort = paste("C", outcomeCohortDefinitionId, sep = ":")) %>%
            dplyr::filter(ageGroupName %in% input$incidenceRateAgeFilter & 
                            genderName %in% input$incidenceRateGenderFilter & 
                            startYear %in% input$incidenceRateCalendarFilter  
                          )
        }
      )
      
      
      
      selectedInputs <- shiny::reactiveVal()
      output$inputsText <- shiny::renderUI(selectedInputs())
      
      # selectedCohortInputs <- shiny::reactiveVal()
      # output$cohortsInputText <- shiny::renderUI(selectedCohortInputs())

      # fetch data when targetIds or outcomeIds change
      shiny::observeEvent(
        eventExpr = input$generate,
        {
          if(is.null(input$targetIds) | is.null(input$outcomeIds)){
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
                    shiny::tags$b("Target(s):"),
                    shiny::HTML(paste("<p>", "C", cohorts$targetIds[cohorts$targetIds %in% input$targetIds], ": ",
                                       unique(names(cohorts$targetIds)[cohorts$targetIds %in% input$targetIds]), "</p>"
                    )
                  )
                  ),
                  shiny::column(
                    width = 6,
                    shiny::tags$b("Outcome(s):"),
                    shiny::HTML(paste("<p>", "C", cohorts$outcomeIds[cohorts$outcomeIds %in% input$outcomeIds], ": ",
                          unique(names(cohorts$outcomeIds)[cohorts$outcomeIds %in% input$outcomeIds]), "</p>"
                    )
                   )
                  )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  div(style = "height: 20px;")
                )
                ),
              shiny::fluidRow(
                shiny::column(
                  width = 3,
                  shiny::tags$b("Filtered Age Groups:"),
                  paste(unique(input$incidenceRateAgeFilter),
                        collapse = ", "
                  )
                ),
                shiny::column(
                  width = 3,
                  shiny::tags$b("Filtered Sex Groups:"),
                  paste(unique(input$incidenceRateGenderFilter),
                        collapse = ", "
                  )
                ),
                shiny::column(
                  width = 3,
                  shiny::tags$b("Filtered Calendar Years:"),
                  paste(unique(input$incidenceRateCalendarFilter),
                        collapse = ", "
                  )
                )
                ),
                shiny::fluidRow(
                  shiny::column(
                    width = 12,
                    div(style = "height: 20px;")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(
                    width = 3,
                    shiny::tags$b("X Axis:"),
                    paste(names(.irPlotCategoricalChoices[.irPlotCategoricalChoices %in% input$plotXAxis]))
                    ),
                  shiny::column(
                    width = 3,
                    shiny::tags$b("Y Axis:"),
                    paste(names(.irPlotNumericChoices[.irPlotNumericChoices %in% input$plotYAxis]))
                  ),
                  shiny::column(
                    width = 3,
                    shiny::tags$b("Row Trellis:"),
                    paste(names(.irPlotCategoricalChoices[.irPlotCategoricalChoices %in% input$plotXTrellis]))
                  ),
                  shiny::column(
                    width = 3,
                    shiny::tags$b("Column Trellis:"),
                    paste(names(.irPlotCategoricalChoices[.irPlotCategoricalChoices %in% input$plotYTrellis])),
                  )
                  ),
                shiny::fluidRow(
                  shiny::column(
                    width = 12,
                    div(style = "height: 20px;")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(
                    width = 3,
                    shiny::tags$b("Color:"),
                    paste(names(.irPlotCategoricalChoices[.irPlotCategoricalChoices %in% input$plotColor]))
                  ),
                  shiny::column(
                    width = 3,
                    shiny::tags$b("Plot Point Size:"),
                    paste(names(.irPlotNumericChoices[.irPlotNumericChoices %in% input$plotSize]))
                  ),
                  shiny::column(
                    width = 3,
                    shiny::tags$b("Plot Point Shape:"),
                    paste(names(.irPlotCategoricalChoices[.irPlotCategoricalChoices %in% input$plotShape]))
                  )
                ),
                shiny::fluidRow(
                  shiny::column(
                    width = 12,
                    div(style = "height: 20px;")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(
                    width = 3,
                    shiny::tags$b("Plotted TAR:"),
                    paste(input$incidenceRateTarFilter)
                      ),
                  shiny::column(
                    width = 3,
                    shiny::tags$b("Y Axis Scale Fixed?"),
                    if(input$irYscaleFixed==T){
                        "Yes"
                    }
                    else{
                      "No"
                    }
                  )
               )
            )
          )
         )
        }
        )
      
      # # fetch data when targetIds or outcomeIds change
      # shiny::observeEvent(
      #   eventExpr = input$generate,
      #   {
      #     if(is.null(input$targetIds) | is.null(input$outcomeIds)){
      #       return(invisible(NULL))
      #     }
      #     
      #     selectedCohortInputs(
      #        shinydashboard::box(
      #          status = 'warning', 
      #          width = "100%",
      #          title = 'Selected:',
      #          shiny::div(
      #            shiny::fluidRow(
      #             shiny::column(
      #               width = 4,
      #               shiny::tags$b("Target(s):"),
      #               shiny::HTML(paste("<p>", "C", cohorts$targetIds[cohorts$targetIds %in% input$targetIds], ": ",
      #                                 unique(names(cohorts$targetIds)[cohorts$targetIds %in% input$targetIds]), "</p>"
      #               )
      #               )
      #             ),
      #             shiny::column(
      #               width = 4,
      #               shiny::tags$b("Outcome(s):"),
      #               shiny::HTML(paste("<p>", "C", cohorts$outcomeIds[cohorts$outcomeIds %in% input$outcomeIds], ": ",
      #                                 unique(names(cohorts$outcomeIds)[cohorts$outcomeIds %in% input$outcomeIds]), "</p>"
      #               )
      #               )
      #             )
      #           )
      #          )
      #        )
      #      )
      #   }
      # )
    

#load in custom colDefs
      
      incidenceColList <- ParallelLogger::loadSettingsFromJson(system.file("components-columnInformation",
                                                                        "characterization-incidence-colDefs.json",
                                                                        package = "OhdsiShinyModules")
      )
      
      class(incidenceColList$genderName$filterMethod) <- "JS_EVAL"
      
      renderIrTable <- shiny::eventReactive(
        eventExpr = input$generate,
        {
          filteredData()
        }
      )
      
      resultTableServer(id = "incidenceRateTable",
                        df = renderIrTable,
                        selectedCols = c("cdmSourceAbbreviation", "targetName", "outcomeName",
                                         "ageGroupName", "genderName", "startYear", "tar", "outcomes",
                                         "incidenceProportionP100p", "incidenceRateP100py"),
                        sortedCols = c("ageGroupName", "genderName", "startYear", "incidenceRateP100py"),
                        elementId = "incidence-select",
                        colDefsInput = incidenceColList,
                        downloadedFileName = "incidenceRateTable-")
      
      #ir plots
      renderIrPlot <- shiny::eventReactive(
        eventExpr = input$generate,
        {
          plotData <- filteredData() %>%
            dplyr::filter(tar %in% input$incidenceRateTarFilter)
          
          # Take the specific tar value you want to plot
          tar_value <- unique(plotData$tar)[1]
          
          # Create a column for the tooltip text
          plotData$tooltip <- with(plotData, paste(
            "Incidence Rate:", incidenceRateP100py, "<br>",
            "Incidence Proportion:", incidenceProportionP100p, "<br>",
            "Outcome ID:", outcomeIdShort, "<br>",
            "Outcome Name:", outcomeName, "<br>",
            "Target ID:", targetIdShort, "<br>",
            "Target Name:", targetName, "<br>",
            "Data Source:", cdmSourceAbbreviation, "<br>",
            "Calendar Year:", startYear, "<br>",
            "Age Group:", ageGroupName, "<br>",
            "Sex:", genderName, "<br>",
            "Clean Window:", cleanWindow, "<br>",
            "Persons at Risk:", personsAtRisk, "<br>",
            "Person Days:", personDays, "<br>",
            "Outcomes:", outcomes
          ))
          

          
          
          
          
          # Check if color, size, shape, and trellis variables are selected, and set aesthetics accordingly
          color_aesthetic <- NULL
          size_aesthetic <- NULL
          shape_aesthetic <- NULL
          trellis_aesthetic_x <- NULL
          trellis_aesthetic_y <- NULL
          
          if (input$plotColor == "Target Cohort" | input$plotColor == "Outcome Cohort") {
            color_aesthetic <- if (input$plotColor == "Target Cohort") {
              dplyr::vars(targetIdShort)
            } else if (input$plotColor == "Outcome Cohort") {
              dplyr::vars(outcomeIdShort)
            }
          }
          
          if (input$plotShape == "Target Cohort" | input$plotShape == "Outcome Cohort") {
            shape_aesthetic <- if (input$plotShape == "Target Cohort") {
              dplyr::vars(targetIdShort)
            } else if (input$plotShape == "Outcome Cohort") {
              dplyr::vars(outcomeIdShort)
            }
          }
          
          max_length <- max(nchar(unique(input$plotXAxis)))
          
          if (input$plotXTrellis!=input$plotYTrellis){

          # Create the base plot with conditional aesthetics
          base_plot <- ggplot2::ggplot(data = plotData,
                                       ggplot2::aes(x = .data[[input$plotXAxis]],
                                                    y = .data[[input$plotYAxis]],
                                                    shape = if(input$plotShape != "None" & input$plotShape != "Target Cohort" & 
                                                               input$plotShape != "Outcome Cohort") .data[[input$plotShape]]
                                                            else shape_aesthetic,
                                                    color = if(input$plotColor != "None" & input$plotColor != "Target Cohort" & 
                                                               input$plotColor != "Outcome Cohort") .data[[input$plotColor]]
                                                            else color_aesthetic,
                                                    text = tooltip)
                                       ) + 
            ggplot2::geom_point(ggplot2::aes(size = if(input$plotSize != "None") .data[[input$plotSize]] else NULL,
                                               alpha = 0.6)
                                  ) 
          
          # Rotate x-axis labels if the maximum length is greater than 10
          if (max_length > 10) {
            base_plot <- base_plot + 
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
          }

          # Add trellising if it's not NULL
          if (input$plotXTrellis!="None" & input$plotXTrellis!="targetName" & input$plotXTrellis!="outcomeName" & 
              input$plotYTrellis!="None" & input$plotYTrellis!="targetName" & input$plotYTrellis!="outcomeName") {
            base_plot <- base_plot + ggplot2::facet_grid(
              rows = vars(.data[[input$plotXTrellis]]),
              cols = vars(.data[[input$plotYTrellis]]),
              scales = if (input$irYscaleFixed) "fixed" else "free_y"
            ) +
              ggplot2::theme(strip.background = ggplot2::element_blank(), 
                             strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
              ) +
              ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
          }
          else if (input$plotXTrellis!="None" & input$plotXTrellis=="targetName" & input$plotXTrellis!="outcomeName" & 
                   input$plotYTrellis!="None" & input$plotYTrellis!="targetName" & input$plotYTrellis!="outcomeName") {
            base_plot <- base_plot + ggplot2::facet_grid(
              rows = vars(targetIdShort),
              cols = vars(.data[[input$plotYTrellis]]),
              scales = if (input$irYscaleFixed) "fixed" else "free_y"
            ) +
              ggplot2::theme(strip.background = ggplot2::element_blank(), 
                             strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
              ) +
              ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
          }
          else if (input$plotXTrellis!="None" & input$plotXTrellis!="targetName" & input$plotXTrellis=="outcomeName" & 
                   input$plotYTrellis!="None" & input$plotYTrellis!="targetName" & input$plotYTrellis!="outcomeName") {
            base_plot <- base_plot + ggplot2::facet_grid(
              rows = vars(outcomeIdShort),
              cols = vars(.data[[input$plotYTrellis]]),
              scales = if (input$irYscaleFixed) "fixed" else "free_y"
            ) +
              ggplot2::theme(strip.background = ggplot2::element_blank(), 
                             strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
              ) +
              ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
          }
          else if (input$plotXTrellis!="None" & input$plotXTrellis!="targetName" & input$plotXTrellis!="outcomeName" & 
                   input$plotYTrellis!="None" & input$plotYTrellis=="targetName" & input$plotYTrellis!="outcomeName") {
            base_plot <- base_plot + ggplot2::facet_grid(
              rows = vars(.data[[input$plotXTrellis]]),
              cols = vars(targetIdShort),
              scales = if (input$irYscaleFixed) "fixed" else "free_y"
            ) +
              ggplot2::theme(strip.background = ggplot2::element_blank(), 
                             strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
              ) +
              ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
          }
          else if (input$plotXTrellis!="None" & input$plotXTrellis!="targetName" & input$plotXTrellis!="outcomeName" & 
                   input$plotYTrellis!="None" & input$plotYTrellis!="targetName" & input$plotYTrellis=="outcomeName") {
            base_plot <- base_plot + ggplot2::facet_grid(
              rows = vars(.data[[input$plotXTrellis]]),
              cols = vars(outcomeIdShort),
              scales = if (input$irYscaleFixed) "fixed" else "free_y"
            ) +
              ggplot2::theme(strip.background = ggplot2::element_blank(), 
                             strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
              ) +
              ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
          }
          else if (input$plotXTrellis!="None" & input$plotXTrellis=="targetName" & input$plotXTrellis!="outcomeName" & 
                   input$plotYTrellis!="None" & input$plotYTrellis=="targetName" & input$plotYTrellis!="outcomeName") {
            base_plot <- base_plot + ggplot2::facet_grid(
              rows = vars(targetIdShort),
              cols = vars(targetIdShort),
              scales = if (input$irYscaleFixed) "fixed" else "free_y"
            ) +
              ggplot2::theme(strip.background = ggplot2::element_blank(), 
                             strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
              ) +
              ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
          }
          else if (input$plotXTrellis!="None" & input$plotXTrellis=="targetName" & input$plotXTrellis!="outcomeName" & 
                   input$plotYTrellis!="None" & input$plotYTrellis!="targetName" & input$plotYTrellis=="outcomeName") {
            base_plot <- base_plot + ggplot2::facet_grid(
              rows = vars(targetIdShort),
              cols = vars(outcomeIdShort),
              scales = if (input$irYscaleFixed) "fixed" else "free_y"
            ) +
              ggplot2::theme(strip.background = ggplot2::element_blank(), 
                             strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
              ) +
              ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
          }
          else if (input$plotXTrellis!="None" & input$plotXTrellis!="targetName" & input$plotXTrellis=="outcomeName" & 
                   input$plotYTrellis!="None" & input$plotYTrellis=="targetName" & input$plotYTrellis!="outcomeName") {
            base_plot <- base_plot + ggplot2::facet_grid(
              rows = vars(outcomeIdShort),
              cols = vars(targetIdShort),
              scales = if (input$irYscaleFixed) "fixed" else "free_y"
            ) +
              ggplot2::theme(strip.background = ggplot2::element_blank(), 
                             strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
              ) +
              ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
          }
          else if (input$plotXTrellis!="None" & input$plotXTrellis!="targetName" & input$plotXTrellis=="outcomeName" & 
                   input$plotYTrellis!="None" & input$plotYTrellis!="targetName" & input$plotYTrellis=="outcomeName") {
            base_plot <- base_plot + ggplot2::facet_grid(
              rows = vars(outcomeIdShort),
              cols = vars(outcomeIdShort),
              scales = if (input$irYscaleFixed) "fixed" else "free_y"
            ) +
              ggplot2::theme(strip.background = ggplot2::element_blank(), 
                             strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
              ) +
              ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
          }
          else if (input$plotXTrellis=="None" & input$plotXTrellis!="targetName" & input$plotXTrellis!="outcomeName" & 
                   input$plotYTrellis!="None" & input$plotYTrellis!="targetName" & input$plotYTrellis=="outcomeName") {
            base_plot <- base_plot + ggplot2::facet_grid(
              rows = NULL,
              cols = vars(outcomeIdShort),
              scales = if (input$irYscaleFixed) "fixed" else "free_y"
            ) +
              ggplot2::theme(strip.background = ggplot2::element_blank(), 
                             strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
              ) +
              ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
          }
          else if (input$plotXTrellis=="None" & input$plotXTrellis!="targetName" & input$plotXTrellis!="outcomeName" & 
                   input$plotYTrellis!="None" & input$plotYTrellis=="targetName" & input$plotYTrellis!="outcomeName") {
            base_plot <- base_plot + ggplot2::facet_grid(
              rows = NULL,
              cols = vars(targetIdShort),
              scales = if (input$irYscaleFixed) "fixed" else "free_y"
            ) +
              ggplot2::theme(strip.background = ggplot2::element_blank(), 
                             strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
              ) +
              ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
          }
          else if (input$plotXTrellis=="None" & input$plotXTrellis!="targetName" & input$plotXTrellis!="outcomeName" & 
                   input$plotYTrellis!="None" & input$plotYTrellis!="targetName" & input$plotYTrellis!="outcomeName") {
            base_plot <- base_plot + ggplot2::facet_grid(
              rows = NULL,
              cols = vars(.data[[input$plotYTrellis]]),
              scales = if (input$irYscaleFixed) "fixed" else "free_y"
            ) +
              ggplot2::theme(strip.background = ggplot2::element_blank(), 
                             strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
              ) +
              ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
          }
          else if (input$plotXTrellis!="None" & input$plotXTrellis!="targetName" & input$plotXTrellis!="outcomeName" & 
                   input$plotYTrellis=="None" & input$plotYTrellis!="targetName" & input$plotYTrellis!="outcomeName") {
            base_plot <- base_plot + ggplot2::facet_grid(
              rows = vars(.data[[input$plotXTrellis]]),
              cols = NULL,
              scales = if (input$irYscaleFixed) "fixed" else "free_y"
            ) +
              ggplot2::theme(strip.background = ggplot2::element_blank(), 
                             strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
              ) +
              ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
          }
          else if (input$plotXTrellis!="None" & input$plotXTrellis=="targetName" & input$plotXTrellis!="outcomeName" & 
                   input$plotYTrellis=="None" & input$plotYTrellis!="targetName" & input$plotYTrellis!="outcomeName") {
            base_plot <- base_plot + ggplot2::facet_grid(
              rows = vars(targetIdShort),
              cols = NULL,
              scales = if (input$irYscaleFixed) "fixed" else "free_y"
            ) +
              ggplot2::theme(strip.background = ggplot2::element_blank(), 
                             strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
              ) +
              ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
          }
          else if (input$plotXTrellis!="None" & input$plotXTrellis!="targetName" & input$plotXTrellis=="outcomeName" & 
                   input$plotYTrellis=="None" & input$plotYTrellis!="targetName" & input$plotYTrellis!="outcomeName") {
            base_plot <- base_plot + ggplot2::facet_grid(
              rows = vars(outcomeIdShort),
              cols = NULL,
              scales = if (input$irYscaleFixed) "fixed" else "free_y"
            ) +
              ggplot2::theme(strip.background = ggplot2::element_blank(), 
                             strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
              ) +
              ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
          }
        
          
          # Rest of your ggplot code remains the same
          base_plot <- base_plot + ggplot2::labs(title = paste("Incidence Rate for TAR:", tar_value),
                                                 x = names(.irPlotCategoricalChoices[.irPlotCategoricalChoices %in% input$plotXAxis]),
                                                 y = names(.irPlotNumericChoices[.irPlotNumericChoices %in% input$plotYAxis]),
                                                 color = names(.irPlotCategoricalChoices[.irPlotCategoricalChoices %in% input$plotColor]),
                                                 size = names(.irPlotNumericChoices[.irPlotNumericChoices %in% input$plotSize]),
                                                 shape = names(.irPlotCategoricalChoices[.irPlotCategoricalChoices %in% input$plotShape])
                                                 ) +
            ggplot2::guides(alpha = "none") + # Remove the alpha legend
            ggplot2::theme_bw() +
            ggplot2::theme(title = ggplot2::element_text(hjust = 0.5),
                           plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 10)),
                           axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
                           axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
                           legend.box = "horizontal",
                           panel.spacing = ggplot2::unit(1, "lines"),
                           strip.background = ggplot2::element_blank(), 
                           strip.text = ggplot2::element_text(face="bold")
                           ) +
            ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
          
          
          
          
          
          #   
          #   # Create a custom color scale
          #   color_scale <- RColorBrewer::colorRampPalette(brewer.pal(9, "YlOrRd"))(100)
          #   
          #   # Create a faceted heatmap by outcome and data source
          #   p <- ggplot2::ggplot(data = plotData, aes(x = targetIdShort, y = ageGroupName,
          #                                             text = paste("Outcome ID:", outcomeIdShort, "<br>Outcome:", outcomeName,
          #                                                          "<br>Target ID:", targetIdShort, "<br>Target:", targetName,
          #                                                          "<br>TAR:", tar, "<br>Age:", ageGroupName, "<br>Sex:", genderName,
          #                                                          "<br>TAR:",
          #                                                          "<br>Incidence Rate:", incidenceRateP100py))) +
          #     ggplot2::geom_tile(aes(fill = incidenceRateP100py), color = "white") +
          #     ggplot2::scale_fill_gradient(colors = color_scale, name = "Incidence Rate") +
          #     ggplot2::labs(title = "Incidence Rate by Strata Variables",
          #          x = "Target Population Cohort",
          #          y = "Age Category") +
          #     ggplot2::theme_minimal() +
          #     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          #           plot.title = element_text(hjust = 0.5)) +
          #     ggplot2::facet_grid(outcome ~ data_source, scales = "free_x", space = "free_x")
          #   
          #   # Convert the ggplot plot to a Plotly plot
          #   p <- plotly::ggplotly(p)
          #   
          #   
            
          }
          
          
          else {

            shiny::validate("Cannout use the same trellis for row and column, please make another selection.")

          }
        
        return(base_plot)
          }
      )
      
      #render the event reactive incidence plot without legend
      renderIrPlotNoLegend <- shiny::eventReactive(
        eventExpr = input$generate,
        {
          plotData <- filteredData() %>%
            dplyr::filter(tar %in% input$incidenceRateTarFilter)
          
          # Get the number of facets in both rows and columns
          num_rows <- length(unique(plotData[[input$plotXTrellis]]))
          num_cols <- length(unique(plotData[[input$plotYTrellis]]))
          
          max_length <- max(nchar(unique(input$plotXAxis)))
          
          base_plot <- renderIrPlot()
          
          p <- base_plot +
            ggplot2::guides(shape = FALSE, color = FALSE, size = FALSE)
          
          # Convert the ggplot to a plotly object
          p <- plotly::ggplotly(p, tooltip = "text")
          
          # Center the main plot title
          p <- p %>% plotly::layout(title = list(x = 0.5, xanchor = "center"),
                                    margin = list(t = 100),
                                    xaxis = list(tickangle = 45, 
                                                 title =list(standoff = 40)
                                                 ),
                                    yaxis = list(title =list(standoff = 40)
                                    )
          ) 
          
          # Specify the angle for x-axis labels
          # if (max_length > 10) {
          #   p <- p %>% 
          #     plotly::layout(xaxis = list(tickangle = 45))
          # }
          
          # if(input$plotXTrellis!= "None"){
          #   
          #   f <- fixfacets(figure = p, facets <- unique(plotData[,input$plotXTrellis]), domain_offset <- 0.1)
          #   
          # }
          # 
          # else f <- p
          # 
          # return(f)
          
          return(p)
          
        }
      )
      
      #render the event reactive incidence plot legend only
      renderIrPlotLegend <- shiny::eventReactive(
        eventExpr = input$generate,
        {
          base_plot <- renderIrPlot()
          
          p <- as_ggplot(cowplot::get_legend(base_plot))
          
          return(p)
        }
      )
      
      
      output$incidencePlot <-  
          plotly::renderPlotly({
            renderIrPlotNoLegend()
          })
      
      output$incidencePlotLegend <-  
        shiny::renderPlot({
         renderIrPlotLegend()
        })
      
      # Track if the "Generate Report" button has been pressed at least once
      reportGenerated <- reactiveVal(FALSE)
      
      
      # Define an event to reset the plot
      observeEvent(input$resetButton, {
        # Reset the input selections to their defaults
        shinyWidgets::updatePickerInput(session, "incidenceRateTarFilter")
        shinyWidgets::updatePickerInput(session, "plotXAxis", selected = "startYear")
        shinyWidgets::updatePickerInput(session, "plotYAxis", selected = "incidenceRateP100py")
        shinyWidgets::updatePickerInput(session, "plotColor", selected = "cdmSourceAbbreviation")
        shinyWidgets::updatePickerInput(session, "plotSize", selected = "outcomes")
        shinyWidgets::updatePickerInput(session, "plotShape", selected = "genderName")
        shinyWidgets::updatePickerInput(session, "plotXTrellis", selected = "targetName")
        shinyWidgets::updatePickerInput(session, "plotYTrellis", selected = "outcomeName")
        shiny::updateCheckboxInput(session, "irYscaleFixed", value = FALSE)
        
        shinyjs::click("generate")
        
        # Trigger the plot regeneration by incrementing the generation counter
        isolate({
          shiny::updateNumericInput(session, "generate", value = input$generate + 1)
        })
      })

      
      return(invisible(NULL))
      
    })
}
          




getIncidenceData <- function(
  targetIds,
  outcomeIds,
  connectionHandler,
  resultDatabaseSettings
){
  
  #shiny::withProgress(message = 'Getting incidence data', value = 0, {
    
  sql <- 'select d.cdm_source_abbreviation, i.* 
    from @result_schema.@incidence_table_prefixINCIDENCE_SUMMARY i
    inner join @result_schema.@database_table_name d
    on d.database_id = i.database_id
    where target_cohort_definition_id in (@target_ids)
    and outcome_cohort_definition_id in (@outcome_ids)
    ;'
  
  #shiny::incProgress(1/2, detail = paste("Created SQL - Extracting..."))
  
  resultTable <- connectionHandler$queryDb(
    sql = sql, 
    result_schema = resultDatabaseSettings$schema,
    incidence_table_prefix = resultDatabaseSettings$incidenceTablePrefix,
    target_ids = paste(as.double(targetIds), collapse = ','),
    outcome_ids = paste(as.double(outcomeIds), collapse = ','),
    database_table_name = resultDatabaseSettings$databaseTable
  )
  
  #shiny::incProgress(2/2, detail = paste("Done..."))
  
  #})
  
  # format the tar
  resultTable$tar <- paste0('(',resultTable$tarStartWith, " + ", resultTable$tarStartOffset, ') - (', resultTable$tarEndWith, " + ", resultTable$tarEndOffset, ')')
  resultTable <- resultTable %>% 
    dplyr::select(-c("tarStartWith","tarStartOffset","tarEndWith","tarEndOffset", "tarId", "subgroupName"))
  
  resultTable[is.na(resultTable)] <- 'All'
  resultTable <- unique(resultTable)
  
  return(resultTable)
}


getTargetOutcomes <- function(
    connectionHandler,
    resultDatabaseSettings
){
  
 # shiny::withProgress(message = 'Getting incidence inputs', value = 0, {
  
  sql <- 'select distinct target_cohort_definition_id, target_name 
  from @result_schema.@incidence_table_prefixINCIDENCE_SUMMARY;'
  
  #shiny::incProgress(1/3, detail = paste("Created SQL - Extracting targets"))

  targets <- connectionHandler$queryDb(
    sql = sql, 
    result_schema = resultDatabaseSettings$schema,
    incidence_table_prefix = resultDatabaseSettings$incidenceTablePrefix
  )
  targetIds <- targets$targetCohortDefinitionId
  names(targetIds) <- targets$targetName
  
  sql <- 'select distinct outcome_cohort_definition_id, outcome_name 
  from @result_schema.@incidence_table_prefixINCIDENCE_SUMMARY;'

  #shiny::incProgress(2/3, detail = paste("Created SQL - Extracting outcomes"))
  
  outcomes <- connectionHandler$queryDb(
    sql = sql, 
    result_schema = resultDatabaseSettings$schema,
    incidence_table_prefix = resultDatabaseSettings$incidenceTablePrefix
  )
  
  outcomeIds <- outcomes$outcomeCohortDefinitionId
  names(outcomeIds) <- outcomes$outcomeName
  
 # shiny::incProgress(3/3, detail = paste("Done"))
 # })
  
  return(
    list(
      targetIds = targetIds,
      outcomeIds = outcomeIds
    )
  )
  
}

#read in custom column name colDef list from rds file, generated by 
#heplers-componentsCreateCustomColDefList.R

#       customColDefs <- createCustomColDefList(
#         rawColNames = names(incidenceColList),
#         niceColNames = c("Database",
#                          "Ref ID",
#                          "Database ID",
#                          "Source ID",
#                          "Target ID",
#                          "Target Name",
#                          "Subgroup ID",
#                          "Outcome ID",
#                          "Outcome Def ID",
#                          "Outcome Name",
#                          "Clean Window",
#                          "Age ID",
#                          "Age Group",
#                          "Gender ID",
#                          "Gender",
#                          "Year",
#                          "Persons At Risk PE",
#                          "Persons At Risk",
#                          "Person Days PE",
#                          "Person Days",
#                          "Person Outcomes PE",
#                          "Person Outcomes",
#                          "Total Outcomes PE",
#                          "Total Outcomes",
#                          "Inc. Proportion Per 100P",
#                          "Inc. Rate Per 100PY",
#                          "Time At Risk"),
#         tooltipText = c("The name of the database",
#                         "The reference ID",
#                         "The database ID",
#                         "The source ID",
#                         "The cohort definition ID of the target",
#                         "The name of the target cohort",
#                         "The name of the subgroup",
#                         "The cohort definition ID of the outcome",
#                         "The cohort definition ID of the outcome (duplicated)",
#                         "The name of the outcome cohort",
#                         "The clean window (in days)",
#                         "The age ID",
#                         "The age group category (in years)",
#                         "The gender ID",
#                         "The gender category",
#                         "The start year of the analysis period",
#                         "The distinct persons at risk before removing excluded time (pre-exclude) from TAR",
#                         "The distinct persons at risk after removing excluded time from TAR",
#                         "Total TAR (in days) before excluded time was removed (pre-exclude)",
#                         "Total TAR (in days) after excluded time was removed",
#                         "The distinct persons with the outcome before removing excluded time (pre-exclude) from TAR",
#                         "The distinct persons with the outcome after removing excluded time from TAR",
#                         "Total outcomes before removing excluded time (pre-exclude) from TAR",
#                         "Total outcomes after removing excluded time from TAR",
#                         "The incidence proportion (per 100 people), calculated by personOutcomes/personsAtRisk*100",
#                         "The incidence rate (per 100 person years), calculated by outcomes/personDays/365.25*100",
#                         "The TAR window (in days)"
#                         ),
#         customColDefOptions = list(
#           list(filterInput = function(values, name) {
#                  tags$select(
#                    # Set to undefined to clear the filter
#                    onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
#                    # "All" has an empty value to clear the filter, and is the default option
#                    tags$option(value = "", "All"),
#                    lapply(unique(values), tags$option),
#                    "aria-label" = sprintf("Filter %s", name),
#                    style = "width: 100%; height: 28px;"
#                  )
#                }),
#           list(show = F),
#           list(show = F),
#           list(show = F),
#           list(filterInput = function(values, name) {
#             tags$select(
#               # Set to undefined to clear the filter
#               onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
#               # "All" has an empty value to clear the filter, and is the default option
#               tags$option(value = "", "All"),
#               lapply(unique(values), tags$option),
#               "aria-label" = sprintf("Filter %s", name),
#               style = "width: 100%; height: 28px;"
#             )
#           }),
#           list(filterInput = function(values, name) {
#             tags$select(
#               # Set to undefined to clear the filter
#               onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
#               # "All" has an empty value to clear the filter, and is the default option
#               tags$option(value = "", "All"),
#               lapply(unique(values), tags$option),
#               "aria-label" = sprintf("Filter %s", name),
#               style = "width: 100%; height: 28px;"
#             )
#           }),
#           list(filterInput = function(values, name) {
#             tags$select(
#               # Set to undefined to clear the filter
#               onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
#               # "All" has an empty value to clear the filter, and is the default option
#               tags$option(value = "", "All"),
#               lapply(unique(values), tags$option),
#               "aria-label" = sprintf("Filter %s", name),
#               style = "width: 100%; height: 28px;"
#             )
#           }),
#           list(filterInput = function(values, name) {
#             tags$select(
#               # Set to undefined to clear the filter
#               onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
#               # "All" has an empty value to clear the filter, and is the default option
#               tags$option(value = "", "All"),
#               lapply(unique(values), tags$option),
#               "aria-label" = sprintf("Filter %s", name),
#               style = "width: 100%; height: 28px;"
#             )
#           }),
#           list(show = F),
#           list(filterInput = function(values, name) {
#             tags$select(
#               # Set to undefined to clear the filter
#               onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
#               # "All" has an empty value to clear the filter, and is the default option
#               tags$option(value = "", "All"),
#               lapply(unique(values), tags$option),
#               "aria-label" = sprintf("Filter %s", name),
#               style = "width: 100%; height: 28px;"
#             )
#           }),
#           list(filterInput = function(values, name) {
#             tags$select(
#               # Set to undefined to clear the filter
#               onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
#               # "All" has an empty value to clear the filter, and is the default option
#               tags$option(value = "", "All"),
#               lapply(unique(values), tags$option),
#               "aria-label" = sprintf("Filter %s", name),
#               style = "width: 100%; height: 28px;"
#             )
#           }),
#           list(show = F),
#           list(defaultSortOrder = "desc",
#                filterInput = function(values, name) {
#                  tags$select(
#                    # Set to undefined to clear the filter
#                    onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
#                    # "All" has an empty value to clear the filter, and is the default option
#                    tags$option(value = "", "All"),
#                    lapply(unique(values), tags$option),
#                    "aria-label" = sprintf("Filter %s", name),
#                    style = "width: 100%; height: 28px;"
#                  )
#                }
#                ),
#           list(show = F),
#           list(defaultSortOrder = "asc",
#                filterInput = function(values, name) {
#                  tags$select(
#                    # Set to undefined to clear the filter
#                    onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
#                    # "All" has an empty value to clear the filter, and is the default option
#                    tags$option(value = "", "All"),
#                    lapply(unique(values), tags$option),
#                    "aria-label" = sprintf("Filter %s", name),
#                    style = "width: 100%; height: 28px;"
#                  )
#                },
#                filterMethod = htmlwidgets::JS("function(rows, columnId, filterValue) {
# return rows.filter(function(row) {
# return row.values[columnId] == filterValue
# })
# }")
#           ),
#           list(defaultSortOrder = "desc",
#                filterInput = function(values, name) {
#                  tags$select(
#                    # Set to undefined to clear the filter
#                    onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
#                    # "All" has an empty value to clear the filter, and is the default option
#                    tags$option(value = "", "All"),
#                    lapply(unique(values), tags$option),
#                    "aria-label" = sprintf("Filter %s", name),
#                    style = "width: 100%; height: 28px;"
#                  )
#                }),
#           list(NULL),
#           list(NULL),
#           list(NULL),
#           list(NULL),
#           list(NULL),
#           list(NULL),
#           list(NULL),
#           list(NULL),
#           list(NULL),
#           list(defaultSortOrder = "desc",
#                filterMethod = htmlwidgets::JS('filterMinValue'),
#                filterInput = htmlwidgets::JS('rangeFilter')),
#           list(filterInput = function(values, name) {
#                  tags$select(
#                    # Set to undefined to clear the filter
#                    onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
#                    # "All" has an empty value to clear the filter, and is the default option
#                    tags$option(value = "", "All"),
#                    lapply(unique(values), tags$option),
#                    "aria-label" = sprintf("Filter %s", name),
#                    style = "width: 100%; height: 28px;"
#                  )
#                })
#         )
#       )
# 
#       # use the below as a guide to save named colDef list as JSON then read it back!
#        ParallelLogger::saveSettingsToJson(customColDefs, "./inst/components-columnInformation/characterization-incidence-colDefs.json")
#        loadTest <- ParallelLogger::loadSettingsFromJson("./inst/components-columnInformation/characterization-incidence-colDefs.json")













