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
    shiny::tags$style(
      '
      .inc-viewer-shell {
        display: grid;
        gap: 16px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .inc-hero {
        border-radius: 22px;
        padding: 22px 24px;
        background: linear-gradient(135deg, #f8fbff 0%, #eef6ff 45%, #fdfcff 100%);
        border: 1px solid #dbe6f3;
        box-shadow: 0 14px 30px rgba(15, 23, 42, 0.08);
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .inc-hero-top {
        display: flex;
        align-items: center;
        gap: 14px;
        margin-bottom: 8px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
        flex-wrap: wrap;
      }
      .inc-hero-icon {
        width: 52px;
        height: 52px;
        border-radius: 16px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: #ffffff;
        background: linear-gradient(135deg, #16a34a, #4ade80);
        box-shadow: 0 12px 20px rgba(34, 197, 94, 0.24);
        flex: 0 0 auto;
      }
      .inc-hero-title {
        font-size: 24px;
        font-weight: 800;
        letter-spacing: -0.02em;
        color: #102033;
        margin: 0;
        display: inline-block;
        white-space: nowrap;
      }
      .inc-hero-copy {
        color: #526173;
        margin: 0;
        line-height: 1.5;
        max-width: 880px;
        overflow-wrap: anywhere;
      }
      .inc-hero-top > div:last-child {
        min-width: 0;
        max-width: 100%;
        overflow-x: auto;
        overflow-y: hidden;
      }
      .inc-options-box.box {
        border-radius: 18px;
        border-top: 4px solid #2563eb;
        box-shadow: 0 12px 26px rgba(15, 23, 42, 0.06);
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        box-sizing: border-box;
      }
      .inc-options-box .box-header,
      .inc-results-card .box-header {
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .inc-options-box .box-title,
      .inc-results-card .box-title {
        display: block;
        white-space: normal;
        word-break: break-word;
        overflow-wrap: anywhere;
        max-width: 100%;
      }
      .inc-options-box .box-body {
        width: 100%;
        max-width: 100%;
        min-width: 0;
        background: #f8fbff;
        overflow-x: auto;
        box-sizing: border-box;
      }
      .inc-options-card {
        background: linear-gradient(180deg, #f8fbff 0%, #eef6ff 100%);
        border: 1px solid #dbe6f3;
        border-radius: 16px;
        padding: 14px 16px 8px 16px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .inc-options-card > div,
      .inc-options-card .shiny-html-output,
      .inc-options-card .table-responsive,
      .inc-options-card .reactable,
      .inc-options-card .rt-table,
      .inc-options-card table {
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .inc-options-card .form-group,
      .inc-options-card .bootstrap-select,
      .inc-options-card .bootstrap-select > .dropdown-toggle,
      .inc-options-card .dropdown-menu {
        width: 100% !important;
        max-width: 100% !important;
        min-width: 0 !important;
        box-sizing: border-box;
      }
      .inc-options-card .bootstrap-select,
      .inc-options-card .bootstrap-select > .dropdown-toggle {
        width: 100% !important;
        max-width: 100% !important;
      }
      .inc-options-card .bootstrap-select .dropdown-menu {
        max-width: 100% !important;
      }
      .inc-options-card .bootstrap-select .dropdown-toggle {
        overflow: hidden;
      }
      .inc-options-card .bootstrap-select .dropdown-toggle .filter-option,
      .inc-options-card .bootstrap-select .dropdown-toggle .filter-option-inner,
      .inc-options-card .bootstrap-select .dropdown-toggle .filter-option-inner-inner {
        max-width: 100% !important;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .inc-results-wrap {
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .inc-results-wrap .nav-tabs,
      .inc-results-wrap .nav-pills {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        border-bottom: none;
      }
      .inc-results-wrap .nav > li {
        float: none;
        margin: 0;
      }
      .inc-results-wrap .nav > li > a {
        border-radius: 999px;
        padding: 10px 16px;
        font-weight: 700;
        white-space: nowrap;
      }
      .inc-results-wrap .nav-pills > li.active > a,
      .inc-results-wrap .nav-pills > li.active > a:focus,
      .inc-results-wrap .nav-pills > li.active > a:hover {
        background: linear-gradient(135deg, #2563eb 0%, #7c3aed 100%);
        box-shadow: 0 10px 18px rgba(37, 99, 235, 0.22);
      }
      .inc-results-wrap .tab-content,
      .inc-results-wrap .tab-pane {
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .inc-results-card {
        border-radius: 20px;
        overflow: hidden;
        box-shadow: 0 16px 32px rgba(15, 23, 42, 0.08);
        border: 1px solid #dbe5f1;
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        box-sizing: border-box;
      }
      .inc-results-card .box-header {
        background: linear-gradient(135deg, #123a63 0%, #1d4ed8 100%);
        color: #ffffff;
        border-bottom: none;
      }
      .inc-results-card .box-title {
        font-weight: 700;
      }
      .inc-results-panel {
        margin-top: 14px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      .inc-plot-panel {
        margin-top: 14px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
        overflow-x: auto;
        overflow-y: hidden;
        box-sizing: border-box;
      }
      /* pickers use container = "body" so the menu escapes the clipping boxes */
      body > .bootstrap-select.dropdown {
        z-index: 1060;
      }
      body > .bootstrap-select .dropdown-menu {
        z-index: 1060;
      }
      body > .bootstrap-select .dropdown-menu .inner {
        max-height: 320px !important;
        overflow-y: auto;
      }
      '
    ),
    shiny::div(
      class = 'inc-viewer-shell',
      shiny::div(
        class = 'inc-hero',
        shiny::div(
          class = 'inc-hero-top',
          shiny::div(
            class = 'inc-hero-icon',
            shiny::icon('chart-line')
          ),
          shiny::div(
            shiny::tags$h2(class = 'inc-hero-title', 'Incidence'),
            shiny::tags$p(
              class = 'inc-hero-copy',
              'Explore incidence rates and plots in a cleaner, more polished layout with consistent styling across characterization modules.'
            )
          )
        )
      ),
      shinydashboard::box(
        collapsible = TRUE,
        title = shiny::tagList(shiny::icon('sliders'), 'Analysis options'),
        width = '100%',
        class = 'inc-options-box',
        shiny::div(
          class = 'inc-options-card',
          shiny::uiOutput(ns("inputOptions"))
        )
      ),
      shiny::conditionalPanel(
        condition = 'output.showIncidence != 0',
        ns = ns,
        shiny::div(
          class = 'inc-results-wrap',
          shiny::tabsetPanel(
            type = 'pills',
            id = ns('incMainPanel'),
            shiny::tabPanel(
              title = 'Incidence Rate Table',
              shiny::div(
                class = 'inc-results-panel',
                shiny::uiOutput(ns("tableFilter")),
                shiny::conditionalPanel(
                  condition = 'output.showTable != 0',
                  ns = ns,
                  shinydashboard::box(
                    class = 'inc-results-card',
                    width = '100%',
                    title = '',
                    resultTableViewer(ns("incidenceRateTable"))
                  )
                )
              )
            ),
            shiny::tabPanel(
              title = 'Incidence Rate Plots',
              shiny::div(
                class = 'inc-plot-panel',
                shiny::uiOutput(ns("plotFilter")),
                shiny::conditionalPanel(
                  condition = 'output.showPlot != 0',
                  ns = ns,
                  shinydashboard::box(
                    class = 'inc-results-card',
                    width = '100%',
                    title = '',
                    shinycssloaders::withSpinner(
                      shiny::plotOutput(
                        ns('incidencePlot'),
                        width = '100%',
                        height = '600px'
                      )
                    )
                  )
                )
              )
            )
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
#' @param resultDatabaseSettings a list containing the characterization result schema, dbms, tablePrefix, databaseTable and cgTablePrefix
#' @param reactiveTargetRow a reactive data.frame with the target of interest details
#' @param reactiveOutcomeTable A reactive data.frame with the outcome table for the target of interest
#' @family Characterization
#' @return
#' The server to the prediction incidence module
#'
#' @export
characterizationIncidenceServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings,
    reactiveTargetRow,
    reactiveOutcomeTable
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
      databases <- OhdsiReportGenerator::getDatabaseDetails(
        connectionHandler = connectionHandler, 
        schema = resultDatabaseSettings$schema, 
        databaseTable = resultDatabaseSettings$databaseTable
      )
      databaseNames <- shiny::reactive({databases$databaseName })
      databaseIds <- shiny::reactive({databases$databaseId })
      
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
            selected = sort(databaseNames())[1],
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              size = 10,
              dropupAuto = FALSE,
              container = "body",
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
        reactiveOutcomeTable()[reactiveOutcomeRowIds(),]
      })
      
      tableSelectionServer(
        id = 'outcome-table-select',
        table = reactiveOutcomeTable, 
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
            targetIds = reactiveTargetRow()$cohortDefinitionId, 
            outcomeIds = reactiveOutcomeRows()$cohortDefinitionId
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
        colDefsInput = characterizationIncidenceColumnDefs(session$ns('incidence-table')), 
        elementId = session$ns('incidence-table'),
        downloadedFileName = "incidenceRateTable-"
      )
      
      
      
      # PLOT FILTER
      output$plotFilter <- shiny::renderUI({
        
        tarChoices <- sort(unique(incidenceFullData()$tar))
        
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
              container = "body",
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
              container = "body",
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Type here to search",
              virtualScroll = 50
            )
          ),
          
          shinyWidgets::pickerInput(
            inputId = session$ns('tarPlot'),
            label = 'Time at risk: ',
            choices = tarChoices,
            selected = tarChoices[1],
            multiple = FALSE,
            options = shinyWidgets::pickerOptions(
              liveSearch = TRUE,
              size = 10,
              dropupAuto = FALSE,
              container = "body",
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Type here to search",
              virtualScroll = 50
            )
          ),
          
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shinyWidgets::pickerInput(
                inputId = session$ns('xAxis'), 
                label = 'Report Type:', 
                choices = c('Age', 'Year'), 
                selected = 'Age', 
                multiple = FALSE,
                options = shinyWidgets::pickerOptions(
                  dropupAuto = FALSE,
                  container = "body"
                )
              )
            ),
            shiny::column(
              width = 3,
              shinyWidgets::pickerInput(
                inputId = session$ns('yScaleType'), 
                label = 'Y-axis scale:', 
                choices = c('Log scale', 'Standard scale'), 
                selected = 'Standard scale', 
                multiple = FALSE,
                options = shinyWidgets::pickerOptions(
                  dropupAuto = FALSE,
                  container = "body"
                )
              )
            ),
            shiny::column(
              width = 3,
              shiny::checkboxInput(
                inputId = session$ns('sexStratifyPlot'), 
                label = 'sex stratify incidence', 
                value = FALSE
              )
            ),
            shiny::column(
              width = 3,
              shiny::checkboxInput(
                inputId = session$ns('scaleVal'), 
                label = 'Fixed y-scale', 
                value = TRUE
              )
            )
            
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
          ) %>%
          dplyr::filter(
            .data$tar %in% input$tarPlot
          )
        
        if(nrow(plotData) > 0){
          
          plotData <- plotData %>% 
            dplyr::select(
              "databaseName", "outcomeName", "ageGroupName", "startYear",
              "genderName", "incidenceRateP100py", "tar", "cleanWindow"
            ) %>% 
            dplyr::mutate(
              facetLabel = paste0(.data$outcomeName, "\n(clean window: ", .data$cleanWindow, ")"),
              # a negative rate means the true rate is below that value (censored small counts)
              valueType = factor(
                x = ifelse(.data$incidenceRateP100py < 0, "Less than the plotted value", "Reported rate"),
                levels = c("Reported rate", "Less than the plotted value")
              ),
              incidenceRateP100py = abs(.data$incidenceRateP100py)
            )
          
          if(xAxis == 'ageGroupName'){
            plotData$ageGroupName <- factor(
              x = plotData$ageGroupName, 
              levels = sortAgeGroupNames(plotData$ageGroupName)
            )
          } else {
            plotData$startYear <- factor(
              x = plotData$startYear, 
              levels = sort(unique(plotData$startYear))
            )
          }
          
          useLogScale <- input$yScaleType == 'Log scale'
          
          if(useLogScale){
            yScale <- ggplot2::scale_y_continuous(
              trans = scales::pseudo_log_trans(base = 10),
              n.breaks = 4
            )
            yName <- "Incidence rate per 100 patient years (log scale)"
            scaleNote <- "the y-axis uses a log-like scale, so equal spacing does not mean equal difference."
          } else {
            yScale <- ggplot2::scale_y_continuous(n.breaks = 5)
            yName <- "Incidence rate per 100 patient years"
            scaleNote <- "the y-axis uses a standard scale, so small rates may be hard to tell apart."
          }
          
          output$incidencePlot <- shiny::renderPlot(
            ggplot2::ggplot(
              data = plotData,
              mapping = ggplot2::aes(x = .data[[xAxis]],
                                     y = .data$incidenceRateP100py,
                                     color = .data[[color]],
                                     group = .data[[color]]
              )
            ) + 
              ggplot2::geom_line(
                linewidth = 0.9,
                alpha = 0.9
              ) +
              ggplot2::geom_point(
                mapping = ggplot2::aes(shape = .data$valueType),
                size = 2.6,
                stroke = 1.1,
                fill = "white"
              ) + 
              ggplot2::facet_grid(
                .data$databaseName ~ .data$facetLabel,
                scales = scaleVal
                ) +
              yScale + 
              ggplot2::scale_color_viridis_d(end = 0.9) +
              ggplot2::scale_shape_manual(
                values = c("Reported rate" = 16, "Less than the plotted value" = 21),
                drop = FALSE
              ) +
              ggplot2::labs(
                title = "Incidence Rates",
                subtitle = paste0("Time at risk: ", input$tarPlot),
                x = xName,
                y = yName,
                color = colorName,
                shape = "Value type",
                caption = paste0(
                  "Notes: ", scaleNote, "\n",
                  "Hollow points are censored small counts: the true rate is somewhere below the plotted value."
                )
              )  +
              ggplot2::theme_bw(base_size = 14) +
              ggplot2::theme(
                legend.position = "bottom",
                legend.title = ggplot2::element_text(face = "bold"),
                plot.title = ggplot2::element_text(face = "bold"),
                plot.subtitle = ggplot2::element_text(color = "grey30"),
                plot.caption = ggplot2::element_text(color = "grey40", hjust = 0),
                panel.grid.minor = ggplot2::element_blank(),
                panel.grid.major.x = ggplot2::element_blank(),
                panel.spacing = ggplot2::unit(1, "lines"),
                strip.background = ggplot2::element_rect(fill = "grey93", color = NA),
                strip.text = ggplot2::element_text(face = "bold", size = 11),
                strip.text.y = ggplot2::element_text(angle = 0),
                axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
              )
          )
        } else{
          output$incidencePlot <- NULL
        }
        
      })
      
      
      
      return(invisible(NULL)) ############# end of server
      
    })
}

# order age group labels such as '0-4', '10-14', '>110' by the age they start at
sortAgeGroupNames <- function(ageGroupNames){
  uniqueNames <- unique(as.character(ageGroupNames))
  startAge <- suppressWarnings(
    as.numeric(sub(pattern = "^[^0-9]*([0-9]+).*$", replacement = "\\1", x = uniqueNames))
  )
  startAge[is.na(startAge)] <- Inf
  uniqueNames[order(startAge, uniqueNames)]
}

# negative counts/rates indicate the true value is censored, so show them as '<value'
formatCensoredValue <- function(value, digits = 0){
  if(length(value) == 0 || is.na(value)){
    return("")
  }
  formatted <- formatC(
    x = abs(value), 
    format = "f", 
    digits = digits, 
    big.mark = ","
  )
  if(value < 0){
    return(paste0("<", formatted))
  }
  formatted
}

characterizationIncidenceColumnDefs <- function(elementId){
  
  list(
    databaseName = reactable::colDef(
      name = "Database",
      header = withTooltip("Database",
                           "The database name")
    ), 
    databaseId = reactable::colDef(
      show = FALSE
    ), 
    targetName = reactable::colDef(
      name = "Target",
      header = withTooltip("Target",
                           "The target cohort name")
    ), 
    targetId = reactable::colDef(
      show = FALSE,
      name = "Target ID",
      header = withTooltip("Target ID",
                           "The target cohort unique identifier")
    ),
    outcomeName = reactable::colDef(
      name = "Outcome",
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
      show = FALSE,
      name = "Outcome ID",
      header = withTooltip("Outcome ID",
                           "The outcome cohort unique identifier")
    ),
    tar = reactable::colDef(
      name = "Time-at-risk",
      header = withTooltip("Time-at-risk",
                           "The time interval where a patient is at risk used to calculate the incidence"),
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
      name = "Age Group",
      header = withTooltip("Age Group",
                           "The age group when stratifying by age")
    ), 
    genderName = reactable::colDef(
      name = "Sex",
      header = withTooltip("Sex",
                           "The sex when stratifying by sex")
    ), 
    startYear = reactable::colDef(
      name = "Index Year",
      header = withTooltip("Index Year",
                           "The index year when stratifying by year")
    ), 
    cleanWindow = reactable::colDef(
      name = "Clean Window",
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
    personsAtRisk = reactable::colDef(
      name = "No. Persons",
      filterable = FALSE,
      header = withTooltip("No. Persons",
                           "The number of people at risk")
    ),
    personDays = reactable::colDef(
      name = "Person Days",
      filterable = FALSE,
      header = withTooltip("Person Days",
                           "The total number of days at risk for all the people at risk")
    ),
    outcomes = reactable::colDef(
      name = "No. Outcomes",
      filterable = FALSE,
      header = withTooltip("No. Outcomes",
                           "The number of outcomes within the people at risk during time-at-risk"),
      cell = function(value){ formatCensoredValue(value = value, digits = 0) }
    ),
    incidenceProportionP100p = reactable::colDef(
      name = "Incidence proportion per 100 persons",
      filterable = FALSE,
      header = withTooltip("Incidence proportion per 100 persons",
                           "The number of outcomes divided by the number of patients multipled by 100"),
      cell = function(value){ formatCensoredValue(value = value, digits = 2) }
    ), 
    incidenceRateP100py = reactable::colDef(
      name = "Incidence rate per 100 person years",
      filterable = FALSE,
      header = withTooltip("Incidence rate per 100 person years",
                           "The number of outcomes divided by the number of person days exposed multipled by 100 times 365"), 
      cell = function(value){ formatCensoredValue(value = value, digits = 2) }
    ),
    personOutcomes = reactable::colDef(
      name = 'No. Person Outcomes',
      cell = function(value){ formatCensoredValue(value = value, digits = 0) }
    ),
    
    personDaysPe = reactable::colDef(
      name = 'Person Days Pre Exclusion'
    ),
    personOutcomesPe = reactable::colDef(
      name = 'Person Outcomes Pre Exclusion'
    ),
    outcomesPe = reactable::colDef(
      name = 'Outcomes Pre Exclusion'
    ),
    personsAtRiskPe = reactable::colDef(
      name = 'Person At Risk Pre Exclusion'
    ),
    tarStartWith = reactable::colDef(show = FALSE),
    tarStartOffset = reactable::colDef(show = FALSE),
    tarEndWith = reactable::colDef(show = FALSE),
    tarEndOffset = reactable::colDef(show = FALSE),
    subgroupName = reactable::colDef(show = FALSE)
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