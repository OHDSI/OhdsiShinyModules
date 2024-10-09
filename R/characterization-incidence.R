# @file characterization-incidence.R
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


# Define the custom age sorting function
custom_age_sort <- function(age_categories) {
  # Extract the largest integer from each category
  largest_integers <- as.integer(sub(".* - (\\d+).*", "\\1", age_categories))
  
  # Create a custom order based on the largest integers
  custom_order <- unique(c("Any", age_categories[order(largest_integers)]))
  
  # Return the sorted age categories
  return(custom_order)
}

break_setter = function(n = 5) {
  function(lims) {pretty(x = as.numeric(lims), n = n)}
}



#' The module viewer for exploring incidence results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @family {Characterization}
#' @return
#' The user interface to the description incidence module
#'
#' @export
characterizationIncidenceViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    
    shiny::uiOutput(ns("inputOptions")),
    
    shiny::conditionalPanel(
      condition = 'input.generate != 0',
      ns = ns,
      
      shiny::tabsetPanel(
        type = 'pills',
        id = ns('incMainPanel'),
        
        shiny::tabPanel(
          title = "Incidence Rate Table",
          resultTableViewer(
            ns("incidenceRateTable"),
            downloadedFileName = "incidenceRateTable-"
            )
        ),
        shiny::tabPanel(
          title = "Incidence Rate Plots",
          shiny::tabsetPanel(
            type = 'pills',
            id = ns('incPlotPanel'),
            shiny::tabPanel(
              title = "Standard Plot (Age Differences)",
              shiny::fluidPage(
                shiny::fluidRow(
                  shiny::column(width = 9, offset = 0, style='padding:0px;'),
                  shiny::column(width = 3,
                                shiny::div(
                                  style = "display:inline-block; float:right",
                                  shiny::downloadButton(ns("downloadPlotStandardAge"),
                                                        "Download Plot",
                                                        icon = shiny::icon("download")
                                  )
                                )
                  ) 
                )
              ),
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  ns('incidencePlotStandardAge'),
                  width="100%",
                  height="600px"
                )
              )
            ),
            shiny::tabPanel(
              title = "Standard Plot (Age & Sex Differences)",
              shiny::fluidPage(
                shiny::fluidRow(
                 shiny::column(width = 9, offset = 0, style='padding:0px;'),
                 shiny::column(width = 3,
                   shiny::div(
                     style = "display:inline-block; float:right",
                     shiny::downloadButton(ns("downloadPlotStandardAgeSex"),
                                           "Download Plot",
                                           icon = shiny::icon("download")
                     )
                   )
                 ) 
                )
              ),
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  ns('incidencePlotStandardAgeSex'),
                  width="100%",
                  height="600px"
                )
              )
            ),
            shiny::tabPanel(
              title = "Standard Plot (Yearly Differences)",
              shiny::fluidPage(
                shiny::fluidRow(
                  shiny::column(width = 9, offset = 0, style='padding:0px;'),
                  shiny::column(width = 3,
                                shiny::div(
                                  style = "display:inline-block; float:right",
                                  shiny::downloadButton(ns("downloadPlotStandardYear"),
                                                        "Download Plot",
                                                        icon = shiny::icon("download")
                                  )
                                )
                  ) 
                )
              ),
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  ns('incidencePlotStandardYear'),
                  width="100%",
                  height="500px"
                )
              )
            ),
            shiny::tabPanel(
              title = "Standard Plot (Aggregate)",
              shiny::fluidPage(
                shiny::fluidRow(
                  shiny::column(width = 9, offset = 0, style='padding:0px;'),
                  shiny::column(width = 3,
                                shiny::div(
                                  style = "display:inline-block; float:right",
                                  shiny::downloadButton(ns("downloadPlotStandardAggregate"),
                                                        "Download Plot",
                                                        icon = shiny::icon("download")
                                  )
                                )
                  ) 
                )
              ),
              
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  ns('incidencePlotStandardAggregate'),
                  width="100%",
                  height="600px"
                )
              )
            ),
            shiny::tabPanel(
              title = "Custom Plot",
              shiny::div(
                inputSelectionViewer(
                  id = ns("input-selection-custom-plot")
                )
              ),
              shiny::conditionalPanel(
                condition = 'input.generate != 0',
                ns = shiny::NS(ns("input-selection-custom-plot")),
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  ns('incidencePlotCustom'),
                  height = "500px"
                )
              ),
              shiny::plotOutput(
                ns('incidencePlotLegendCustom'),
                width="100%",
                height="600px"
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
#' @param parents a list of parent cohorts
#' @param parentIndex an integer specifying the parent index of interest
#' @param outcomes a reactive object specifying the outcomes of interest
#' @param targetIds a reactive vector of integer specifying the targetIds of interest
#' @family {Characterization}
#' @return
#' The server to the prediction incidence module
#'
#' @export
characterizationIncidenceServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings,
    #options, # this gets overwritten in code below - why here?
    parents,
    parentIndex, # reactive
    outcomes, # reactive
    targetIds # reactive
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ##  ns <- session$ns

      ciOptions <- getIncidenceOptions(connectionHandler, resultDatabaseSettings)

      output$inputOptions <- shiny::renderUI({
        shinydashboard::box(
          collapsible = TRUE,
          title = "Options",
          width = "100%",
          
          shiny::div(
            "Select Your Results",
            style = "font-weight: bold; font-size: 20px; text-align: center; margin-bottom: 20px;"
          ),
          
          shinyWidgets::pickerInput(
            inputId = session$ns('outcomeIds'),
            label = 'Outcome: ',
            choices = outcomes(),
            selected = outcomes()[1],
            multiple = T,
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
          
          shinyWidgets::pickerInput(
            inputId = session$ns('databaseSelector'),
            label = 'Filter By Database: ',
            choices = sort(ciOptions$databases),
            selected = ciOptions$databases,
            multiple = T,
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
          
          shinyWidgets::pickerInput(
            inputId = session$ns('ageIds'),
            label = 'Filter By Age Group: ',
            choices = ciOptions$ageGroup,
            selected = ciOptions$ageGroup,
            multiple = T,
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
          
          shinyWidgets::pickerInput(
            inputId = session$ns('sexIds'),
            label = 'Filter By Sex: ',
            choices = ciOptions$sex,
            selected = ciOptions$sex,
            multiple = T,
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
          
          shinyWidgets::pickerInput(
            inputId = session$ns('startYears'),
            label = 'Filter By Start Year: ',
            choices = ciOptions$startYear,
            selected = ciOptions$startYear,
            multiple = T,
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
          
          shinyWidgets::pickerInput(
            inputId = session$ns('cleanWindows'),
            label = 'Select Clean Window',
            choices = ciOptions$cleanWindows,
            selected = ciOptions$cleanWindows,
            multiple = F,
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
          
          shinyWidgets::pickerInput(
            inputId = session$ns('tars'),
            label = 'Select Time at risk (TAR)',
            choices = ciOptions$tar,
            selected = ciOptions$tar[1],
            multiple = F,
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
          
          
          shiny::actionButton(
            inputId = session$ns('generate'), 
            label = 'Generate',
            icon = shiny::icon('redo') 
          )
        )
      })
      
      outcomeIds <- shiny::reactiveVal(NULL)
      incidenceRateTarFilter <- shiny::reactiveVal(NULL)
      incidenceRateCalendarFilter <- shiny::reactiveVal(NULL)
      incidenceRateAgeFilter <- shiny::reactiveVal(NULL)
      incidenceRateGenderFilter <- shiny::reactiveVal(NULL)
      incidenceRateDbFilter <- shiny::reactiveVal(NULL)
      incidenceRateCleanWindowsFilter <- shiny::reactiveVal(NULL)
      shiny::observeEvent(input$generate,{
        incidenceRateTarFilter(names(ciOptions$tar)[(ciOptions$tar == input$tars)]) # filter needs actual value
        incidenceRateCalendarFilter(input$startYears)
        incidenceRateAgeFilter(input$ageIds)
        incidenceRateGenderFilter(input$sexIds)
        incidenceRateDbFilter(input$databaseSelector)
        incidenceRateCleanWindowsFilter(input$cleanWindows)
        outcomeIds(input$outcomeIds)
      })
      
      
      inputSelectedCustomPlot <- inputSelectionServer(
        id = "input-selection-custom-plot", 
        inputSettingList = list(
          createInputSetting(
            rowNumber = 4,                           
            columnWidth = 12,
            varName = 'secondtext',
            inputReturn = T,
            uiFunction = 'shiny::div',
            uiInputs = list(
              "Configure Your Plot",
              style = "font-weight: bold; font-size: 20px; text-align: center; margin-bottom: 20px; margin-top: 20px; "
            )
          ),
          
          # plotting settings 5th row
          
          createInputSetting(
            rowNumber = 5,                           
            columnWidth = 3,
            varName = 'plotYAxis',
            uiFunction = 'shinyWidgets::pickerInput',
            updateFunction = 'shinyWidgets::updatePickerInput',
            uiInputs = list(
              label = 'Y Axis (Numeric) ',
              choices = ciOptions$irPlotNumericChoices,
              selected = "incidenceRateP100py",
              multiple = F,
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
          
          createInputSetting(
            rowNumber = 5,                           
            columnWidth = 3,
            varName = 'plotXAxis',
            uiFunction = 'shinyWidgets::pickerInput',
            updateFunction = 'shinyWidgets::updatePickerInput',
            uiInputs = list(
              label = 'X Axis (Categorical) ',
              choices = ciOptions$irPlotCategoricalChoices,
              selected = "startYear",
              multiple = F,
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
          
          createInputSetting(
            rowNumber = 5,                           
            columnWidth = 3,
            varName = 'plotXTrellis',
            uiFunction = 'shinyWidgets::pickerInput',
            updateFunction = 'shinyWidgets::updatePickerInput',
            uiInputs = list(
              label = 'Row Trellis (Categorical) ',
              choices = ciOptions$irPlotCategoricalChoices,
              selected = "targetName",
              multiple = F,
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
          
          createInputSetting(
            rowNumber = 5,                           
            columnWidth = 3,
            varName = 'plotYTrellis',
            uiFunction = 'shinyWidgets::pickerInput',
            updateFunction = 'shinyWidgets::updatePickerInput',
            uiInputs = list(
              label = 'Column Trellis (Categorical)',
              choices = ciOptions$irPlotCategoricalChoices,
              selected = "outcomeName",
              multiple = F,
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
          
          # row 6
          
          createInputSetting(
            rowNumber = 6,                           
            columnWidth = 3,
            varName = 'plotColor',
            uiFunction = 'shinyWidgets::pickerInput',
            updateFunction = 'shinyWidgets::updatePickerInput',
            uiInputs = list(
              label = 'Color (Categorical)',
              choices = ciOptions$irPlotCategoricalChoices,
              selected = "cdmSourceAbbreviation",
              multiple = F,
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
          createInputSetting(
            rowNumber = 6,                           
            columnWidth = 3,
            varName = 'plotSize',
            uiFunction = 'shinyWidgets::pickerInput',
            updateFunction = 'shinyWidgets::updatePickerInput',
            uiInputs = list(
              label = 'Plot Point Size (Numeric)',
              choices = ciOptions$irPlotNumericChoices,
              selected = "outcomes",
              multiple = F,
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
          createInputSetting(
            rowNumber = 6,                           
            columnWidth = 3,
            varName = 'plotShape',
            uiFunction = 'shinyWidgets::pickerInput',
            updateFunction = 'shinyWidgets::updatePickerInput',
            uiInputs = list(
              label = 'Plot Point Shape (Categorical)',
              choices = ciOptions$irPlotCategoricalChoices,
              selected = "genderName",
              multiple = F,
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
          
          createInputSetting(
            rowNumber = 6,                           
            columnWidth = 3,
            varName = 'irYscaleFixed',
            uiFunction = 'shiny::checkboxInput',
            uiInputs = list(
              label = "Use same y-axis scale across plots?"
            )
          )
        )
      )
      
      
      extractedData <- shiny::reactiveVal()
      shiny::observeEvent(input$generate ,      
                          {
                            if (is.null(targetIds()) |
                                is.null(outcomeIds())
                            ) {
                              shiny::validate("Please wait...")
                            }
                            
                            else if(targetIds()[1] == outcomeIds()[1] &&
                                    length(targetIds())==1 && length(outcomeIds())==1
                            ){
                              shiny::validate("Target and outcome cohorts must differ from each other. Make a different selection.")
                            }
                            
                            else {
                              result <- getIncidenceData(targetIds = targetIds(),
                                                         outcomeIds = outcomeIds(),
                                                         connectionHandler = connectionHandler,
                                                         resultDatabaseSettings = resultDatabaseSettings
                              )
                              extractedData(result)
                            }
                          }
      )
      
      filteredData <- shiny::reactive({
        shiny::req(nrow(extractedData() > 0))
        if(nrow(extractedData()) > 0){
          extractedData() %>%
            dplyr::relocate("tar", .before = "outcomes") %>%
            dplyr::mutate(incidenceProportionP100p = as.numeric(.data$incidenceProportionP100p),
                          incidenceRateP100py = as.numeric(.data$incidenceRateP100py),
                          dplyr::across(dplyr::where(is.numeric), round, 4),
                          targetNameShort = paste("C", .data$targetCohortDefinitionId, sep = "-"),
                          outcomeNameShort = paste("C", .data$outcomeCohortDefinitionId, sep = "-")) %>%
            dplyr::filter(.data$ageGroupId %in% !! incidenceRateAgeFilter() & 
                            .data$genderId %in% !! incidenceRateGenderFilter() & 
                            .data$startYear %in% !! incidenceRateCalendarFilter() & 
                            .data$tar %in% incidenceRateTarFilter() &
                            .data$cdmSourceAbbreviation %in% !! incidenceRateDbFilter() &
                            .data$cleanWindow %in% !! incidenceRateCleanWindowsFilter()
            ) %>%
            dplyr::relocate("targetName", .after = "cdmSourceAbbreviation") %>%
            dplyr::relocate("outcomeName", .after = "targetName") %>%
            dplyr::relocate("targetNameShort", .after = "targetName") %>%
            dplyr::relocate("outcomeNameShort", .after = "outcomeName")
        }
      })
      
      filteredDataAggregateForPlot <-  shiny::reactive({
        if(nrow(extractedData()) > 0){
          extractedData() %>%
            dplyr::relocate("tar", .before = "outcomes") %>%
            dplyr::mutate(incidenceProportionP100p = as.numeric(.data$incidenceProportionP100p),
                          incidenceRateP100py = as.numeric(.data$incidenceRateP100py),
                          dplyr::across(dplyr::where(is.numeric), round, 4),
                          targetNameShort = paste("C", .data$targetCohortDefinitionId, sep = "-"),
                          outcomeNameShort = paste("C", .data$outcomeCohortDefinitionId, sep = "-")) %>%
            dplyr::relocate("targetNameShort", .after = "targetName") %>%
            dplyr::relocate("outcomeNameShort", .after = "outcomeName")
        }
      })
      
      incidenceColList <- .createCiColDefList()
      
      resultTableServer(
        id = "incidenceRateTable",
        df = filteredData,
        selectedCols = c("cdmSourceAbbreviation", "targetName", "targetNameShort", "outcomeName", "outcomeNameShort",
                         "ageGroupName", "genderName", "startYear", "cleanWindow", "tar", "outcomes",
                         "incidenceProportionP100p", "incidenceRateP100py"),
        sortedCols = c("ageGroupName", "genderName", "startYear", "incidenceRateP100py"),
        elementId = "incidence-select",
        colDefsInput = incidenceColList,
        downloadedFileName = "incidenceRateTable-"
      ) 

      '%!in%' <- function(x,y)!('%in%'(x,y))

      #ir plots
      irPlotCustom <- shiny::reactive( # observeEvent generate instead?
        {
          if (is.null(targetIds()) |
              is.null(outcomeIds())) {
            return(data.frame())
          }
          if(nrow(filteredData()) == 0){
            return(FALSE)
          }
          
          ifelse(incidenceRateTarFilter() %in% filteredData()$tar,
                 plotData <- filteredData() %>%
                   dplyr::filter(.data$tar %in% incidenceRateTarFilter()) %>%
                   dplyr::mutate(targetLabel = paste(.data$targetNameShort, " = ", .data$targetName),
                                 outcomeLabel = paste(.data$outcomeNameShort, " = ", .data$outcomeName)
                   ),
                 shiny::validate("Selected TAR is not found in your result data. Revise input selections or select a different TAR.")
          )
          
          # Take the specific tar value you want to plot
          tar_value <- unique(plotData$tar)[1]
          
          # Create a column for the tooltip text
          plotData$tooltip <- with(plotData, paste(
            "Incidence Rate:", incidenceRateP100py, "<br>",
            "Incidence Proportion:", incidenceProportionP100p, "<br>",
            "Outcome ID:", outcomeNameShort, "<br>",
            "Outcome Name:", outcomeName, "<br>",
            "Target ID:", targetNameShort, "<br>",
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
          
          # Get unique target and outcome labels
          unique_target_labels <- strwrap(unique(plotData$targetLabel), width = 300)
          unique_outcome_labels <- strwrap(unique(plotData$outcomeLabel), width = 300)
          
          # Combine all unique values into a final vector
          final_unique_values <- unique(c(unique_target_labels, unique_outcome_labels))
          
          # Create the caption text with line breaks
          caption_text <- paste(final_unique_values, collapse = "\n")
          
          if (inputSelectedCustomPlot()$plotColor == "Target Cohort" | inputSelectedCustomPlot()$plotColor == "Outcome Cohort") {
            color_aesthetic <- if (inputSelectedCustomPlot()$plotColor == "Target Cohort") {
              dplyr::vars(.data$targetNameShort)
            } else if (inputSelectedCustomPlot()$plotColor == "Outcome Cohort") {
              dplyr::vars(.data$outcomeNameShort)
            }
          }
          
          if (inputSelectedCustomPlot()$plotShape == "Target Cohort" | inputSelectedCustomPlot()$plotShape == "Outcome Cohort") {
            shape_aesthetic <- if (inputSelectedCustomPlot()$plotShape == "Target Cohort") {
              dplyr::vars(.data$targetNameShort)
            } else if (inputSelectedCustomPlot()$plotShape == "Outcome Cohort") {
              dplyr::vars(.data$outcomeNameShort)
            }
          }
          
          max_length <- max(nchar(unique(inputSelectedCustomPlot()$plotXAxis)))
          
          if (inputSelectedCustomPlot()$plotXTrellis != inputSelectedCustomPlot()$plotYTrellis | 
              (inputSelectedCustomPlot()$plotXTrellis == "(None)" && inputSelectedCustomPlot()$plotYTrellis == "(None)")){
            
            # Create the base plot with conditional aesthetics
            base_plot <- ggplot2::ggplot(
              data = plotData,
              ggplot2::aes(x = .data[[inputSelectedCustomPlot()$plotXAxis]],
                           y = .data[[inputSelectedCustomPlot()$plotYAxis]],
                           shape = if(inputSelectedCustomPlot()$plotShape != "(None)" & inputSelectedCustomPlot()$plotShape != "Target Cohort" & 
                                      inputSelectedCustomPlot()$plotShape != "Outcome Cohort") .data[[inputSelectedCustomPlot()$plotShape]]
                           else shape_aesthetic,
                           color = if(inputSelectedCustomPlot()$plotColor != "(None)" & inputSelectedCustomPlot()$plotColor != "Target Cohort" & 
                                      inputSelectedCustomPlot()$plotColor != "Outcome Cohort") .data[[inputSelectedCustomPlot()$plotColor]]
                           else color_aesthetic
              )
            ) + 
              ggplot2::geom_point(ggplot2::aes(size = if(inputSelectedCustomPlot()$plotSize != "(None)") .data[[inputSelectedCustomPlot()$plotSize]] else NULL)
              ) + 
              ggplot2::scale_size_continuous(range = c(3,7))
            
            # Rotate x-axis labels if the maximum length is greater than 10
            if (max_length > 10) {
              base_plot <- base_plot + 
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
            }
            
            # Add trellising if it's not NULL
            if (inputSelectedCustomPlot()$plotXTrellis!="(None)" & inputSelectedCustomPlot()$plotXTrellis!="targetName" & inputSelectedCustomPlot()$plotXTrellis!="outcomeName" & 
                inputSelectedCustomPlot()$plotYTrellis!="(None)" & inputSelectedCustomPlot()$plotYTrellis!="targetName" & inputSelectedCustomPlot()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data[[inputSelectedCustomPlot()$plotXTrellis]]),
                cols = dplyr::vars(.data[[inputSelectedCustomPlot()$plotYTrellis]]),
                scales = if (inputSelectedCustomPlot()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) 
            }
            else if (inputSelectedCustomPlot()$plotXTrellis!="(None)" & inputSelectedCustomPlot()$plotXTrellis=="targetName" & inputSelectedCustomPlot()$plotXTrellis!="outcomeName" & 
                     inputSelectedCustomPlot()$plotYTrellis!="(None)" & inputSelectedCustomPlot()$plotYTrellis!="targetName" & inputSelectedCustomPlot()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data$targetNameShort),
                cols = dplyr::vars(.data[[inputSelectedCustomPlot()$plotYTrellis]]),
                scales = if (inputSelectedCustomPlot()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) 
            }
            else if (inputSelectedCustomPlot()$plotXTrellis!="(None)" & inputSelectedCustomPlot()$plotXTrellis!="targetName" & inputSelectedCustomPlot()$plotXTrellis=="outcomeName" & 
                     inputSelectedCustomPlot()$plotYTrellis!="(None)" & inputSelectedCustomPlot()$plotYTrellis!="targetName" & inputSelectedCustomPlot()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data$outcomeNameShort),
                cols = dplyr::vars(.data[[inputSelectedCustomPlot()$plotYTrellis]]),
                scales = if (inputSelectedCustomPlot()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) 
            }
            else if (inputSelectedCustomPlot()$plotXTrellis!="(None)" & inputSelectedCustomPlot()$plotXTrellis!="targetName" & inputSelectedCustomPlot()$plotXTrellis!="outcomeName" & 
                     inputSelectedCustomPlot()$plotYTrellis!="(None)" & inputSelectedCustomPlot()$plotYTrellis=="targetName" & inputSelectedCustomPlot()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data[[inputSelectedCustomPlot()$plotXTrellis]]),
                cols = dplyr::vars(.data$targetNameShort),
                scales = if (inputSelectedCustomPlot()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) 
            }
            else if (inputSelectedCustomPlot()$plotXTrellis!="(None)" & inputSelectedCustomPlot()$plotXTrellis!="targetName" & inputSelectedCustomPlot()$plotXTrellis!="outcomeName" & 
                     inputSelectedCustomPlot()$plotYTrellis!="(None)" & inputSelectedCustomPlot()$plotYTrellis!="targetName" & inputSelectedCustomPlot()$plotYTrellis=="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data[[inputSelectedCustomPlot()$plotXTrellis]]),
                cols = dplyr::vars(.data$outcomeNameShort),
                scales = if (inputSelectedCustomPlot()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) 
            }
            else if (inputSelectedCustomPlot()$plotXTrellis!="(None)" & inputSelectedCustomPlot()$plotXTrellis=="targetName" & inputSelectedCustomPlot()$plotXTrellis!="outcomeName" & 
                     inputSelectedCustomPlot()$plotYTrellis!="(None)" & inputSelectedCustomPlot()$plotYTrellis=="targetName" & inputSelectedCustomPlot()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data$targetNameShort),
                cols = dplyr::vars(.data$targetNameShort),
                scales = if (inputSelectedCustomPlot()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) 
            }
            else if (inputSelectedCustomPlot()$plotXTrellis!="(None)" & inputSelectedCustomPlot()$plotXTrellis=="targetName" & inputSelectedCustomPlot()$plotXTrellis!="outcomeName" & 
                     inputSelectedCustomPlot()$plotYTrellis!="(None)" & inputSelectedCustomPlot()$plotYTrellis!="targetName" & inputSelectedCustomPlot()$plotYTrellis=="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data$targetNameShort),
                cols = dplyr::vars(.data$outcomeNameShort),
                scales = if (inputSelectedCustomPlot()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) 
            }
            else if (inputSelectedCustomPlot()$plotXTrellis!="(None)" & inputSelectedCustomPlot()$plotXTrellis!="targetName" & inputSelectedCustomPlot()$plotXTrellis=="outcomeName" & 
                     inputSelectedCustomPlot()$plotYTrellis!="(None)" & inputSelectedCustomPlot()$plotYTrellis=="targetName" & inputSelectedCustomPlot()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data$outcomeNameShort),
                cols = dplyr::vars(.data$targetNameShort),
                scales = if (inputSelectedCustomPlot()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) 
            }
            else if (inputSelectedCustomPlot()$plotXTrellis!="(None)" & inputSelectedCustomPlot()$plotXTrellis!="targetName" & inputSelectedCustomPlot()$plotXTrellis=="outcomeName" & 
                     inputSelectedCustomPlot()$plotYTrellis!="(None)" & inputSelectedCustomPlot()$plotYTrellis!="targetName" & inputSelectedCustomPlot()$plotYTrellis=="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data$outcomeNameShort),
                cols = dplyr::vars(.data$outcomeNameShort),
                scales = if (inputSelectedCustomPlot()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) 
            }
            else if (inputSelectedCustomPlot()$plotXTrellis=="(None)" & inputSelectedCustomPlot()$plotXTrellis!="targetName" & inputSelectedCustomPlot()$plotXTrellis!="outcomeName" & 
                     inputSelectedCustomPlot()$plotYTrellis!="(None)" & inputSelectedCustomPlot()$plotYTrellis!="targetName" & inputSelectedCustomPlot()$plotYTrellis=="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = NULL,
                cols = dplyr::vars(.data$outcomeNameShort),
                scales = if (inputSelectedCustomPlot()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) 
            }
            else if (inputSelectedCustomPlot()$plotXTrellis=="(None)" & inputSelectedCustomPlot()$plotXTrellis!="targetName" & inputSelectedCustomPlot()$plotXTrellis!="outcomeName" & 
                     inputSelectedCustomPlot()$plotYTrellis!="(None)" & inputSelectedCustomPlot()$plotYTrellis=="targetName" & inputSelectedCustomPlot()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = NULL,
                cols = dplyr::vars(.data$targetNameShort),
                scales = if (inputSelectedCustomPlot()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) 
            }
            else if (inputSelectedCustomPlot()$plotXTrellis=="(None)" & inputSelectedCustomPlot()$plotXTrellis!="targetName" & inputSelectedCustomPlot()$plotXTrellis!="outcomeName" & 
                     inputSelectedCustomPlot()$plotYTrellis!="(None)" & inputSelectedCustomPlot()$plotYTrellis!="targetName" & inputSelectedCustomPlot()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = NULL,
                cols = dplyr::vars(.data[[inputSelectedCustomPlot()$plotYTrellis]]),
                scales = if (inputSelectedCustomPlot()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) 
            }
            else if (inputSelectedCustomPlot()$plotXTrellis!="(None)" & inputSelectedCustomPlot()$plotXTrellis!="targetName" & inputSelectedCustomPlot()$plotXTrellis!="outcomeName" & 
                     inputSelectedCustomPlot()$plotYTrellis=="(None)" & inputSelectedCustomPlot()$plotYTrellis!="targetName" & inputSelectedCustomPlot()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data[[inputSelectedCustomPlot()$plotXTrellis]]),
                cols = NULL,
                scales = if (inputSelectedCustomPlot()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) 
            }
            else if (inputSelectedCustomPlot()$plotXTrellis!="(None)" & inputSelectedCustomPlot()$plotXTrellis=="targetName" & inputSelectedCustomPlot()$plotXTrellis!="outcomeName" & 
                     inputSelectedCustomPlot()$plotYTrellis=="(None)" & inputSelectedCustomPlot()$plotYTrellis!="targetName" & inputSelectedCustomPlot()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data$targetNameShort),
                cols = NULL,
                scales = if (inputSelectedCustomPlot()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) 
            }
            else if (inputSelectedCustomPlot()$plotXTrellis!="(None)" & inputSelectedCustomPlot()$plotXTrellis!="targetName" & inputSelectedCustomPlot()$plotXTrellis=="outcomeName" & 
                     inputSelectedCustomPlot()$plotYTrellis=="(None)" & inputSelectedCustomPlot()$plotYTrellis!="targetName" & inputSelectedCustomPlot()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data$outcomeNameShort),
                cols = NULL,
                scales = if (inputSelectedCustomPlot()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) 
            }
            else if (inputSelectedCustomPlot()$plotXTrellis=="(None)" & inputSelectedCustomPlot()$plotYTrellis=="(None)") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = NULL,
                cols = NULL,
                scales = if (inputSelectedCustomPlot()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) 
            }
            
            
            
            
            # Rest of your ggplot code remains the same
            base_plot <- base_plot + ggplot2::labs(
              title = paste("Incidence Rate for TAR:", tar_value),
              x = names(ciOptions$irPlotCategoricalChoices[ciOptions$irPlotCategoricalChoices %in% inputSelectedCustomPlot()$plotXAxis]),
              y = names(ciOptions$irPlotNumericChoices[ciOptions$irPlotNumericChoices %in% inputSelectedCustomPlot()$plotYAxis]),
              color = names(ciOptions$irPlotCategoricalChoices[ciOptions$irPlotCategoricalChoices %in% inputSelectedCustomPlot()$plotColor]),
              size = names(ciOptions$irPlotNumericChoices[ciOptions$irPlotNumericChoices %in% inputSelectedCustomPlot()$plotSize]),
              shape = names(ciOptions$irPlotCategoricalChoices[ciOptions$irPlotCategoricalChoices %in% inputSelectedCustomPlot()$plotShape]),
              caption = caption_text
            ) +
              ggplot2::scale_y_log10(breaks = scales::breaks_log(n=6)) +
              ggplot2::guides(alpha = "none") + # Remove the alpha legend
              ggplot2::theme_bw() +
              ggplot2::theme(
                plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 10), hjust = 0.5, size = 25, face="bold"),
                axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 30), size = 20),
                axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 30), size = 20),
                axis.text.x = ggplot2::element_text(size = 14, angle = 45, hjust = 0.5, vjust = 0.25),
                axis.text.y = ggplot2::element_text(size = 14),
                legend.position = "right",
                legend.box.spacing = ggplot2::unit(3, "pt"),
                legend.text = ggplot2::element_text(size=10),
                legend.title = ggplot2::element_text(size=16, face = "bold"),
                panel.spacing = ggplot2::unit(2, "lines"),
                strip.text = ggplot2::element_text(face="bold", size = 14),
                plot.caption = ggplot2::element_text(hjust = 0, face = "italic", size = 12,
                                                     margin = ggplot2::margin(t = 20))
              ) + 
              ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 6)),
                              color = ggplot2::guide_legend(override.aes = list(size = 6)))
            
          }
          
          
          else {
            
            shiny::validate("Cannout use the same trellis for row and column, please make another selection.")
            
          }
          
          return(base_plot)
        }
      )
      
      #render the event reactive incidence plot without legend
      renderIrPlotCustomNoLegend <- shiny::reactive(
        {
          if (is.null(targetIds()) |
              is.null(outcomeIds())) {
            shiny::validate("Please select at least one target and one outcome.")
          }
          if(nrow(filteredData()) == 0){
            shiny::validate("No results.")
          }
          
          else {
            plotData <- filteredData() %>%
              dplyr::filter(.data$tar %in% incidenceRateTarFilter())
            
            # Get the number of facets in both rows and columns
            num_rows <- length(unique(plotData[[inputSelectedCustomPlot()$plotXTrellis]]))
            num_cols <- length(unique(plotData[[inputSelectedCustomPlot()$plotYTrellis]]))
            
            max_length <- max(nchar(unique(inputSelectedCustomPlot()$plotXAxis)))
            
            base_plot <- irPlotCustom()
            
            p <- base_plot +
              ggplot2::guides(shape = FALSE, color = FALSE, size = FALSE)
            
            # Convert the ggplot to a plotly object
            p <- plotly::ggplotly(p, tooltip = "text")
            
            # Center the main plot title
            p <- p %>% plotly::layout(title = list(x = 0.5, xanchor = "center"),
                                      margin = list(t = 75, b = 150, l = 125, r = 25),
                                      #add several xaxis placeholders in case row trellis has several distinct values (this is a workaround)
                                      xaxis =  list(tickangle = 45),
                                      xaxis2 =  list(tickangle = 45),
                                      xaxis3 =  list(tickangle = 45),
                                      xaxis4 =  list(tickangle = 45),
                                      xaxis5 =  list(tickangle = 45),
                                      xaxis6 =  list(tickangle = 45),
                                      xaxis7 =  list(tickangle = 45),
                                      xaxis8 =  list(tickangle = 45),
                                      xaxis9 =  list(tickangle = 45),
                                      xaxis10 =  list(tickangle = 45),
                                      xaxis11 =  list(tickangle = 45),
                                      xaxis12 =  list(tickangle = 45),
                                      xaxis13 =  list(tickangle = 45),
                                      xaxis14 =  list(tickangle = 45),
                                      xaxis15 =  list(tickangle = 45)
            ) 
            
            return(p)
            
          }
          
        }
      )
      
      #render the event reactive incidence plot without legend
      renderIrPlotCustom <- shiny::reactive(
        {
          if (is.null(targetIds()) |
              is.null(outcomeIds())) {
            shiny::validate("Please select at least one target and one outcome.")
          }
          if(nrow(filteredData()) == 0){
            shiny::validate("No results.")
          }
          
          else {
            plotData <- filteredData() %>%
              dplyr::filter(.data$tar %in% incidenceRateTarFilter())
            
            # Get the number of facets in both rows and columns
            num_rows <- length(unique(plotData[[inputSelectedCustomPlot()$plotXTrellis]]))
            num_cols <- length(unique(plotData[[inputSelectedCustomPlot()$plotYTrellis]]))
            
            max_length <- max(nchar(unique(inputSelectedCustomPlot()$plotXAxis)))
            
            base_plot <- irPlotCustom()
            
            p <- base_plot
            
            return(p)
            
          }
          
        }
      )
      
      #render the event reactive incidence plot legend only
      renderIrPlotCustomLegend <- shiny::reactive(
        {
          base_plot <- irPlotCustom()
          
          p <- as_ggplot(cowplot::get_legend(base_plot))
          
          return(p)
        }
      )
      
      
      output$incidencePlotCustom <-  
        shiny::renderPlot({
          renderIrPlotCustom()
        })
      
      output$incidencePlotCustomLegend <-  
        shiny::renderPlot({
          renderIrPlotCustomLegend()
        })
      
      
      
      
      targetLabeller <- function(string, prefix = "Target: ") paste0(prefix, string)
      outcomeLabeller <- function(string, prefix = "Outcome: ") paste0(prefix, string)
      ageLabeller <- function(string, prefix = "Age: ") paste0(prefix, string)
      
      
      
      
      
      #by age
      
      renderIrPlotStandardAge <- shiny::reactive({
        
        
        if (is.null(targetIds()) |
            is.null(outcomeIds())) {
          return(data.frame())
        }
        if(nrow(filteredData()) == 0){
          shiny::validate("No results.")
        }
        
        ifelse(incidenceRateTarFilter() %in% filteredData()$tar,
               plotData <- filteredData() %>%
                 dplyr::filter(.data$tar %in% incidenceRateTarFilter()),
               shiny::validate("Selected TAR is not found in your result data. Revise input selections or select a different TAR.")
        )
        
        #add check to make sure facetted plots fit nicely in plotting window (600px). this is currently nrow * ncol in facet_wrap()
        ifelse(length(targetIds()) * length(outcomeIds()) <= 10,
               plotData <- filteredData(),
               shiny::validate("Too many Target-Outcome pairs selected to plot efficiently. Please choose fewer targets and/or outcomes.")
        )
        
        #add check to make sure "> 1 distinct age is selected for by age plot"any" is in selection for year and sex
        ifelse("Any" %in% incidenceRateCalendarFilter() & "Any" %in% incidenceRateGenderFilter(),
               plotData <- filteredData(),
               shiny::validate("This standard plot is designed to show results aggregated over all (`Any`) year and sex categories. Please make sure you have selected `Any` in the `Select your results` section above for these variables.")
        )
        
        plotData <- plotData %>%
          dplyr::filter(#ageGroupName != "Any" & 
            .data$genderName == "Any" & 
              .data$startYear == "Any") %>%
          dplyr::mutate(targetLabel = paste(.data$targetNameShort, " = ", .data$targetName),
                        outcomeLabel = paste(.data$outcomeNameShort, " = ", .data$outcomeName),
                        ageGroupName = factor(.data$ageGroupName, levels = custom_age_sort(.data$ageGroupName), ordered = TRUE)
          ) %>%
          dplyr::rename(Target = "targetNameShort",
                        Outcome = "outcomeNameShort",
                        Age = "ageGroupName")
        
        # Get unique target and outcome labels
        unique_target_labels <- strwrap(unique(plotData$targetLabel), width = 300)
        unique_outcome_labels <- strwrap(unique(plotData$outcomeLabel), width = 300)
        
        # Combine all unique values into a final vector
        final_unique_values <- unique(c(unique_target_labels, unique_outcome_labels))
        
        # Create the caption text with line breaks
        caption_text <- paste(final_unique_values, collapse = "\n")
        
        
        # Take the specific tar value you want to plot
        tar_value <- unique(plotData$tar)[1]
        
        
        base_plot <- ggplot2::ggplot(
          data = plotData,
          ggplot2::aes(x = .data$Age,
                       y = .data$incidenceRateP100py,
                       color = .data$cdmSourceAbbreviation
          )
        ) + 
          ggplot2::geom_point(
            ggplot2::aes(size = 3)
          ) + 
          #geom_jitter() +
          #scale_size_continuous(range = c(5,15)) +
          ggplot2::scale_colour_brewer(palette = "Paired") +
          ggplot2::facet_wrap(
            Target~Outcome,
            labeller = "label_both",
            scales = "free_x",
            nrow = 2,
            ncol = 5
            #,
            #strip.position = "right"
          ) + 
          ggplot2::scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),
                                      n.breaks = 4)
        
        base_plot <- base_plot + ggplot2::labs(
          title = paste("Incidence Rate for Time at Risk:", tar_value),
          x = paste(names(ciOptions$irPlotCategoricalChoices[ciOptions$irPlotCategoricalChoices %in% "ageGroupName"]), "\n"),
          y = names(ciOptions$irPlotNumericChoices[ciOptions$irPlotNumericChoices %in% "incidenceRateP100py"]),
          color = names(ciOptions$irPlotCategoricalChoices[ciOptions$irPlotCategoricalChoices %in% "cdmSourceAbbreviation"]),
          #size = names(options$irPlotNumericChoices[options$irPlotNumericChoices %in% "outcomes"]),
          #shape = names(options$irPlotCategoricalChoices[options$irPlotCategoricalChoices %in% "genderName"]),
          caption = caption_text
        ) +
          ggplot2::guides(alpha = "none", size = "none") + # Remove the alpha and size legend
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 10), hjust = 0.5, size = 25, face="bold"),
            plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b = 20), hjust = 0.5, size = 16),
            axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 25), size = 18),
            axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 25), size = 18),
            axis.text.x = ggplot2::element_text(size = 14, angle = 45, hjust = 0.5, vjust = 0.25),
            axis.text.y = ggplot2::element_text(size = 14),
            legend.position = "bottom",
            legend.box.spacing = ggplot2::unit(3, "pt"),
            legend.text = ggplot2::element_text(size=10),
            legend.title = ggplot2::element_text(size=16, face = "bold"),
            legend.title.align = 0.5,
            plot.caption = ggplot2::element_text(hjust = 0, face = "italic", size = 12,
                                                 margin = ggplot2::margin(t = 20)),
            panel.spacing = ggplot2::unit(2, "lines"),
            strip.text = ggplot2::element_text(face="bold", size = 14),
            strip.background = ggplot2::element_blank(),
            strip.clip = "off"
          ) + 
          ggplot2::guides(#shape = ggplot2::guide_legend(override.aes = list(size = 6)),
            color = ggplot2::guide_legend(override.aes = list(size = 6))
          )
        
        return(base_plot)
        
      })
      
      output$incidencePlotStandardAge<-  
        shiny::renderPlot({
          renderIrPlotStandardAge()
        })
      
      
      # Define a function to save the plot as an image
      output$downloadPlotStandardAge <- shiny::downloadHandler(
        filename = function() {
          paste("standard-age-ir-plot-", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
          cowplot::save_plot(file, plot = renderIrPlotStandardAge(), base_height = 12)
        }
      )

      #by age and sex 
      
      renderIrPlotStandardAgeSex <- shiny::reactive({
        
        if (is.null(targetIds()) |
            is.null(outcomeIds())) {
          return(data.frame())
        }
        if(nrow(filteredData()) == 0){
          return(data.frame())
        }
        
        ifelse(incidenceRateTarFilter() %in% filteredData()$tar,
               plotData <- filteredData() %>%
                 dplyr::filter(.data$tar %in% incidenceRateTarFilter()),
               shiny::validate("Selected TAR is not found in your result data. Revise input selections or select a different TAR.")
        )
        
        #add check to make sure facetted plots fit nicely in plotting window (600px). this is currently nrow * ncol in facet_wrap()
        ifelse(length(targetIds()) * length(outcomeIds()) <= 10,
               plotData <- filteredData(),
               shiny::validate("Too many Target-Outcome pairs selected to plot efficiently. Please choose fewer targets and/or outcomes.")
        )
        
        #add check to make sure "Any" is in the year filter
        ifelse("Any" %in% incidenceRateCalendarFilter(),
               plotData <- filteredData(),
               shiny::validate("This standard plot is designed to show results aggregated over all (`Any`) year categories. Please make sure you have selected `Any` in the `Select your results` section above for this variable.")
        )
        
        #add check to make sure males and females are included
        ifelse(8507 %in% incidenceRateGenderFilter() & 8532 %in% incidenceRateGenderFilter(),
               plotData <- filteredData(),
               shiny::validate("This standard plot is designed to show results stratified by male and female biological sex. Please make sure you have both `Male` and `Female` selected above and try again.")
        )
        
        plotData <- plotData %>%
          dplyr::filter( #ageGroupName != "Any" & 
            .data$genderName != "Any" & 
              .data$startYear == "Any") %>%
          dplyr::mutate(targetLabel = paste(.data$targetNameShort, " = ", .data$targetName),
                        outcomeLabel = paste(.data$outcomeNameShort, " = ", .data$outcomeName),
                        ageGroupName = factor(.data$ageGroupName, levels = custom_age_sort(.data$ageGroupName), ordered = TRUE)
          ) %>%
          dplyr::rename(Target = "targetNameShort",
                        Outcome = "outcomeNameShort",
                        Age = "ageGroupName")
        
        # Get unique target and outcome labels
        unique_target_labels <- strwrap(unique(plotData$targetLabel), width = 300)
        unique_outcome_labels <- strwrap(unique(plotData$outcomeLabel), width = 300)
        
        # Combine all unique values into a final vector
        final_unique_values <- unique(c(unique_target_labels, unique_outcome_labels))
        
        # Create the caption text with line breaks
        caption_text <- paste(final_unique_values, collapse = "\n")
        
        
        # Take the specific tar value you want to plot
        tar_value <- unique(plotData$tar)[1]
        
        base_plot <- ggplot2::ggplot(
          data = plotData,
          ggplot2::aes(x = .data$Age,
                       y = .data$incidenceRateP100py,
                       shape = .data$genderName,
                       color = .data$cdmSourceAbbreviation
          )
        ) + 
          ggplot2::geom_point(
            ggplot2::aes(size = 3)
          ) + 
          #geom_jitter() +
          #scale_size_continuous(range = c(5,15)) +
          ggplot2::scale_colour_brewer(palette = "Paired") +
          ggplot2::facet_wrap(
            Target~Outcome,
            labeller = "label_both",
            scales = "free_x",
            nrow = 2,
            ncol = 5
            #,
            #strip.position = "right"
          ) + 
          ggplot2::scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),
                                      n.breaks = 4)
        
        base_plot <- base_plot + ggplot2::labs(
          title = paste("Incidence Rate for Time at Risk:", tar_value),
          x = paste(names(ciOptions$irPlotCategoricalChoices[ciOptions$irPlotCategoricalChoices %in% "ageGroupName"]), "\n"),
          y = names(ciOptions$irPlotNumericChoices[ciOptions$irPlotNumericChoices %in% "incidenceRateP100py"]),
          color = names(ciOptions$irPlotCategoricalChoices[ciOptions$irPlotCategoricalChoices %in% "cdmSourceAbbreviation"]),
          #size = names(options$irPlotNumericChoices[options$irPlotNumericChoices %in% "outcomes"]),
          shape = names(ciOptions$irPlotCategoricalChoices[ciOptions$irPlotCategoricalChoices %in% "genderName"]),
          caption = caption_text
        ) +
          ggplot2::guides(alpha = "none", size = "none") + # Remove the alpha and size legend
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 10), hjust = 0.5, size = 25, face="bold"),
            plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b = 20), hjust = 0.5, size = 16),
            axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 25), size = 18),
            axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 25), size = 18),
            axis.text.x = ggplot2::element_text(size = 14, angle = 45, hjust = 0.5, vjust = 0.25),
            axis.text.y = ggplot2::element_text(size = 14),
            legend.position = "bottom",
            legend.box.spacing = ggplot2::unit(3, "pt"),
            legend.text = ggplot2::element_text(size=10),
            legend.title = ggplot2::element_text(size=16, face = "bold"),
            legend.title.align = 0.5,
            plot.caption = ggplot2::element_text(hjust = 0, face = "italic", size = 12,
                                                 margin = ggplot2::margin(t = 20)),
            panel.spacing = ggplot2::unit(2, "lines"),
            strip.text = ggplot2::element_text(face="bold", size = 14),
            strip.background = ggplot2::element_blank(),
            strip.clip = "off"
          ) + 
          ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 6)),
                          color = ggplot2::guide_legend(override.aes = list(size = 6))
          )
        
        return(base_plot)
        
      })
      
      output$incidencePlotStandardAgeSex<-  
        shiny::renderPlot({
          renderIrPlotStandardAgeSex()
        })
      
      
      # Define a function to save the plot as an image
      output$downloadPlotStandardAgeSex <- shiny::downloadHandler(
        filename = function() {
          paste("standard-age-sex-ir-plot-", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
          cowplot::save_plot(file, plot = renderIrPlotStandardAgeSex(), base_height = 12)
        }
      )
      
      
      
      # by calendar year
      renderIrPlotStandardYear <- shiny::reactive({
        
        if (is.null(targetIds()) |
            is.null(outcomeIds())) {
          return(data.frame())
        }
        if(nrow(filteredData()) == 0){
          return(data.frame())
        }
        
        ifelse(incidenceRateTarFilter() %in% filteredData()$tar,
               plotData <- filteredData() %>%
                 dplyr::filter(.data$tar %in% incidenceRateTarFilter()),
               shiny::validate("Selected TAR is not found in your result data. Revise input selections or select a different TAR.")
        )
        
        ifelse(length(incidenceRateTarFilter() %in% filteredData()$tar) == 1,
               plotData <- filteredData() %>%
                 dplyr::filter(.data$tar %in% incidenceRateTarFilter()),
               shiny::validate("Please select only one TAR at a time to view yearly plots.")
        )
        
        ifelse((length(targetIds()) == 1) & 
                 (length(outcomeIds()) == 1), 
               plotData <- plotData,
               shiny::validate("Please select only one Target and Outcome at a time to view yearly plots.")
        )
        
        ifelse((length(incidenceRateCalendarFilter()) == 1) & 
                 (incidenceRateCalendarFilter() == "Any"), 
               shiny::validate("Please select at least one start year besides `Any`. This plot depicts calendar trends over time on the x-axis, so at least one distinct year is required."),
               plotData <- plotData
        )
        
        
        
        plotData <- plotData %>%
          dplyr::filter(.data$genderName != "Any" & 
                          .data$startYear != "Any") %>%
          dplyr::mutate(targetLabel = paste(.data$targetNameShort, " = ", .data$targetName),
                        outcomeLabel = paste(.data$outcomeNameShort, " = ", .data$outcomeName),
                        ageGroupName = factor(.data$ageGroupName, levels = custom_age_sort(.data$ageGroupName), ordered = TRUE)
          ) %>%
          dplyr::rename(Target = "targetNameShort",
                        Outcome = "outcomeNameShort",
                        Age = "ageGroupName")
        
        #get unique shorthand cohort name
        unique_target <- unique(plotData$Target)
        unique_outcome <- unique(plotData$Outcome)
        
        
        # Get unique target and outcome labels
        unique_target_labels <- strwrap(unique(plotData$targetLabel), width = 300)
        unique_outcome_labels <- strwrap(unique(plotData$outcomeLabel), width = 300)
        
        # Combine all unique values into a final vector
        final_unique_values <- unique(c(unique_target_labels, unique_outcome_labels))
        
        # Create the caption text with line breaks
        caption_text <- paste(final_unique_values, collapse = "\n")
        
        
        # Take the specific tar value you want to plot
        tar_value <- unique(plotData$tar)[1]
        
        plotData <- plotData %>%
          dplyr::filter("Any" %!in% .data$startYear) %>%
          dplyr::mutate(startYear = as.Date(paste0(.data$startYear, "-01-01"))
          )
        
        base_plot <- ggplot2::ggplot(
          data = plotData,
          ggplot2::aes(x = .data$startYear,
                       y = .data$incidenceRateP100py,
                       shape = .data$genderName,
                       color = .data$cdmSourceAbbreviation,
                       group = interaction(.data$cdmSourceAbbreviation, .data$genderName)
          )
        ) + 
          ggplot2::geom_point(
            ggplot2::aes(size = 2.5)
          ) + 
          ggplot2::geom_line(ggplot2::aes(linetype = .data$genderName)) +
          ggplot2::scale_colour_brewer(palette = "Paired") +
          ggplot2::facet_wrap(
            ~Age,
            labeller = "label_both",
            scales = "free_x",
            nrow = 2
          ) +
          ggplot2::scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),
                                      n.breaks = 3) +
          ggplot2::scale_x_date(breaks= seq(min(plotData$startYear), max(plotData$startYear), by = "3 years"),
                                date_labels = "%Y",
                                limits = c(min(plotData$startYear),
                                           max(plotData$startYear))
          )
        
        base_plot <- base_plot + ggplot2::labs(
          title = paste("Incidence Rate for Time at Risk:", tar_value),
          subtitle = paste("Target = ", unique_target, "; Outcome = ", unique_outcome, sep = ""),
          x = paste(names(ciOptions$irPlotCategoricalChoices[ciOptions$irPlotCategoricalChoices %in% "startYear"]), "\n"),
          y = paste(names(ciOptions$irPlotNumericChoices[ciOptions$irPlotNumericChoices %in% "incidenceRateP100py"]), " (log10 scale)"),
          color = names(ciOptions$irPlotCategoricalChoices[ciOptions$irPlotCategoricalChoices %in% "cdmSourceAbbreviation"]),
          shape = names(ciOptions$irPlotCategoricalChoices[ciOptions$irPlotCategoricalChoices %in% "genderName"]),
          caption = caption_text
        ) +
          ggplot2::guides(alpha = "none", size = "none", linetype = "none") + # Remove the alpha and size legend
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 10), hjust = 0.5, size = 25, face="bold"),
            plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b = 20), hjust = 0.5, size = 16),
            axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 25), size = 18),
            axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 25), size = 18),
            axis.text.x = ggplot2::element_text(size = 14, angle = 45, hjust = 0.5, vjust = 0.25),
            axis.text.y = ggplot2::element_text(size = 14),
            legend.position = "bottom",
            legend.box.spacing = ggplot2::unit(3, "pt"),
            legend.text = ggplot2::element_text(size=10),
            legend.title = ggplot2::element_text(size=16, face = "bold"),
            legend.title.align = 0.5,
            plot.caption = ggplot2::element_text(hjust = 0, face = "italic", size = 12,
                                                 margin = ggplot2::margin(t = 20)),
            panel.spacing = ggplot2::unit(2, "lines"),
            strip.text = ggplot2::element_text(face="bold", size = 14),
            strip.background = ggplot2::element_blank(),
            strip.clip = "off"
          ) + 
          ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 6)),
                          color = ggplot2::guide_legend(override.aes = list(size = 6))
          )
        
        return(base_plot)
        
      })
      
      output$incidencePlotStandardYear<-  
        shiny::renderPlot({
          renderIrPlotStandardYear()
        })
      
      
      # Define a function to save the plot as an image
      output$downloadPlotStandardYear <- shiny::downloadHandler(
        filename = function() {
          paste("standard-yearly-ir-plot-", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
          cowplot::save_plot(file, plot = renderIrPlotStandardYear(), base_height = 24)
        }
      )
      

      #aggregate (unstratified)
      
      renderIrPlotStandardAggregate <- shiny::reactive({
        
        if (is.null(targetIds()) |
            is.null(outcomeIds())) {
          return(data.frame())
        }
        if(nrow(filteredData()) == 0){
          return(data.frame())
        }
        
        ifelse(incidenceRateTarFilter() %in% filteredData()$tar,
               plotData <- filteredDataAggregateForPlot() %>%
                 dplyr::filter(.data$tar %in% incidenceRateTarFilter()),
               shiny::validate("Selected TAR is not found in your result data. Revise input selections or select a different TAR.")
        )
        
        #add check to make sure facetted plots fit nicely in plotting window (600px). this is currently nrow * ncol in facet_wrap()
        ifelse(length(targetIds()) * length(outcomeIds()) <= 10,
               plotData <- filteredData(),
               shiny::validate("Too many Target-Outcome pairs selected to plot efficiently. Please choose fewer targets and/or outcomes.")
        )
        
        ifelse("Any" %in% incidenceRateAgeFilter() & 
                 "Any" %in% incidenceRateGenderFilter() & 
                 "Any" %in% incidenceRateCalendarFilter(),
               plotData <- filteredData(),
               shiny::validate("This plot requires the `Any` category to be selected to aggregate over all ages, sexes, and years. Please ensure `Any` is selected in each of these inputs above and try again.")
        )
        
        plotData <- plotData %>%
          dplyr::filter(.data$ageGroupName == "Any" & 
                          .data$genderName == "Any") %>%
          dplyr::mutate(targetLabel = paste(.data$targetNameShort, " = ", .data$targetName),
                        outcomeLabel = paste(.data$outcomeNameShort, " = ", .data$outcomeName)
          ) %>%
          dplyr::rename(Target = "targetNameShort",
                        Outcome = "outcomeNameShort",
                        Age = "ageGroupName")
        
        # Get unique target and outcome labels
        unique_target_labels <- strwrap(unique(plotData$targetLabel), width = 300)
        unique_outcome_labels <- strwrap(unique(plotData$outcomeLabel), width = 300)
        
        # Combine all unique values into a final vector
        final_unique_values <- unique(c(unique_target_labels, unique_outcome_labels))
        
        # Create the caption text with line breaks
        caption_text <- paste(final_unique_values, collapse = "\n")
        
        
        # Take the specific tar value you want to plot
        tar_value <- unique(plotData$tar)[1]
        
        base_plot <- ggplot2::ggplot(
          data = plotData,
          ggplot2::aes(x = .data$startYear,
                       y = .data$incidenceRateP100py,
                       color = .data$cdmSourceAbbreviation
          )
        ) + 
          ggplot2::geom_point(
            ggplot2::aes(size = 3)
          ) + 
          #ggplot2::geom_jitter() +
          #scale_size_continuous(range = c(5,15)) +
          ggplot2::scale_colour_brewer(palette = "Paired") +
          ggplot2::facet_wrap(
            Target~Outcome,
            labeller = "label_both",
            scales = "free_x",
            nrow = 2,
            ncol = 5
            #,
            #strip.position = "right"
          ) + 
          ggplot2::scale_y_continuous(trans=scales::pseudo_log_trans(base = 10),
                                      n.breaks = 4)
        
        base_plot <- base_plot + ggplot2::labs(
          title = paste("Incidence Rate for Time at Risk:", tar_value),
          x = paste(names(ciOptions$irPlotCategoricalChoices[ciOptions$irPlotCategoricalChoices %in% "startYear"]), "\n"),
          y = names(ciOptions$irPlotNumericChoices[ciOptions$irPlotNumericChoices %in% "incidenceRateP100py"]),
          color = names(ciOptions$irPlotCategoricalChoices[ciOptions$irPlotCategoricalChoices %in% "cdmSourceAbbreviation"]),
          caption = caption_text
        ) +
          ggplot2::guides(alpha = "none", size = "none") + # Remove the alpha and size legend
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 10), hjust = 0.5, size = 25, face="bold"),
            plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b = 20), hjust = 0.5, size = 16),
            axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 25), size = 18),
            axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 25), size = 18),
            axis.text.x = ggplot2::element_text(size = 14, angle = 45, hjust = 0.5, vjust = 0.25),
            axis.text.y = ggplot2::element_text(size = 14),
            legend.position = "bottom",
            legend.box.spacing = ggplot2::unit(3, "pt"),
            legend.text = ggplot2::element_text(size=10),
            legend.title = ggplot2::element_text(size=16, face = "bold"),
            legend.title.align = 0.5,
            plot.caption = ggplot2::element_text(hjust = 0, face = "italic", size = 12,
                                                 margin = ggplot2::margin(t = 20)),
            panel.spacing = ggplot2::unit(2, "lines"),
            strip.text = ggplot2::element_text(face="bold", size = 14),
            strip.background = ggplot2::element_blank(),
            strip.clip = "off"
          ) + 
          ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 6)),
                          color = ggplot2::guide_legend(override.aes = list(size = 6)))
        
        return(base_plot)
        
      })
      
      output$incidencePlotStandardAggregate <-  
        shiny::renderPlot({
          renderIrPlotStandardAggregate()
        })
      
      
      # Define a function to save the plot as an image
      output$downloadPlotStandardAggregate <- shiny::downloadHandler(
        filename = function() {
          paste("standard-aggregate-ir-plot-", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
          cowplot::save_plot(file, plot = renderIrPlotStandardAggregate(), base_height = 12)
        }
      )
      
      return(invisible(NULL)) ############# end of server
      
    })
}

#------------
#------------ Fetching data functions  
#------------

getIncidenceData <- function(
    targetIds,
    outcomeIds,
    connectionHandler,
    resultDatabaseSettings
){
  if(!is.null(targetIds) & !is.null(outcomeIds)){
    
    shiny::withProgress(message = 'Getting incidence data', value = 0, {
    
    sql <- 'select d.cdm_source_abbreviation, i.*, ct1.cohort_name as target_name, ct2.cohort_name as outcome_name
from (
  select od.outcome_cohort_definition_id, od.clean_window, agd.age_group_name, 
    tad.tar_start_with, tad.tar_start_offset, tad.tar_end_with, tad.tar_end_offset,
    sd.subgroup_name, i.*
  from @result_schema.@incidence_table_prefixINCIDENCE_SUMMARY i
  join @result_schema.@incidence_table_prefixOUTCOME_DEF od on i.outcome_id = od.outcome_id
    and i.ref_id = od.ref_id
  join @result_schema.@incidence_table_prefixTAR_DEF tad on i.tar_id = tad.tar_id
    and i.ref_id = tad.ref_id
  join @result_schema.@incidence_table_prefixSUBGROUP_DEF sd on i.subgroup_id = sd.subgroup_id
    and i.ref_id = sd.ref_id
  left join @result_schema.@incidence_table_prefixAGE_GROUP_DEF agd on i.age_group_id = agd.age_group_id
    and i.ref_id = agd.ref_id
) i
inner join @result_schema.@database_table_name d
    on d.database_id = i.database_id
inner join @result_schema.@cg_table_prefixcohort_definition ct1
    on ct1.cohort_definition_id = i.target_cohort_definition_id
inner join @result_schema.@cg_table_prefixcohort_definition ct2
    on ct2.cohort_definition_id = i.outcome_cohort_definition_id
where i.target_cohort_definition_id in (@target_ids)
  and i.outcome_cohort_definition_id in (@outcome_ids);'
    
    shiny::incProgress(1/2, detail = paste("Created SQL - Extracting..."))
    
    resultTable <- connectionHandler$queryDb(
      sql = sql, 
      result_schema = resultDatabaseSettings$schema,
      incidence_table_prefix = resultDatabaseSettings$incidenceTablePrefix,
      cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
      target_ids = paste(as.double(targetIds), collapse = ','),
      outcome_ids = paste(as.double(outcomeIds), collapse = ','),
      database_table_name = resultDatabaseSettings$databaseTable
    )
    
    shiny::incProgress(2/2, detail = paste("Extracted ", nrow(resultTable)," rows"))
    
    })
    
    if(nrow(resultTable)>0){
      
      # format the tar
      ##Jenna edit resultTable$tar <- paste0('(',resultTable$tarStartWith, " + ", resultTable$tarStartOffset, ') - (', resultTable$tarEndWith, " + ", resultTable$tarEndOffset, ')')
      resultTable$tar <- cohortIncidenceFormatTar(resultTable)
      
      resultTable <- resultTable %>% 
        dplyr::select(-c("tarStartWith","tarStartOffset","tarEndWith","tarEndOffset", "tarId", "subgroupName"))
      
      resultTable[is.na(resultTable)] <- 'Any'
      resultTable <- unique(resultTable)
    }
    return(resultTable)
  } else{
    return(NULL)
  }
}

# Jenna added
cohortIncidenceFormatTar <- function(x){
  result <- paste0('(',x$tarStartWith, " + ", x$tarStartOffset, ') - (', x$tarEndWith, " + ", x$tarEndOffset, ')')
  return(result)
}

getIncidenceOptions <- function(connectionHandler,
                                resultDatabaseSettings){

  # database options
  databaseDf <- connectionHandler$queryDb(
    sql = 'select database_id, cdm_source_abbreviation from @result_schema.@database_table_name;', 
    result_schema = resultDatabaseSettings$schema,
    database_table_name = resultDatabaseSettings$databaseTable
  )
  databases <- databaseDf$cdmSourceAbbreviation

  # Age Gruop Options
  ageGroupDf <- connectionHandler$queryDb(
    sql = 'select age_group_id, age_group_name from @result_schema.@incidence_table_prefixAGE_GROUP_DEF;', 
    result_schema = resultDatabaseSettings$schema,
    incidence_table_prefix = resultDatabaseSettings$incidenceTablePrefix
  )
  ageGroupDf <- rbind(data.frame(ageGroupId = 'Any', ageGroupName = 'Any'), ageGroupDf)
  ageGroup <- ageGroupDf$ageGroupId
  names(ageGroup) <- ageGroupDf$ageGroupName
  
  sex <- c(8507, 8532 , 'Any')
  names(sex) <- c('Male', 'Female', 'Any')
  
  startYear <- c('Any', format(Sys.Date(), "%Y"):1990)
  names(startYear) <- c('Any', format(Sys.Date(), "%Y"):1990)
  
  # get tar and then call cohortIncidenceFormatTar()
  
  tarDf <- characterizationGetCiTars(connectionHandler,resultDatabaseSettings)
  
  tar <- tarDf$tarId
  names(tar) <- cohortIncidenceFormatTar(tarDf)
  
  sql <- '
select outcome_id, outcome_name 
from @result_schema.@incidence_table_prefixOUTCOME_DEF
'
  outcomeDf <- connectionHandler$queryDb(
    sql = sql, 
    result_schema = resultDatabaseSettings$schema,
    incidence_table_prefix = resultDatabaseSettings$incidenceTablePrefix
  )
  outcomes <- outcomeDf$outcomeId
  names(outcomes) <- outcomeDf$outcomeName
  
  #getting clean window options
  sql <- '
select clean_window 
from @result_schema.@incidence_table_prefixOUTCOME_DEF
'
  cleanWindowDf <- connectionHandler$queryDb(
    sql = sql, 
    result_schema = resultDatabaseSettings$schema,
    incidence_table_prefix = resultDatabaseSettings$incidenceTablePrefix
  )
  cleanWindows <- cleanWindowDf$cleanWindow
  names(cleanWindows) <- cleanWindowDf$cleanWindow

  
  irPlotCategoricalChoices <- list(
    "cdmSourceAbbreviation",
    "ageGroupName",
    "genderName",
    "startYear",
    "targetName",
    "outcomeName",
    "tar",
    "cleanWindow",
    "(None)"
  )
  names(irPlotCategoricalChoices) <- c(
    "Data Source", 
    "Age Group", 
    "Sex", 
    "Calendar Year", 
    "Target Cohort",
    "Outcome Cohort", 
    "TAR", 
    "Clean Window",
    "(None)"
  )
  
  irPlotNumericChoices <- list(
    "incidenceRateP100py",
    "incidenceProportionP100p",
    "outcomes",
    "outcomesPe",
    "personOutcomes",
    "personOutcomesPe",
    "personsAtRisk",
    "personsAtRiskPe",
    "personDays",
    "personDaysPe",
    "(None)"
  )
  names(irPlotNumericChoices) <- c(
    "Incidence Rate (per 100PY)", 
    "Incidence Proportion (per 100P)", 
    "Outcomes", 
    "Outcomes PE",
    "Person Outcomes", 
    "Person Outcomes PE", 
    "Persons At Risk", 
    "Persons At Risk PE", 
    "Person Days", 
    "Person Days PE",
    "(None)"
  )
  
  return(
    list(
      databases = databases,
      ageGroup = ageGroup,
      sex = sex,
      startYear = startYear,
      tar = tar,
      outcomes = outcomes,
      cleanWindows = cleanWindows,
      irPlotNumericChoices = irPlotNumericChoices,
      irPlotCategoricalChoices = irPlotCategoricalChoices
    )
  )
  
}

characterizationGetCiTars <- function(connectionHandler,
                                      resultDatabaseSettings){
  sql <- "SELECT TAR_ID, TAR_START_WITH, TAR_START_OFFSET,
          TAR_END_WITH, TAR_END_OFFSET
          from @schema.@ci_table_prefixtar_def;"
  tars <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    ci_table_prefix = resultDatabaseSettings$incidenceTablePrefix
  )
  return(tars)
}

.createCiColDefList <- function() {
  colDefCsv <- readr::read_csv(system.file("components-columnInformation",
                           "characterization-incidence-colDefs.csv",
                           package = "OhdsiShinyModules"),
                           show_col_types = FALSE)
  
  createCustomColDefList(
      rawColNames = colDefCsv$colName,
      niceColNames = colDefCsv$niceName,
      tooltipText = colDefCsv$toolTip
    )
}
