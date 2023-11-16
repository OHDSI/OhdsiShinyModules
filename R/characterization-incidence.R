# @file characterization-incidence.R
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

    infoHelperViewer(
      id = "helper",
      helpLocation= system.file("characterization-www", "help-incidenceRate.html", package = utils::packageName())
    ),
    
    inputSelectionViewer(
      id = ns("input-selection")
    ),
    
    shiny::conditionalPanel(
      condition = 'input.generate != 0',
      ns = shiny::NS(ns("input-selection")),
      
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
              title = "Custom Plot",
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                  ns('incidencePlot'),
                  height = "1000px"
                )
              ),
              shiny::plotOutput(
                ns('incidencePlotLegend'),
                width="100%",
                height="300px"
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
#' 
#' @return
#' The server to the prediction incidence module
#'
#' @export
characterizationIncidenceServer <- function(
    id, 
    connectionHandler,
    resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      ##  ns <- session$ns
      
     options <- getIncidenceOptions( # written using getTargetOutcomes
       connectionHandler,
       resultDatabaseSettings
     )
     
        # input selection component
        inputSelected <- inputSelectionServer(
          id = "input-selection", 
          inputSettingList = list(
            createInputSetting(
              rowNumber = 1,                           
              columnWidth = 12,
              varName = 'firsttext',
              inputReturn = F,
              uiFunction = 'shiny::div',
              uiInputs = list(
                "Select Your Results",
                style = "font-weight: bold; font-size: 20px; text-align: center; margin-bottom: 20px;"
              )
            ),
            createInputSetting(
              rowNumber = 2,                           
              columnWidth = 6,
              varName = 'targetIds',
              uiFunction = 'shinyWidgets::pickerInput',
              updateFunction = 'shinyWidgets::updatePickerInput',
              uiInputs = list(
                label = 'Target: ',
                choices = options$targetIds,
                selected = options$targetIds[1],
                multiple = T,
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
              rowNumber = 2,                           
              columnWidth = 6,
              varName = 'outcomeIds',
              uiFunction = 'shinyWidgets::pickerInput',
              updateFunction = 'shinyWidgets::updatePickerInput',
              uiInputs = list(
                label = 'Outcome: ',
                choices = options$outcomeIds,
                selected = options$outcomeIds[1],
                multiple = T,
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
            
            # third row
            createInputSetting(
              rowNumber = 3,                           
              columnWidth = 4,
              varName = 'incidenceRateAgeFilter',
              uiFunction = 'shinyWidgets::pickerInput',
              updateFunction = 'shinyWidgets::updatePickerInput',
              collapse = T,
              uiInputs = list(
                label = 'Filter By Age: ',
                choices = options$ageGroupName,
                selected = options$ageGroupName,
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
              )
            ),
            
            
            createInputSetting(
              rowNumber = 3,                           
              columnWidth = 4,
              varName = 'incidenceRateGenderFilter',
              uiFunction = 'shinyWidgets::pickerInput',
              updateFunction = 'shinyWidgets::updatePickerInput',
              collapse = T,
              uiInputs = list(
                label = 'Filter By Sex: ',
                choices = options$genderName,
                selected = options$genderName,
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
              )
            ),
            
            createInputSetting(
              rowNumber = 3,                           
              columnWidth = 4,
              varName = 'incidenceRateCalendarFilter',
              uiFunction = 'shinyWidgets::pickerInput',
              updateFunction = 'shinyWidgets::updatePickerInput',
              collapse = T,
              uiInputs = list(
                label = 'Filter By Start Year: ',
                choices = options$startYear,
                selected = options$startYear,
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
              )
            ),
            
            # 4th row text
            createInputSetting(
              rowNumber = 4,                           
              columnWidth = 12,
              varName = 'secondtext',
              inputReturn = F,
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
                choices = options$irPlotNumericChoices,
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
                choices = options$irPlotCategoricalChoices,
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
                choices = options$irPlotCategoricalChoices,
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
                choices = options$irPlotCategoricalChoices,
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
              columnWidth = 4,
              varName = 'plotColor',
              uiFunction = 'shinyWidgets::pickerInput',
              updateFunction = 'shinyWidgets::updatePickerInput',
              uiInputs = list(
                label = 'Color (Categorical)',
                choices = options$irPlotCategoricalChoices,
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
              columnWidth = 4,
              varName = 'plotSize',
              uiFunction = 'shinyWidgets::pickerInput',
              updateFunction = 'shinyWidgets::updatePickerInput',
              uiInputs = list(
                label = 'Plot Point Size (Numeric)',
                choices = options$irPlotNumericChoices,
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
              columnWidth = 4,
              varName = 'plotShape',
              uiFunction = 'shinyWidgets::pickerInput',
              updateFunction = 'shinyWidgets::updatePickerInput',
              uiInputs = list(
                label = 'Plot Point Shape (Categorical)',
                choices = options$irPlotCategoricalChoices,
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
            
            # row 7
            
            createInputSetting(
              rowNumber = 7,                           
              columnWidth = 8,
              varName = 'incidenceRateTarFilter',
              uiFunction = 'shinyWidgets::pickerInput',
              updateFunction = 'shinyWidgets::updatePickerInput',
              uiInputs = list(
                label = 'Select Time at risk (TAR)',
                choices = options$tar,
                selected = options$tar[1],
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
              rowNumber = 7,                           
              columnWidth = 4,
              varName = 'irYscaleFixed',
              uiFunction = 'shiny::checkboxInput',
              uiInputs = list(
                label = "Use same y-axis scale across plots?"
                )
            )

          
            
            )
        )
      
      
      filteredData <- shiny::reactive(         
        {
          if (is.null(inputSelected()$targetIds) |
              is.null(inputSelected()$outcomeIds)) {
            return(data.frame())
          }
          
          getIncidenceData(targetIds = inputSelected()$targetIds,
                           outcomeIds = inputSelected()$outcomeIds,
                           connectionHandler = connectionHandler,
                           resultDatabaseSettings = resultDatabaseSettings
          ) %>%
            dplyr::relocate("tar", .before = "outcomes") %>%
            dplyr::mutate(incidenceProportionP100p = as.numeric(.data$incidenceProportionP100p),
                          incidenceRateP100py = as.numeric(.data$incidenceRateP100py),
                          dplyr::across(dplyr::where(is.numeric), round, 4),
                          targetIdShort = paste("C", .data$targetCohortDefinitionId, sep = ":"),
                          outcomeIdShort = paste("C", .data$outcomeCohortDefinitionId, sep = ":")) %>%
            dplyr::filter(.data$ageGroupName %in% !!inputSelected()$incidenceRateAgeFilter & 
                            .data$genderName %in% !!inputSelected()$incidenceRateGenderFilter & 
                            .data$startYear %in% !!inputSelected()$incidenceRateCalendarFilter  
            )
        }
      )


      incidenceColList <- ParallelLogger::loadSettingsFromJson(
        system.file("components-columnInformation",
                    "characterization-incidence-colDefs.json",
                    package = "OhdsiShinyModules"
        )
      )
      
      ## CHECK - caused error for me but it is in Nate's latest code
      class(incidenceColList$genderName$filterMethod) <- "JS_EVAL"
      
      renderIrTable <- shiny::reactive(
        {
          filteredData()
        }
      )
      
      resultTableServer(
        id = "incidenceRateTable",
        df = renderIrTable,
        selectedCols = c("cdmSourceAbbreviation", "targetName", "outcomeName",
                         "ageGroupName", "genderName", "startYear", "tar", "outcomes",
                         "incidenceProportionP100p", "incidenceRateP100py"),
        sortedCols = c("ageGroupName", "genderName", "startYear", "incidenceRateP100py"),
        elementId = "incidence-select",
        colDefsInput = incidenceColList,
        downloadedFileName = "incidenceRateTable-"
      )
      
      #ir plots - TODO edit to reactive
      renderIrPlot <- shiny::reactive(
        {
          if (is.null(inputSelected()$targetIds) |
              is.null(inputSelected()$outcomeIds)) {
            return(data.frame())
          }
          
          plotData <- filteredData() %>%
            dplyr::filter(.data$tar %in% inputSelected()$incidenceRateTarFilter)
          
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
          
          if (inputSelected()$plotColor == "Target Cohort" | inputSelected()$plotColor == "Outcome Cohort") {
            color_aesthetic <- if (inputSelected()$plotColor == "Target Cohort") {
              dplyr::vars(.data$targetIdShort)
            } else if (inputSelected()$plotColor == "Outcome Cohort") {
              dplyr::vars(.data$outcomeIdShort)
            }
          }
          
          if (inputSelected()$plotShape == "Target Cohort" | inputSelected()$plotShape == "Outcome Cohort") {
            shape_aesthetic <- if (inputSelected()$plotShape == "Target Cohort") {
              dplyr::vars(.data$targetIdShort)
            } else if (inputSelected()$plotShape == "Outcome Cohort") {
              dplyr::vars(.data$outcomeIdShort)
            }
          }
          
          max_length <- max(nchar(unique(inputSelected()$plotXAxis)))
          
          if (inputSelected()$plotXTrellis != inputSelected()$plotYTrellis){
            
            # Create the base plot with conditional aesthetics
            base_plot <- ggplot2::ggplot(
              data = plotData,
              ggplot2::aes(x = .data[[inputSelected()$plotXAxis]],
                           y = .data[[inputSelected()$plotYAxis]],
                           shape = if(inputSelected()$plotShape != "None" & inputSelected()$plotShape != "Target Cohort" & 
                                      inputSelected()$plotShape != "Outcome Cohort") .data[[inputSelected()$plotShape]]
                           else shape_aesthetic,
                           color = if(inputSelected()$plotColor != "None" & inputSelected()$plotColor != "Target Cohort" & 
                                      inputSelected()$plotColor != "Outcome Cohort") .data[[inputSelected()$plotColor]]
                           else color_aesthetic,
                           text = .data$tooltip
              )
            ) + 
              ggplot2::geom_point(ggplot2::aes(size = if(inputSelected()$plotSize != "None") .data[[inputSelected()$plotSize]] else NULL,
                                               alpha = 0.6)
              ) 
            
            # Rotate x-axis labels if the maximum length is greater than 10
            if (max_length > 10) {
              base_plot <- base_plot + 
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
            }
            
            # Add trellising if it's not NULL
            if (inputSelected()$plotXTrellis!="None" & inputSelected()$plotXTrellis!="targetName" & inputSelected()$plotXTrellis!="outcomeName" & 
                inputSelected()$plotYTrellis!="None" & inputSelected()$plotYTrellis!="targetName" & inputSelected()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data[[inputSelected()$plotXTrellis]]),
                cols = dplyr::vars(.data[[inputSelected()$plotYTrellis]]),
                scales = if (inputSelected()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) +
                ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
            }
            else if (inputSelected()$plotXTrellis!="None" & inputSelected()$plotXTrellis=="targetName" & inputSelected()$plotXTrellis!="outcomeName" & 
                     inputSelected()$plotYTrellis!="None" & inputSelected()$plotYTrellis!="targetName" & inputSelected()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data$targetIdShort),
                cols = dplyr::vars(.data[[inputSelected()$plotYTrellis]]),
                scales = if (inputSelected()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) +
                ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
            }
            else if (inputSelected()$plotXTrellis!="None" & inputSelected()$plotXTrellis!="targetName" & inputSelected()$plotXTrellis=="outcomeName" & 
                     inputSelected()$plotYTrellis!="None" & inputSelected()$plotYTrellis!="targetName" & inputSelected()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data$outcomeIdShort),
                cols = dplyr::vars(.data[[inputSelected()$plotYTrellis]]),
                scales = if (inputSelected()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) +
                ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
            }
            else if (inputSelected()$plotXTrellis!="None" & inputSelected()$plotXTrellis!="targetName" & inputSelected()$plotXTrellis!="outcomeName" & 
                     inputSelected()$plotYTrellis!="None" & inputSelected()$plotYTrellis=="targetName" & inputSelected()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data[[inputSelected()$plotXTrellis]]),
                cols = dplyr::vars(.data$targetIdShort),
                scales = if (inputSelected()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) +
                ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
            }
            else if (inputSelected()$plotXTrellis!="None" & inputSelected()$plotXTrellis!="targetName" & inputSelected()$plotXTrellis!="outcomeName" & 
                     inputSelected()$plotYTrellis!="None" & inputSelected()$plotYTrellis!="targetName" & inputSelected()$plotYTrellis=="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data[[inputSelected()$plotXTrellis]]),
                cols = dplyr::vars(.data$outcomeIdShort),
                scales = if (inputSelected()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) +
                ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
            }
            else if (inputSelected()$plotXTrellis!="None" & inputSelected()$plotXTrellis=="targetName" & inputSelected()$plotXTrellis!="outcomeName" & 
                     inputSelected()$plotYTrellis!="None" & inputSelected()$plotYTrellis=="targetName" & inputSelected()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data$targetIdShort),
                cols = dplyr::vars(.data$targetIdShort),
                scales = if (inputSelected()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) +
                ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
            }
            else if (inputSelected()$plotXTrellis!="None" & inputSelected()$plotXTrellis=="targetName" & inputSelected()$plotXTrellis!="outcomeName" & 
                     inputSelected()$plotYTrellis!="None" & inputSelected()$plotYTrellis!="targetName" & inputSelected()$plotYTrellis=="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data$targetIdShort),
                cols = dplyr::vars(.data$outcomeIdShort),
                scales = if (inputSelected()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) +
                ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
            }
            else if (inputSelected()$plotXTrellis!="None" & inputSelected()$plotXTrellis!="targetName" & inputSelected()$plotXTrellis=="outcomeName" & 
                     inputSelected()$plotYTrellis!="None" & inputSelected()$plotYTrellis=="targetName" & inputSelected()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data$outcomeIdShort),
                cols = dplyr::vars(.data$targetIdShort),
                scales = if (inputSelected()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) +
                ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
            }
            else if (inputSelected()$plotXTrellis!="None" & inputSelected()$plotXTrellis!="targetName" & inputSelected()$plotXTrellis=="outcomeName" & 
                     inputSelected()$plotYTrellis!="None" & inputSelected()$plotYTrellis!="targetName" & inputSelected()$plotYTrellis=="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data$outcomeIdShort),
                cols = dplyr::vars(.data$outcomeIdShort),
                scales = if (inputSelected()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) +
                ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
            }
            else if (inputSelected()$plotXTrellis=="None" & inputSelected()$plotXTrellis!="targetName" & inputSelected()$plotXTrellis!="outcomeName" & 
                     inputSelected()$plotYTrellis!="None" & inputSelected()$plotYTrellis!="targetName" & inputSelected()$plotYTrellis=="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = NULL,
                cols = dplyr::vars(.data$outcomeIdShort),
                scales = if (inputSelected()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) +
                ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
            }
            else if (inputSelected()$plotXTrellis=="None" & inputSelected()$plotXTrellis!="targetName" & inputSelected()$plotXTrellis!="outcomeName" & 
                     inputSelected()$plotYTrellis!="None" & inputSelected()$plotYTrellis=="targetName" & inputSelected()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = NULL,
                cols = dplyr::vars(.data$targetIdShort),
                scales = if (inputSelected()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) +
                ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
            }
            else if (inputSelected()$plotXTrellis=="None" & inputSelected()$plotXTrellis!="targetName" & inputSelected()$plotXTrellis!="outcomeName" & 
                     inputSelected()$plotYTrellis!="None" & inputSelected()$plotYTrellis!="targetName" & inputSelected()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = NULL,
                cols = dplyr::vars(.data[[inputSelected()$plotYTrellis]]),
                scales = if (inputSelected()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) +
                ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
            }
            else if (inputSelected()$plotXTrellis!="None" & inputSelected()$plotXTrellis!="targetName" & inputSelected()$plotXTrellis!="outcomeName" & 
                     inputSelected()$plotYTrellis=="None" & inputSelected()$plotYTrellis!="targetName" & inputSelected()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data[[inputSelected()$plotXTrellis]]),
                cols = NULL,
                scales = if (inputSelected()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) +
                ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
            }
            else if (inputSelected()$plotXTrellis!="None" & inputSelected()$plotXTrellis=="targetName" & inputSelected()$plotXTrellis!="outcomeName" & 
                     inputSelected()$plotYTrellis=="None" & inputSelected()$plotYTrellis!="targetName" & inputSelected()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data$targetIdShort),
                cols = NULL,
                scales = if (inputSelected()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) +
                ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
            }
            else if (inputSelected()$plotXTrellis!="None" & inputSelected()$plotXTrellis!="targetName" & inputSelected()$plotXTrellis=="outcomeName" & 
                     inputSelected()$plotYTrellis=="None" & inputSelected()$plotYTrellis!="targetName" & inputSelected()$plotYTrellis!="outcomeName") {
              base_plot <- base_plot + ggplot2::facet_grid(
                rows = dplyr::vars(.data$outcomeIdShort),
                cols = NULL,
                scales = if (inputSelected()$irYscaleFixed) "fixed" else "free_y"
              ) +
                ggplot2::theme(strip.background = ggplot2::element_blank(), 
                               strip.text = ggplot2::element_text(size = NULL, color = NULL, face="bold")
                ) +
                ggh4x::force_panelsizes(rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in"))
            }
            
            
            # Rest of your ggplot code remains the same
            base_plot <- base_plot + ggplot2::labs(
              title = paste("Incidence Rate for TAR:", tar_value),
              x = names(options$irPlotCategoricalChoices[options$irPlotCategoricalChoices %in% inputSelected()$plotXAxis]),
              y = names(options$irPlotNumericChoices[options$irPlotNumericChoices %in% inputSelected()$plotYAxis]),
              color = names(options$irPlotCategoricalChoices[options$irPlotCategoricalChoices %in% inputSelected()$plotColor]),
              size = names(options$irPlotNumericChoices[options$irPlotNumericChoices %in% inputSelected()$plotSize]),
              shape = names(options$irPlotCategoricalChoices[options$irPlotCategoricalChoices %in% inputSelected()$plotShape]
              )
            ) +
              ggplot2::guides(alpha = "none") + # Remove the alpha legend
              ggplot2::theme_bw() +
              ggplot2::theme(
                title = ggplot2::element_text(hjust = 0.5),
                plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 10)),
                axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 30)),
                axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 30)),
                legend.box = "horizontal",
                panel.spacing = ggplot2::unit(1, "lines"),
                strip.background = ggplot2::element_blank(), 
                strip.text = ggplot2::element_text(face="bold")
              ) +
              ggh4x::force_panelsizes(
                rows = ggplot2::unit(4, "in"), cols = ggplot2::unit(3, "in")
                )
            
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
      renderIrPlotNoLegend <- shiny::reactive(
        {
          if (is.null(inputSelected()$targetIds) |
              is.null(inputSelected()$outcomeIds)) {
            return(data.frame())
          }
          
          plotData <- filteredData() %>%
            dplyr::filter(.data$tar %in% inputSelected()$incidenceRateTarFilter)
          
          # Get the number of facets in both rows and columns
          num_rows <- length(unique(plotData[[inputSelected()$plotXTrellis]]))
          num_cols <- length(unique(plotData[[inputSelected()$plotYTrellis]]))
          
          max_length <- max(nchar(unique(inputSelected()$plotXAxis)))
          
          base_plot <- renderIrPlot()
          
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
      )
      
      #render the event reactive incidence plot legend only
      renderIrPlotLegend <- shiny::reactive(
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
      

      return(invisible(NULL))
      
    })
}
  
  
getIncidenceData <- function(
    targetIds,
    outcomeIds,
    connectionHandler,
    resultDatabaseSettings
){
  
  if(!is.null(targetIds) & !is.null(outcomeIds)){
    
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
  } else{
    return(NULL)
  }
}


getIncidenceOptions <- function(
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
  
  sql <- 'select distinct tar_id, tar_start_with, tar_start_offset, tar_end_with, tar_end_offset
  from @result_schema.@incidence_table_prefixINCIDENCE_SUMMARY;'
  
  #shiny::incProgress(1/3, detail = paste("Created SQL - Extracting targets"))
  
  tars <- connectionHandler$queryDb(
    sql = sql, 
    result_schema = resultDatabaseSettings$schema,
    incidence_table_prefix = resultDatabaseSettings$incidenceTablePrefix
  )
  tar <- paste0('(',tars$tarStartWith, " + ", tars$tarStartOffset, ') - (', tars$tarEndWith, " + ", tars$tarEndOffset, ')')
  #tar <- tars$tarId
  names(tar) <- paste0('(',tars$tarStartWith, " + ", tars$tarStartOffset, ') - (', tars$tarEndWith, " + ", tars$tarEndOffset, ')')
  
  sql <- 'select distinct age_group_name
  from @result_schema.@incidence_table_prefixINCIDENCE_SUMMARY;'
  
  result <- connectionHandler$queryDb(
    sql = sql, 
    result_schema = resultDatabaseSettings$schema,
    incidence_table_prefix = resultDatabaseSettings$incidenceTablePrefix
  )
  
  ageGroupName <- result$ageGroupName
  ageGroupName[is.na(ageGroupName)] <- 'All'
  ageGroupName <- sort(ageGroupName)
  
  sql <- 'select distinct gender_name
  from @result_schema.@incidence_table_prefixINCIDENCE_SUMMARY;'
  
  result <- connectionHandler$queryDb(
    sql = sql, 
    result_schema = resultDatabaseSettings$schema,
    incidence_table_prefix = resultDatabaseSettings$incidenceTablePrefix
  )
  
  genderName <- result$genderName
  genderName[is.na(genderName)] <- 'All'
  genderName <- sort(genderName)
  
  sql <- 'select distinct start_year
  from @result_schema.@incidence_table_prefixINCIDENCE_SUMMARY;'
  
  result <- connectionHandler$queryDb(
    sql = sql, 
    result_schema = resultDatabaseSettings$schema,
    incidence_table_prefix = resultDatabaseSettings$incidenceTablePrefix
  )
  
  startYear <- result$startYear
  startYear[is.na(startYear)] <- 'All'
  startYear <- sort(startYear)
  
  # shiny::incProgress(3/3, detail = paste("Done"))
  # })
  
  irPlotCategoricalChoices <- list(
    "cdmSourceAbbreviation",
    "ageGroupName",
    "genderName",
    "startYear",
    "targetName",
    "outcomeName",
    "tar",
    "cleanWindow",
    "None"
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
    "None"
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
    "None"
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
    "None"
  )
  
  return(
    list(
      targetIds = targetIds,
      outcomeIds = outcomeIds,
      tar = tar,
      irPlotNumericChoices = irPlotNumericChoices,
      irPlotCategoricalChoices = irPlotCategoricalChoices,
      ageGroupName = ageGroupName,
      genderName = genderName,
      startYear = startYear 
    )
  )
  
}

