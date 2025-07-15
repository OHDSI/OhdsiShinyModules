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


# view two cohorts and compare
characterizationCohortComparisonViewer <- function(id) {
  ns <- shiny::NS(id)
  
    # module that does input selection for a single row DF
    shiny::div(
      
      # UI for inputs
      # summary table
      shinydashboard::box(
        collapsible = TRUE,
        title = "Options",
        width = "100%",
        shiny::uiOutput(ns("inputs"))
      ),
      
      # displayed inputs
      shiny::conditionalPanel(
        condition = "output.showCohortComp != 0", 
        ns = ns,
        
        inputSelectionDfViewer(id = ns('inputSelected'), title = 'Selected'),
        
        # add basic table 
        shiny::tabsetPanel(
          type = 'pills',
          shiny::tabPanel(
            title = 'Counts',
            resultTableViewer(id = ns('countTable'), boxTitle = 'Counts')
          ),
          shiny::tabPanel(
            title = 'Binary',
            shiny::tabsetPanel(
              type = 'pills',
              id = ns('binaryPanel'),
              shiny::tabPanel(
                title = "Table",
                resultTableViewer(id = ns('mainTable'), boxTitle = 'Binary')
              ),
              shiny::tabPanel(
                title = "Plot",
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(ns('scatterPlot'))
                )
              )
            )
          ),
          shiny::tabPanel(
            title = 'Continuous',
            resultTableViewer(id = ns('continuousTable'), boxTitle = 'Continuous')
          )
        )
        
      )
    )
}



characterizationCohortComparisonServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings,
    targetTable,
    reactiveTargetRow
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      # initially do not show results
      output$showCohortComp <- shiny::reactive(0)
      shiny::outputOptions(output, "showCohortComp", suspendWhenHidden = FALSE)
      
      # if target or outcome changes hide results
      shiny::observeEvent(reactiveTargetRow(), {
        output$showCohortComp <- shiny::reactive(0)
      })
      
      # get the databases that the target cohort has data in
      databaseNames <- shiny::reactive(unlist(strsplit(x = reactiveTargetRow()$databaseString, split = ', ')))
      databaseIds <- shiny::reactive(unlist(strsplit(x = reactiveTargetRow()$databaseIdString, split = ', ')))
      
      # add the server for the comparator table select
      reactiveComparatorRow <- tableSelectionServer(
        id = 'comparator-selector', 
        table = shiny::reactive(targetTable %>%
                                  dplyr::filter(.data$cohortComparator == 1) %>%
                                  dplyr::filter(.data$cohortId != reactiveTargetRow()$cohortId) %>%
                                  dplyr::select("parentName", "cohortName", "cohortId")
                                ), 
        selectMultiple = FALSE, 
        elementId = session$ns('comp-selector'),
        inputColumns = list(
          parentName = reactable::colDef(
            name = 'Comparator', 
            minWidth = 150
          ),
          cohortName = reactable::colDef(
            name = 'Subset',
            minWidth = 300
          ),
          cohortId = reactable::colDef(
            show = TRUE,
            name = 'Cohort ID'
          )
        ),
        selectButtonText = 'Select Comparator'
      )
      
      # hide results if reactiveComparatorRow changes
      shiny::observeEvent(reactiveComparatorRow(),{
        output$showCohortComp <- shiny::reactive(0)
      })
      
      
      # initial comp chilren
      output$inputs <- shiny::renderUI({
        
        shiny::div(
          
          tableSelectionViewer(
            id = session$ns('comparator-selector')
            ),
          
        shinyWidgets::pickerInput(
          inputId = session$ns('databaseName'),
          label = 'Database: ',
          choices = databaseNames(),
          selected = databaseNames(),
          multiple = F,
          options = shinyWidgets::pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            dropupAuto = F,
            size = 10,
            liveSearchStyle = "contains",
            liveSearchPlaceholder = "Type here to search",
            virtualScroll = 500
          )
        ),
        
        shiny::actionButton(
          inputId = session$ns('generate'), 
          label = 'Generate'
            )
      )
      
      })
      
      # show selected inputs to user
      inputSelectionDfServer(
        id = 'inputSelected', 
        dataFrameRow = selected,
        ncol = 1
      )
      
      #get results
      selected <- shiny::reactiveVal()
      shiny::observeEvent(input$generate,{
        
        
        # TODO update logic for running 
        if(is.null(reactiveComparatorRow()) | is.null(reactiveTargetRow())){
          output$showCohortComp <- shiny::reactive(0)
          shiny::showNotification('Must select a comparison')
        } else{
          if(nrow(reactiveTargetRow()) > 0 & nrow(reactiveComparatorRow()) > 0){
            
            output$showCohortComp <- shiny::reactive(1)
            
            selected(
              data.frame(
                Target = reactiveTargetRow()$cohortName,
                Comparator = reactiveComparatorRow()$cohortName,
                Database = input$databaseName
              )
            )
            
            resultTable <- characterizatonGetCohortData(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              targetIds = c(reactiveTargetRow()$cohortId,reactiveComparatorRow()$cohortId),
              databaseIds = databaseIds()[databaseNames() == input$databaseName],
              minThreshold = 0.01,
              addSMD = T
            )
            
            countTable <- characterizatonGetCohortCounts(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              targetIds = c(reactiveTargetRow()$cohortId,reactiveComparatorRow()$cohortId),
              databaseIds = databaseIds()[databaseNames() == input$databaseName]
            )
            
            continuousTable <- characterizatonGetCohortComparisonDataContinuous(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              targetIds = c(reactiveTargetRow()$cohortId,reactiveComparatorRow()$cohortId),
              databaseIds = databaseIds()[databaseNames() == input$databaseName]
            )
            
            resultTableServer(
              id = 'mainTable',
              df = resultTable,
              details = data.frame(
                Target = reactiveTargetRow()$cohortName,
                Comparator = reactiveComparatorRow()$cohortName,
                Database = input$databaseName,
                Analysis = 'Cohort comparison within database'
              ),
              downloadedFileName = 'cohort_comparison_binary',
              colDefsInput = characterizationCohortsColumns(
                addExtras = T,
                elementId = session$ns('main-table-filter')
              ), 
              elementId = session$ns('main-table-filter')
            ) 
            
            resultTableServer(
              id = 'continuousTable',
              df = continuousTable,
              details = data.frame(
                Target = reactiveTargetRow()$cohortName,
                Comparator = reactiveComparatorRow()$cohortName,
                Database = input$databaseName,
                Analysis = 'Cohort comparison within database'
              ),
              downloadedFileName = 'cohort_comparison_cont',
              colDefsInput = characterizationCohortsColumnsContinuous(
                addExtras = T,
                elementId = session$ns('continuous-table-filter')
              ),
              elementId = session$ns('continuous-table-filter')
            ) 
            
            resultTableServer(
              id = 'countTable',
              df = countTable,
              details = data.frame(
                Target = reactiveTargetRow()$cohortName,
                Comparator = reactiveComparatorRow()$cohortName,
                Database = input$databaseName,
                Analysis = 'Cohort comparison within database'
              ),
              downloadedFileName = 'cohort_comparison_count',
              colDefsInput = characteriationCountTableColDefs(
                elementId = session$ns('count-table-filter')
              ),
              elementId = session$ns('count-table-filter')
            )
            
            
            # clean plot data
            if(nrow(resultTable) > 0){
              plotDf <- resultTable
              if(sum(is.na(plotDf)) > 0){
                plotDf <- plotDf[is.na(plotDf)] <- 0
              }
            plotDf <- plotDf %>%
              dplyr::mutate(domain = dplyr::case_when(
                grepl("condition_", .data$covariateName) | sub("\\s.*", "", .data$covariateName) == "condition" ~ "Condition",
                grepl("drug_", .data$covariateName) | sub("\\s.*", "", .data$covariateName) == "drug" ~ "Drug",
                grepl("procedure_", .data$covariateName) | sub("\\s.*", "", .data$covariateName) == "procedure" ~ "Procedure",
                grepl("measurement_", .data$covariateName) | sub("\\s.*", "", .data$covariateName) == "measurement" ~ "Measurement",
                grepl("observation_", .data$covariateName) | sub("\\s.*", "", .data$covariateName) == "observation" ~ "Observation",
                grepl("device_", .data$covariateName) | sub("\\s.*", "", .data$covariateName) == "device" ~ "Device",
                grepl("cohort_", .data$covariateName) | sub("\\s.*", "", .data$covariateName) == "cohort" ~ "Cohort",
                grepl("visit_", .data$covariateName) | sub("\\s.*", "", .data$covariateName) == "visit" ~ "Visit",
                .default = "Demographic"
              ))
            
            # Create hover text for plotly
            plotDf$hoverText <- paste(
              "Covariate Name:", plotDf$covariateName, 
              "<br>", "Target", ":", scales::percent(plotDf$averageValue_1), 
              "<br>", "Comparator", ":", scales::percent(plotDf$averageValue_2)
            )
            
            #removing negatives, which come from "< min threshold"
            plotDf$averageValue_1[plotDf$averageValue_1 < 0] <- 0
            plotDf$averageValue_2[plotDf$averageValue_2 < 0] <- 0
            
            output$scatterPlot <- plotly::renderPlotly({
              
              # Create the scatter plot with the diagonal line (x = y)
              p <- ggplot2::ggplot(plotDf, ggplot2::aes(       x = .data$averageValue_1,
                                                               y = .data$averageValue_2,
                                                               color = .data$domain,
                                                               text = .data$hoverText)) +  # Use hoverText for hover labels
                ggplot2::geom_point(size = 2) +    # Smaller point size
                ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Diagonal x=y line in black
                ggplot2::labs(
                  x = paste0("Target", " %"),
                  y = paste0("Comparator", " %"),
                  color = "Domain",
                  title = paste0("Database: ", input$databaseName)
                  ) +
                ggplot2::theme_minimal() +          # Optional: use a clean theme
                ggplot2::theme(
                  plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 10), hjust = 0.5, size = 25, face="bold"),
                  legend.position = "right",        # Position legend as needed
                  axis.title = ggplot2::element_text(size = 12),  # Adjust axis title size
                  axis.text = ggplot2::element_text(size = 10)    # Adjust axis text size
                ) +
                ggplot2::scale_x_continuous(labels = scales::percent_format()) +  # Format x-axis as percentage
                ggplot2::scale_y_continuous(labels = scales::percent_format())    # Format y-axis as percentage
              
              # Convert to a plotly object for interactivity
              plotly::ggplotly(p, tooltip = "text")  # Use the custom hover text
            })
            } # if nrow >0
            
          } else{
            shiny::showNotification('Must select a comparison and target cohort')
            output$showCohortComp <- shiny::reactive(0)
          }
        }
        
      })
      
      return(invisible(NULL))
      
    })
  
}


characterizationCohortsColumns <- function(
    addExtras = F,
    elementId 
    ){
  
  res <- list(
    covariateName = reactable::colDef(
      header = withTooltip(
        "Covariate Name",
        "The name of the covariate"
      ), 
      minWidth = 300
    ),
    covariateId = reactable::colDef(
      show = F,
      header = withTooltip("Covariate ID",
                           "Unique identifier of the covariate")
    ),
    minPriorObservation = reactable::colDef(
      header = withTooltip(
        "Min Prior Obs",
        "The minimum prior observation a patient in the target 
        population must have to be included."),
      filterable = T,
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
    SMD = reactable::colDef(
      header = withTooltip("SMD",
                           "Standardized mean difference between the target and comparator percentages"),
      format = reactable::colFormat(digits = 3)
    ),
    absSMD = reactable::colDef(
      header = withTooltip("absSMD",
                           "Absolute standardized mean difference between the target and comparator percentages"),
      format = reactable::colFormat(digits = 3),
      filterable = TRUE,
      filterMethod = reactable::JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return row.values[columnId] >= filterValue
        })
      }"),
      filterInput = function(values, name) {
        oninput <- sprintf("Reactable.setFilter('%s', '%s', this.value)", elementId, name)
        shiny::tags$input(
          type = "range",
          min = floor(min(values, na.rm = T)),
          max = ceiling(max(values, na.rm = T)),
          value = floor(min(values, na.rm = T)),
          oninput = oninput,
          onchange = oninput, # For IE11 support
          "aria-label" = sprintf("Filter by minimum %s", name)
        )
      }
    ),
    analysisName = reactable::colDef(
      header = withTooltip(
        "Covariate Class",
        "Class/type of the covariate"
      )
    )
  )
  
  if(addExtras){
    res <- append(
      res,
      list(
        sumValue_1 = reactable::colDef(
          header = withTooltip("Target Sum",
                               "The total sum of the covariate for the target cohort."),
          cell = function(value) {
            if (value >= 0) value else '< min threshold'
          }
        ),
        sumValue_2 = reactable::colDef(
          header = withTooltip("Compatator Sum",
                               "The total sum of the covariate for the comparator cohort."),
          cell = function(value) {
            if (value >= 0) value else '< min threshold'
          }
        ),
        averageValue_1 = reactable::colDef(
          header = withTooltip("Target %",
                               "The percentage of the target cohort who had the covariate prior to index."), 
          cell = function(value) {
            if (value >= 0) paste0(round(value*100, digits = 3),'%') else '< min threshold'
          }
        ),
        averageValue_2 = reactable::colDef(
          header = withTooltip("Comparator %",
                               "The percentage of the comparator cohort who had the covariate prior to index"),
          cell = function(value) {
            if (value >= 0) paste0(round(value*100, digits = 3),'%') else '< min threshold'
          }
        )
      )
      )
  }
  return(res)
}

characteriationCountTableColDefs <- function(
    elementId
    ){
  result <- list(
    selection = reactable::colDef(
      filterable = T
    ),
    cohortName = reactable::colDef(
      header = withTooltip("Cohort",
                           "Name of the cohort"),
      filterable = T
    ),
    minPriorObservation = reactable::colDef(
      header = withTooltip(
        "Min Prior Obs",
        "The minimum prior observation a patient in the target 
        population must have to be included."),
      filterable = T,
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
    rowCount = reactable::colDef(
      header = withTooltip("Record Count",
                           "Count of the number of records"),
      filterable = T
    ), 
    personCount = reactable::colDef(
      header = withTooltip("Person Count",
                           "Count of the number of persons"),
      filterable = T
    )
  )
  return(result)
}

characterizationCohortsColumnsContinuous <- function(
    addExtras = F,
    elementId
  ){
  res <- list(
    covariateName = reactable::colDef(
      header = withTooltip(
        "Covariate Name",
        "The name of the covariate"
      ), 
      filterable = T, 
      minWidth = 300,
    ),
    databaseName = reactable::colDef(
      header = withTooltip(
        "Database",
        "The name of the database"
      ), 
      filterable = T,
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
    covariateId = reactable::colDef(
      show = F,
      header = withTooltip("Covariate ID",
                           "Unique identifier of the covariate")
    ),
    minPriorObservation = reactable::colDef(
      header = withTooltip(
        "Min Prior Obs",
        "The minimum prior observation a patient in the target 
        population must have to be included."),
      filterable = T,
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
    outcomeWashoutPeriod = reactable::colDef(
      show = F
      ),
    countValue = reactable::colDef(
      header = withTooltip("Count",
                           "Number of people with the covariate in the cohort."),
      cell = function(value) {
        if (value >= 0) value else '< min threshold'
      },
      filterable = T,
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
    averageValue = reactable::colDef(
      header = withTooltip("Mean",
                           "The mean value of the covariate in the cohort"),
      cell = function(value) {
        if (value >= 0) round(value, digits = 3) else '< min threshold'
      }
    ),
    standardDeviation = reactable::colDef(
      header = withTooltip("StDev",
                           "The standard deviation value of the covariate in the cohort"),
      cell = function(value) {
        if (value >= 0) round(value, digits = 3) else '< min threshold'
      }
    ),
    medianValue = reactable::colDef(
      header = withTooltip("Median",
                           "The median value of the covariate in the cohort."),
      cell = function(value) {
        round(value, digits = 3)
      }
    ),
    minValue = reactable::colDef(
      header = withTooltip("Min Value",
                           "Minimum value of the covariate in the cohort"),
      format = reactable::colFormat(digits = 3)
    ),
    maxValue = reactable::colDef(
      header = withTooltip("Max Value",
                           "Maximum value the covariate in the cohort"),
      format = reactable::colFormat(digits = 3)
    ),
    p25Value = reactable::colDef(
      header = withTooltip("25th %tile",
                           "25th percentile value of the covariate in the cohort"),
      format = reactable::colFormat(digits = 3)
    ),
    p75Value = reactable::colDef(
      header = withTooltip("75th %tile",
                           "75th percentile value of the covariate in the cohort"),
      format = reactable::colFormat(digits = 3)
    ),
    p10Value = reactable::colDef(
      header = withTooltip("10th %tile",
                           "10th percentile value of the covariate in the cohort"),
      format = reactable::colFormat(digits = 3)
    ),
    p90Value = reactable::colDef(
      header = withTooltip("90th %tile",
                           "90th percentile value of the covariate in the cohort"),
      format = reactable::colFormat(digits = 3)
    )
  )
  
  if(addExtras){
    res <- append(
      res,
      list(
        SMD = reactable::colDef(
          header = withTooltip("SMD",
                               "Standardized mean difference"),
          format = reactable::colFormat(digits = 3)
        ),
        absSMD = reactable::colDef(
          header = withTooltip("absSMD",
                               "Absolute standardized mean difference"),
          format = reactable::colFormat(digits = 3)
        ),
        countValue_1 = reactable::colDef(
          header = withTooltip("Target Count",
                               "Number of people with the covariate for the target cohort."),
          cell = function(value) {
            if (value >= 0) value else '< min threshold'
          }
        ),
        countValue_2 = reactable::colDef(
          header = withTooltip("Comparator Count",
                               "Number of people with the covariate for the comparator cohort."),
          cell = function(value) {
            if (value >= 0) value else '< min threshold'
          }
        ),
        averageValue_1 = reactable::colDef(
          header = withTooltip("Target Mean",
                               "The mean of the covariate for the target cohort."), 
          cell = function(value) {
            if (value >= 0) round(value, digits = 3) else '< min threshold'
          }
        ),
        averageValue_2 = reactable::colDef(
          header = withTooltip("Comparator Mean",
                               "The mean of the covariate for the comparator cohort."),
          cell = function(value) {
            if (value >= 0) round(value, digits = 3) else '< min threshold'
          }
        ),
        standardDeviation_1 = reactable::colDef(
          header = withTooltip("Target StDev",
                               "The standard deviation of the covariate for the target cohort."), 
          cell = function(value) {
            if (value >= 0) round(value, digits = 3) else '< min threshold'
          }
        ),
        standardDeviation_2 = reactable::colDef(
          header = withTooltip("Comparator StDev",
                               "The standard deviation of the covariate for the comparator cohort."),
          cell = function(value) {
            if (value >= 0) round(value, digits = 3) else '< min threshold'
          }
        ),
        medianValue_1 = reactable::colDef(
          header = withTooltip("Target Median",
                               "The median of the covariate for the target cohort."), 
          cell = function(value) {
            round(value, digits = 3)
          }
        ),
        medianValue_2 = reactable::colDef(
          header = withTooltip("Comparator Median",
                               "The median of the covariate for the comparator cohort."),
          cell = function(value) {
            round(value, digits = 3)
          }
        ),
        minValue_2 = reactable::colDef(
          header = withTooltip("Comparator Min Value",
                               "Minimum value of the comparator cohort"),
          format = reactable::colFormat(digits = 3)
        ),
        minValue_1 = reactable::colDef(
          header = withTooltip("Target Min Value",
                               "Minimum value of the target cohort"),
          format = reactable::colFormat(digits = 3)
        ),
        maxValue_2 = reactable::colDef(
          header = withTooltip("Comparator Max Value",
                               "Maximum value of the comparator cohort"),
          format = reactable::colFormat(digits = 3)
        ),
        maxValue_1 = reactable::colDef(
          header = withTooltip("Target Max Value",
                               "Maximum value of the target cohort"),
          format = reactable::colFormat(digits = 3)
        ),
        p25Value_2 = reactable::colDef(
          header = withTooltip("Comparator 25th %tile",
                               "25th percentile value of the comparator cohort"),
          format = reactable::colFormat(digits = 3)
        ),
        p25Value_1 = reactable::colDef(
          header = withTooltip("Target 25th %tile",
                               "25th percentile value of the target cohort"),
          format = reactable::colFormat(digits = 3)
        ),
        p75Value_2 = reactable::colDef(
          header = withTooltip("Comparator 75th %tile",
                               "75th percentile value of the comparator cohort"),
          format = reactable::colFormat(digits = 3)
        ),
        p75Value_1 = reactable::colDef(
          header = withTooltip("Target 75th %tile",
                               "75th percentile value of the target cohort"),
          format = reactable::colFormat(digits = 3)
        )
      )
    )
  }
  
  return(res)
}


characterizatonGetCohortCounts <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetIds,
    databaseIds
){
  
  start <- Sys.time()
  result <- connectionHandler$queryDb(
    sql = "
select  distinct 
        cc.target_cohort_id as cohort_definition_id,
        cc.min_prior_observation,
        cc.row_count,
        cc.person_count,
        d.cdm_source_abbreviation as database_name,
        d.database_id,
        cg.cohort_name 
  from 
  @schema.@database_table d
  inner join
  @schema.@c_table_prefixcohort_counts cc 
  on d.database_id = cc.database_id
  inner join
  @schema.@cg_table_prefixcohort_definition cg
  on cg.cohort_definition_id = cc.target_cohort_id
  
  where
  cc.target_cohort_id in (@target_ids) 
  and cc.cohort_type = 'Target'
  AND cc.database_id in (@database_ids)
  ;
  ", 
    schema = resultDatabaseSettings$schema, 
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    target_ids =   paste0(targetIds, collapse= ','),
    database_ids = paste0("'",databaseIds,"'",collapse= ','), 
    database_table = resultDatabaseSettings$databaseTable
  )
  end <- Sys.time() - start 
  message(paste0('Extracting ', nrow(result) ,' cohort count rows took: ', round(end, digits = 2), ' ', units(end)))
  
  if(length(targetIds)>1){
  result <- merge(
    x = result,
    y = data.frame(
      selection = c('Target','Comparator'),
      cohortDefinitionId = targetIds
    ), 
    by = 'cohortDefinitionId'
  )
  } else{
    result$selection <- result$databaseName
  }
  
  result <- result %>% dplyr::select(
    'selection',
    'cohortName', 
    'minPriorObservation',
    'rowCount',
    'personCount'
  )
  
  return(result)
  
}


characterizatonGetCohortData <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetIds,
    databaseIds,
    minThreshold = 0.01,
    addSMD = F
){
  
  if(is.null(targetIds) |  is.null(databaseIds)){
    warning('Ids cannot be NULL')
   return(NULL)
  }

  shiny::withProgress(message = 'characterizatonGetCohortData', value = 0, {

    shiny::incProgress(1/4, detail = paste("Setting types"))
    
    types <- data.frame(
      type = 1:(length(targetIds)*length(databaseIds)),
      cohortDefinitionId = rep(targetIds, length(databaseIds)),
      databaseId = rep(databaseIds, length(targetIds))
    )
    
    shiny::incProgress(2/4, detail = paste("Extracting data"))
    
    sql <- "select  ref.covariate_name, 
          s.min_prior_observation,
          cov.target_cohort_id as cohort_definition_id,
          cov.* from   
    @schema.@c_table_prefixCOVARIATES cov 
    inner join 
    @schema.@c_table_prefixcovariate_ref ref
    on cov.covariate_id = ref.covariate_id
    and cov.setting_id = ref.setting_id
    and cov.database_id = ref.database_id
    inner join 
    @schema.@c_table_prefixsettings s
    on s.database_id = cov.database_id
    and s.setting_id = cov.setting_id
    
    where 
    cov.target_cohort_id in (@target_ids) 
    and cov.cohort_type = 'Target'
    AND cov.database_id in (@database_ids)
    AND cov.average_value >= @min_threshold;"
          
    start <- Sys.time()
    # settings.min_characterization_mean needed?
    res <- connectionHandler$queryDb(
      sql = sql,
      target_ids = paste0(targetIds, collapse = ','),
      database_ids = paste0("'",databaseIds,"'", collapse = ','),
      schema = resultDatabaseSettings$schema,
      c_table_prefix = resultDatabaseSettings$cTablePrefix,
      min_threshold = minThreshold
    )
    end <- Sys.time() - start 
    shiny::incProgress(3/4, detail = paste("Extracted data"))
    message(paste0('Extracting ', nrow(res) ,' characterization cohort rows took: ', round(end, digits = 2), ' ', units(end)))
    
    # add the first/section type
    res <- merge(res, types, by = c('cohortDefinitionId','databaseId'))
    
    # pivot
    result <- tidyr::pivot_wider(
      data = res, 
      id_cols = c('covariateName', 'covariateId','minPriorObservation'), 
      names_from = 'type', 
      values_from = c('sumValue', 'averageValue'), 
      values_fn = mean, 
      values_fill = -1
    ) 
    
    if(addSMD == T){
      # TODO get min_characterization_mean from settings table
      # minCharacterizationMean <- minThreshold
      # add SMD
      if(sum(c('averageValue_1','averageValue_2') %in% colnames(result)) == 2){
        convertMissing <- function(vec){sapply(vec, function(x) ifelse(x==-1, minThreshold, x))}
        
        Ns <- c()
        for(minPriorObservation in unique(result$minPriorObservation)){
          ind <- result$minPriorObservation == minPriorObservation
          Ns <- rbind(Ns,
                      data.frame(
                        minPriorObservation = minPriorObservation,
                        N_1 = max(result$sumValue_1[ind]/result$averageValue_1[ind], na.rm = T),
                        N_2 = max(result$sumValue_2[ind]/result$averageValue_2[ind], na.rm = T)
                      )
          )
        }
        result <- merge(result, Ns, by = 'minPriorObservation')
        result$firstVar <- ((convertMissing(result$averageValue_1)-1)^2*result$sumValue_1 + (convertMissing(result$averageValue_1)-0)^2*(result$N_1-result$sumValue_1))/result$N_1
        result$secondVar <- ((convertMissing(result$averageValue_2)-1)^2*result$sumValue_2 + (convertMissing(result$averageValue_2)-0)^2*(result$N_2-result$sumValue_2))/result$N_2
        result$SMD <- (convertMissing(result$averageValue_1) - convertMissing(result$averageValue_2))/(sqrt((result$firstVar+result$secondVar)/2))
        result$absSMD <- abs(result$SMD)
        result <- result %>% dplyr::select(-"firstVar",-"secondVar", -"N_1", -"N_2")
        
      } else{
        NULL
        shiny::showNotification('Unable to add SMD due to missing columns')
      }
    }
    shiny::incProgress(4/4, detail = paste("Done"))
  })
  
    return(result)
  
}


characterizatonGetCohortComparisonDataContinuous <- function(
  connectionHandler,
  resultDatabaseSettings,
  targetIds,
  databaseIds,
  pivot = T
){

  if(is.null(targetIds) |  is.null(databaseIds)){
    warning('Ids cannot be NULL')
    return(NULL)
  }
  targetIds <- unique(targetIds)
  databaseIds <- unique(databaseIds)
  
  shiny::withProgress(message = 'characterizatonGetCohortDataContinuous', value = 0, {
    
    shiny::incProgress(1/4, detail = paste("Setting types"))
    
    types <- data.frame(
      type = 1:(length(targetIds)*length(databaseIds)),
      cohortDefinitionId = rep(targetIds, length(databaseIds)),
      databaseId = rep(databaseIds, length(targetIds))
    )
    
    shiny::incProgress(2/4, detail = paste("Extracting data"))
    
    sql <- "select  ref.covariate_name, 
          s.min_prior_observation,
          cov.target_cohort_id as cohort_definition_id,
          cov.*,
          d.CDM_SOURCE_ABBREVIATION as database_name
          
          from   
    @schema.@c_table_prefixCOVARIATES_continuous cov 
    inner join 
    @schema.@c_table_prefixcovariate_ref ref
    on cov.covariate_id = ref.covariate_id
    and cov.setting_id = ref.setting_id
    and cov.database_id = ref.database_id
    inner join
    @schema.@c_table_prefixsettings s 
    on cov.setting_id = s.setting_id
    and cov.database_id = s.database_id
    inner join @schema.@database_meta_table d 
    on s.database_id = d.database_id
    
    where 
    cov.target_cohort_id in (@target_ids) 
    and cov.cohort_type = 'Target'
    AND cov.database_id in (@database_ids);"
    
    start <- Sys.time()
    # settings.min_characterization_mean needed?
    res <- connectionHandler$queryDb(
      sql = sql,
      target_ids = paste0(targetIds, collapse = ','),
      database_ids = paste0("'",databaseIds,"'", collapse = ','),
      schema = resultDatabaseSettings$schema,
      c_table_prefix = resultDatabaseSettings$cTablePrefix,
      database_meta_table = resultDatabaseSettings$databaseTable
    )
    end <- Sys.time() - start 
    shiny::incProgress(3/4, detail = paste("Extracted data"))
    message(paste0('Extracting ', nrow(res) ,' continuous characterization cohort rows took: ', round(end, digits = 2), ' ', units(end)))
    
    # add the first/section type
    res <- merge(res, types, by = c('cohortDefinitionId','databaseId'))
    
    if(pivot){
      # if pivot
      res <- tidyr::pivot_wider(
        data = res, 
        id_cols = c('covariateName', 'covariateId','minPriorObservation'), 
        names_from = 'type', 
        values_from = c('countValue', 'averageValue', 'standardDeviation', 'medianValue','minValue', 'maxValue', 'p25Value','p75Value'), 
        values_fn = mean, 
        values_fill = -1
      ) 
      
      # if both have results then add SMD
      if(length(unique(res$type)) == 2){
        res <- res %>% 
          dplyr::mutate(
            SMD = (.data$averageValue_1-.data$averageValue_2)/(sqrt((.data$standardDeviation_1^2 + .data$standardDeviation_2^2)/2))
          ) %>%
          dplyr::mutate(
            absSMD = abs(.data$SMD)
          )
      }
    } else{
      # if multiple databases make the type the databaseName
      res$type <- res$databaseName
      res <- res %>% dplyr::select(-"cohortDefinitionId", -"databaseId", -"type",
                                   -"settingId", -"targetCohortId", -"outcomeCohortId", 
                                   -"cohortType") %>%
        dplyr::relocate("databaseName", .after = "covariateName")

      # fill missing values with 0
      res[is.na(res)] <- 0
    }
    
    shiny::incProgress(4/4, detail = paste("Done"))
  })
  
  return(res)
}

