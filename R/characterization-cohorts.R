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
      
      shiny::helpText('Compare covariates at index between two cohorts within the same database.'),
      
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
            title = 'Binary Table',
            shiny::uiOutput(outputId = ns('helpTextBinary')),
            resultTableViewer(id = ns('mainTable'), boxTitle = 'Binary')
          ),
          
          shiny::tabPanel(
            title = "Binary Plot",
            shiny::helpText('Pick two databases and compare binary features across the databases.'),
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns('scatterPlot'))
            )
          ),
          
          shiny::tabPanel(
            title = 'Continuous',
            shiny::uiOutput(outputId = ns('helpTextContinuous')),
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
      reactiveComparatorRowId <- shiny::reactiveVal(NULL)
      tableSelectionServer(
        id = 'comparator-selector', 
        table = shiny::reactive(targetTable %>%
                                  dplyr::filter(.data$cohortComparator == 1) %>%
                                  dplyr::filter(.data$cohortId != reactiveTargetRow()$cohortId) %>%
                                  dplyr::select("parentName", "cohortName", "cohortId")
                                ), 
        selectedRowId = reactiveComparatorRowId,
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
      shiny::observeEvent(reactiveComparatorRowId(),{
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
      
        # got to apply same filter  
      filteredTable <- targetTable %>%
          dplyr::filter(.data$cohortComparator == 1) %>%
          dplyr::filter(.data$cohortId != reactiveTargetRow()$cohortId)
      reactiveComparatorRow <- filteredTable[reactiveComparatorRowId(),]
        
        # TODO update logic for running 
        if(is.null(reactiveComparatorRow) | is.null(reactiveTargetRow())){
          output$showCohortComp <- shiny::reactive(0)
          shiny::showNotification('Must select a comparison')
        } else{
          if(nrow(reactiveTargetRow()) > 0 & nrow(reactiveComparatorRow) > 0){
            
            selected(
              data.frame(
                Target = reactiveTargetRow()$cohortName,
                Comparator = reactiveComparatorRow$cohortName,
                Database = input$databaseName
              )
            )
            
            result <- characterizatonGetCohortData(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              targetIds = c(reactiveTargetRow()$cohortId,reactiveComparatorRow$cohortId),
              databaseIds = databaseIds()[databaseNames() == input$databaseName],
              minThreshold = 0
            )
            resultTable <- result$covariates
            countTable <- result$covRef
            
            # if no results in database
            if(is.null(countTable)){
              shiny::showNotification('No covariate data for selected database')
              output$showCohortComp <- shiny::reactive(0)
            } else if(nrow(countTable) == 1){
              shiny::showNotification(paste0('Unable to compare as only cohort ', unique(countTable$cohortName) ,' has covariate data in selected database.'))
              output$showCohortComp <- shiny::reactive(0)
            } else{
              output$showCohortComp <- shiny::reactive(1)
          
            output$helpTextBinary <- shiny::renderUI(
              shiny::helpText(paste0("This analysis shows the fraction of patients in the cohorts (restricted to first index date and requiring ",
                                     countTable$minPriorObservation[1]," days observation prior to index) with a history of each binary features across databases."))
            )
            output$helpTextContinuous <- shiny::renderUI(
              shiny::helpText(paste0("This analysis shows the fraction of patients in the cohorts (restricted to first index date and requiring ",
                                     countTable$minPriorObservation[1]," days observation prior to index) with a history of each continuous features across databases."))
            )
            
            continuous <- characterizatonGetCohortComparisonDataContinuous(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              targetIds = c(reactiveTargetRow()$cohortId,reactiveComparatorRow$cohortId),
              databaseIds = databaseIds()[databaseNames() == input$databaseName]
            )
            
            continuousTable <- continuous$covariates
            
            getDbCount <- function(cohortId){
              countOfInt <- countTable %>% 
                dplyr::filter(.data$cohortId == !!cohortId)
              
              return(countOfInt)
            }
            
            groupColumns <- list()
            
            targetRows <- getDbCount(reactiveTargetRow()$cohortId)
            for(j in 1:nrow(targetRows)){
              groupColumns[[length(groupColumns) + 1]] <- reactable::colGroup(
                name = paste0('Target with ',targetRows$minPriorObservation[j], ' days obs (N = ',targetRows$n[j],')'), 
                columns = c(
                  paste0('sumValue_',targetRows$id[j]), 
                  paste0('averageValue_',targetRows$id[j]))
              )
            }
            compRows <- getDbCount(reactiveComparatorRow$cohortId)
            for(j in 1:nrow(compRows)){
              groupColumns[[length(groupColumns) + 1]] <- reactable::colGroup(
                name = paste0('Comparator with ',compRows$minPriorObservation[j], ' days obs (N = ',compRows$n[j],')'), 
                columns = c(
                  paste0('sumValue_',compRows$id[j]), 
                  paste0('averageValue_',compRows$id[j]))
              )
            }
            
            # figure out the column names and how to present them to reactable
            binColumns <- list(
              averageValue_1 = reactable::colDef(
                name = '%',
                header = withTooltip(
                  paste0('%'),
                  paste0("The percentage of the target population in database who had the covariate prior.")
                ),
                cell = function(value) {
                  if (value >= 0) paste0(round(value*100, digits = 3),' %') else '< min threshold'
                }
              ),
              averageValue_2 = reactable::colDef(
                name = '%',
                header = withTooltip(
                  paste0('%'),
                  paste0("The percentage of the comparator population in database who had the covariate prior.")
                ),
                cell = function(value) {
                  if (value >= 0) paste0(round(value*100, digits = 3),' %') else '< min threshold'
                }
              ),
              sumValue_1 = reactable::colDef(
                name = 'Count',
                header = withTooltip(
                  paste0("Count"),
                  paste0("The number of people in the target cohort in database who have the covariate prior.")
                ),
                cell = function(value) {
                  if (value >= 0) value else '< min threshold'
                }
              )
              ,
              sumValue_2 = reactable::colDef(
                name = 'Count',
                header = withTooltip(
                  paste0("Count"),
                  paste0("The number of people in the comparator cohort in database who have the covariate prior.")
                ),
                cell = function(value) {
                  if (value >= 0) value else '< min threshold'
                }
              )
          )
            
            resultTableServer(
              id = 'mainTable',
              df = resultTable,
              details = data.frame(
                Target = reactiveTargetRow()$cohortName,
                Comparator = reactiveComparatorRow$cohortName,
                Database = input$databaseName,
                Analysis = 'Cohort comparison within database'
              ),
              downloadedFileName = 'cohort_comparison_binary',
              colDefsInput = append(
                characterizationCohortsColumns(elementId = session$ns('main-table-filter')),
                binColumns
              ), 
              columnGroups = groupColumns,
              elementId = session$ns('main-table-filter')
            ) 
            
            
            # column formatting for continuous
            # create group columns for continuous
            groupColumnsContinuous <- list()

              groupColumnsContinuous[[1]] <- reactable::colGroup(
                name = paste0('Target with ',countTable$minPriorObservation[1] ,' days prior obs (N = ',countTable$n[1],')'), 
                columns = c(
                  paste0('countValue_',1), 
                  paste0('averageValue_',1),
                  paste0('standardDeviation_',1),
                  paste0('medianValue_',1),
                  paste0('minValue_',1),
                  paste0('maxValue_',1)
                )
              )
              
              groupColumnsContinuous[[2]] <- reactable::colGroup(
                name = paste0('Comparator with ',countTable$minPriorObservation[2] ,' days prior obs (N = ',countTable$n[2],')'), 
                columns = c(
                  paste0('countValue_',2), 
                  paste0('averageValue_',2),
                  paste0('standardDeviation_',2),
                  paste0('medianValue_',2),
                  paste0('minValue_',2),
                  paste0('maxValue_',2)
                )
              )
              
              continuousCols <- characterizationCohortsColumnsContinuous()
              
              for(i in 1:2){
              newCols <- list(
                countValue = reactable::colDef(
                  name = 'Count',
                  header = withTooltip("Count",
                                       "Number of people with the covariate in the cohort."),
                  cell = function(value) {
                    if (value >= 0) value else paste0('< ', abs(value))
                  },
                  filterable = T
                ),
                averageValue = reactable::colDef(
                  name = 'Mean',
                  header = withTooltip("Mean",
                                       "The mean value of the covariate in the cohort"),
                  cell = function(value) {
                    if (value >= 0) round(value, digits = 3) else paste0('< ', abs(round(value, digits = 3)))
                  }
                ),
                standardDeviation = reactable::colDef(
                  name = 'StDev',
                  header = withTooltip("StDev",
                                       "The standard deviation value of the covariate in the cohort"),
                  cell = function(value) {
                    if (value >= 0) round(value, digits = 3) else paste0('< ', abs(round(value, digits = 3)))
                  }
                ),
                medianValue = reactable::colDef(
                  name = 'Median',
                  header = withTooltip("Median",
                                       "The median value of the covariate in the cohort."),
                  cell = function(value) {
                    round(value, digits = 3)
                  }
                ),
                minValue = reactable::colDef(
                  name = 'Min Value',
                  header = withTooltip("Min Value",
                                       "Minimum value of the covariate in the cohort"),
                  format = reactable::colFormat(digits = 3)
                ),
                maxValue = reactable::colDef(
                  name = 'Max Value',
                  header = withTooltip("Max Value",
                                       "Maximum value the covariate in the cohort"),
                  format = reactable::colFormat(digits = 3)
                ),
                p25Value = reactable::colDef(
                  show = FALSE,
                  header = withTooltip("25th %tile",
                                       "25th percentile value of the covariate in the cohort"),
                  format = reactable::colFormat(digits = 3)
                ),
                p75Value = reactable::colDef(
                  show = FALSE,
                  header = withTooltip("75th %tile",
                                       "75th percentile value of the covariate in the cohort"),
                  format = reactable::colFormat(digits = 3)
                ),
                p10Value = reactable::colDef(
                  show = FALSE,
                  header = withTooltip("10th %tile",
                                       "10th percentile value of the covariate in the cohort"),
                  format = reactable::colFormat(digits = 3)
                ),
                p90Value = reactable::colDef(
                  show = FALSE,
                  header = withTooltip("90th %tile",
                                       "90th percentile value of the covariate in the cohort"),
                  format = reactable::colFormat(digits = 3)
                )
              )
              names(newCols) <- paste0(names(newCols),'_',i)
              
              continuousCols <- append(
                continuousCols, 
                newCols
              )
              }
              
            resultTableServer(
              id = 'continuousTable',
              df = continuousTable,
              details = data.frame(
                Target = reactiveTargetRow()$cohortName,
                Comparator = reactiveComparatorRow$cohortName,
                Database = input$databaseName,
                Analysis = 'Cohort comparison within database'
              ),
              downloadedFileName = 'cohort_comparison_cont',
              colDefsInput = continuousCols, 
              columnGroups = groupColumnsContinuous,
              elementId = session$ns('continuous-table-filter')
            ) 

          
            # clean plot data
            if(nrow(resultTable) > 0){
              plotDf <- resultTable
              if(sum(is.na(plotDf)) > 0){
                plotDf <- plotDf %>%
                  tidyr::replace_na(list(
                    averageValue_1 = 0,
                    averageValue_2 = 0
                ))
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
          } # end if counts not NULL
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
    elementId 
    ){
  
  res <- list(
    covariateName = reactable::colDef(
      name = "Covariate Name",
      header = withTooltip(
        "Covariate Name",
        "The name of the covariate"
      ), 
      minWidth = 300
    ),
    covariateId = reactable::colDef(
      show = FALSE,
      header = withTooltip("Covariate ID",
                           "Unique identifier of the covariate")
    ),
    minPriorObservation = reactable::colDef(
      show = FALSE
    ), 
    SMD = reactable::colDef(
      name = "SMD",
      header = withTooltip("SMD",
                           "Standardized mean difference between the target and comparator percentages"),
      format = reactable::colFormat(digits = 3)
    ),
    absSMD = reactable::colDef(
      name = "absSMD",
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
      name = "Covariate Class",
      header = withTooltip(
        "Covariate Class",
        "Class/type of the covariate"
      )
    )
  )

  return(res)
}


characterizationCohortsColumnsContinuous <- function(){
  res <- list(
    covariateName = reactable::colDef(
      name = "Covariate Name",
      header = withTooltip(
        "Covariate Name",
        "The name of the covariate"
      ), 
      filterable = T, 
      minWidth = 300,
    ),
    covariateId = reactable::colDef(
      show = FALSE,
      header = withTooltip("Covariate ID",
                           "Unique identifier of the covariate")
    ),
    minPriorObservation = reactable::colDef(
      show = FALSE
    ),
    SMD = reactable::colDef(
      name = "SMD",
      header = withTooltip("SMD",
                           "Standardized mean difference"),
      format = reactable::colFormat(digits = 3)
    ),
    absSMD = reactable::colDef(
      name = "absSMD",
      header = withTooltip("absSMD",
                           "Absolute standardized mean difference"),
      format = reactable::colFormat(digits = 3)
    )
  )
  
  return(res)
}


characterizatonGetCohortData <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetIds,
    databaseIds,
    minThreshold = 0.01
){
  
  shiny::withProgress(message = 'characterizatonGetCohortData', value = 0, {
    
    shiny::incProgress(1/4, detail = paste("Checking inputs"))
    
  
  if(is.null(targetIds) |  is.null(databaseIds)){
    warning('Ids cannot be NULL')
   return(NULL)
  }
    
    shiny::incProgress(2/4, detail = paste("Extracting data"))
    
    result <- OhdsiReportGenerator::getCharacterizationCohortBinary(
      connectionHandler = connectionHandler,
      schema = resultDatabaseSettings$schema,
      cTablePrefix = resultDatabaseSettings$cTablePrefix,
      cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
      databaseTable = resultDatabaseSettings$databaseTable,
      targetIds = targetIds,
      databaseIds = databaseIds,
      minThreshold = minThreshold
    )
    
    shiny::incProgress(4/4, detail = paste("Done"))
  })
  
    return(result)
  
}


characterizatonGetCohortComparisonDataContinuous <- function(
  connectionHandler,
  resultDatabaseSettings,
  targetIds,
  databaseIds,
  minThreshold = 0.01
){
  
  shiny::withProgress(message = 'characterizatonGetCohortDataContinuous', value = 0, {
    
    shiny::incProgress(1/4, detail = paste("Checking inputs"))
    

  if(is.null(targetIds) |  is.null(databaseIds)){
    warning('Ids cannot be NULL')
    return(NULL)
  }
    
  targetIds <- unique(targetIds)
  databaseIds <- unique(databaseIds)
  
    
    shiny::incProgress(2/4, detail = paste("Extracting data"))
    
    result <- OhdsiReportGenerator::getCharacterizationCohortContinuous(
      connectionHandler = connectionHandler,
      schema = resultDatabaseSettings$schema,
      cTablePrefix = resultDatabaseSettings$cTablePrefix,
      cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
      databaseTable = resultDatabaseSettings$databaseTable,
      targetIds = targetIds,
      databaseIds = databaseIds,
      minThreshold = minThreshold
    )
    
    
    shiny::incProgress(4/4, detail = paste("Done"))
  })
  
  return(result)
}

