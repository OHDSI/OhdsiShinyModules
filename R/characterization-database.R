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
characterizationDatabaseComparisonViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    
    shiny::helpText('Compare covariates at index between two databases for the same cohort.'),
    
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
      condition = "output.showDatabase != 0",
      ns = ns,
      
      inputSelectionDfViewer(id = ns('inputSelected'), title = 'Selected'),
      
      # add basic table 
      shiny::tabsetPanel(
        type = 'pills',

        shiny::tabPanel(
          title = 'Binary Table ',
          shiny::uiOutput(outputId = ns('helpTextBinary')),
          resultTableViewer(id = ns('mainTable'), boxTitle = 'Binary')
        ),
        
        shiny::tabPanel(
          title = "Binary Plot",
          shiny::helpText('Pick two databases and compare binary features across the databases.'),
          shiny::uiOutput(ns('plotInputs')),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns('scatterPlot'))
          )
        ),
        
        shiny::tabPanel(
          title = 'Continuous Table',
          shiny::uiOutput(outputId = ns('helpTextContinuous')),
          resultTableViewer(id = ns('continuousTable'), boxTitle = 'Continuous')
        )
      )
    )
  )
}



characterizationDatabaseComparisonServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings,
    reactiveTargetRow
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # initially do not show results
      output$showDatabase <- shiny::reactive(0)
      shiny::outputOptions(output, "showDatabase", suspendWhenHidden = FALSE)
      
      # if target or outcome changes hide results
      shiny::observeEvent(reactiveTargetRow(), {
        output$showDatabase <- shiny::reactive(0)
      })
      
      databaseNames <- shiny::reactive(unlist(strsplit(x = reactiveTargetRow()$databaseString, split = ', ')))
      databaseIds <- shiny::reactive(unlist(strsplit(x = reactiveTargetRow()$databaseIdString, split = ', ')))
      
      # get min char value:
      # set this to the min threshold used in analysis: covariates.min_characterization_mean
      minCharVal <- getMinCovaraiteThreshold(
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      output$inputs <- shiny::renderUI({
        
        shiny::div(
          shiny::fluidRow(
            shiny::column(
              width = 8,
              shinyWidgets::pickerInput(
                inputId = session$ns('databaseNames'), 
                label = 'Databases: ',
                choices = databaseNames(),
                selected = databaseNames()[1],
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
            
            shiny::column(
              width = 4,
              shiny::sliderInput(
                inputId = session$ns('minThreshold'), 
                label = 'Covariate Threshold', 
                min = minCharVal, 
                max = 1, 
                value = 0.01, 
                step = 0.01, 
                ticks = F
              )
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
      plotResult <- shiny::reactiveVal(NULL)
      shiny::observeEvent(input$generate,{
        
        if(is.null(input$databaseNames) | is.null(reactiveTargetRow())){
          output$showDatabase <- shiny::reactive(0)
          shiny::showNotification('No databases selected')
        } else {
        if(length(input$databaseNames) == 0 | nrow(reactiveTargetRow()) == 0){
          output$showDatabase <- shiny::reactive(0)
          shiny::showNotification('No databases selected')
        } else {
        
        output$showDatabase <- shiny::reactive(1)
        
        selectedDatabases <- paste0(input$databaseNames, collapse =  ', ')
        
        selected(
          data.frame(
            Target = reactiveTargetRow()$cohortName[1],
            Databases = selectedDatabases,
            `Minimum Covariate Threshold` = input$minThreshold
          )
        )


        #get results
        
        result <- characterizatonGetCohortData(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          targetIds = reactiveTargetRow()$cohortId,
          databaseIds = databaseIds()[databaseNames() %in% input$databaseNames],
          minThreshold = input$minThreshold
        )
        
        resultTable <- result$covariates
        countTable <- result$covRef
          
          output$helpTextBinary <- shiny::renderUI(
            shiny::helpText(paste0("This analysis shows the fraction of patients in the target cohort (restricted to first index date and requiring ",
                            countTable$minPriorObservation[1]," days observation prior to index) with a history of each binary features across databases."))
          )
          output$helpTextContinuous <- shiny::renderUI(
            shiny::helpText(paste0("This analysis shows the fraction of patients in the target cohort (restricted to first index date and requiring ",
                                   countTable$minPriorObservation[1]," days observation prior to index) with a history of each continuous features across databases."))
          )
          
          # this will pivot now and needs addressing
          continuous <- characterizatonGetCohortComparisonDataContinuous(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetIds = reactiveTargetRow()$cohortId,
            databaseIds = databaseIds()[databaseNames() %in% input$databaseNames]
          )
          
          continuousTable <- continuous$covariates
          
          #databaseNamesResult <- result$databaseNames
          
          # figure out the column names and how to present them to reactable
          meanColumns <- lapply(1:nrow(countTable), function(i){
            reactable::colDef(
              header = withTooltip(
                paste0('%'),
                paste0("The percentage of the target population in database ", countTable$databaseName[i], ' who had the covariate prior.')
              ),
              cell = function(value) {
                if (value >= 0) paste0(round(value*100, digits = 3),' %') else '< min threshold'
              }
            )
          })
          names(meanColumns) <- unlist(lapply(countTable$id, function(i) paste0('averageValue_',i)))
          
          sumColumns <- lapply(1:nrow(countTable), function(i){
            reactable::colDef(
              header = withTooltip(
                paste0("Count"),
                paste0("The number of people in the target cohort in database ", countTable$databaseName[i], ' who have the covariate prior.')
              ),
              cell = function(value) {
                if (value >= 0) value else '< min threshold'
              }
            )
          })
          names(sumColumns) <- unlist(lapply(countTable$id, function(i) paste0('sumValue_',i)))
        
          # group columns with the counts
          
          
          groupColumns <- list()
          
          for(i in 1:nrow(countTable)){
            groupColumns[[length(groupColumns) + 1]] <- reactable::colGroup(
              name = paste0(countTable$databaseName[i], ' with ',countTable$minPriorObservation[i] ,' days prior obs (N = ',countTable$n[i],')'), 
              columns = c(
                paste0('sumValue_',countTable$id[i]), 
                paste0('averageValue_',countTable$id[i]))
            )
          }
          
          # how to add counts here - in details?
          resultTableServer(
            id = 'mainTable',
            df = resultTable,
            details = data.frame(
              Target = reactiveTargetRow()$cohortName,
              Databases = selectedDatabases,
              `Minimum Covariate Threshold` = input$minThreshold,
              Analysis = 'Cohort comparison across databases'
            ),
            downloadedFileName = 'database_comparison_binary',
            colDefsInput = append(
              characterizationCohortsColumns(
                elementId = session$ns('main-table-filter')
              ),
              append(
                sumColumns,
                meanColumns
              )
            ),
            columnGroups = groupColumns,
            elementId = session$ns('main-table-filter')
          )
          
          
          # create group columns for continuous
          groupColumnsContinuous <- list()
          continuousCols <- characterizationCohortsColumnsContinuous()
          
          for(i in 1:nrow(countTable)){
            groupColumnsContinuous[[length(groupColumnsContinuous) + 1]] <- reactable::colGroup(
              name = paste0(countTable$databaseName[i], ' with ',countTable$minPriorObservation[i] ,' days prior obs (N = ',countTable$n[i],')'), 
              columns = c(
                paste0('countValue_',countTable$id[i]), 
                paste0('averageValue_',countTable$id[i]),
                paste0('standardDeviation_',countTable$id[i]),
                paste0('medianValue_',countTable$id[i]),
                paste0('minValue_',countTable$id[i]),
                paste0('maxValue_',countTable$id[i])
            )
            )
            
            newCols <- list(
              countValue = reactable::colDef(
                header = withTooltip("Count",
                                     "Number of people with the covariate in the cohort."),
                cell = function(value) {
                  if (value >= 0) value else paste0('< ', abs(value))
                },
                filterable = T
              ),
              averageValue = reactable::colDef(
                header = withTooltip("Mean",
                                     "The mean value of the covariate in the cohort"),
                cell = function(value) {
                  if (value >= 0) round(value, digits = 3) else paste0('< ', abs(round(value, digits = 3)))
                }
              ),
              standardDeviation = reactable::colDef(
                header = withTooltip("StDev",
                                     "The standard deviation value of the covariate in the cohort"),
                cell = function(value) {
                  if (value >= 0) round(value, digits = 3) else paste0('< ', abs(round(value, digits = 3)))
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
            names(newCols) <- paste0(names(newCols),'_',countTable$id[i])
            
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
              Databases = selectedDatabases,
              `Minimum Covariate Threshold` = input$minThreshold,
              Analysis = 'Cohort comparison across databases'
            ),
            downloadedFileName = 'database_comparison_cont',
            colDefsInput = continuousCols, 
            columnGroups = groupColumnsContinuous,
            elementId = session$ns('continuous-table-filter')
          )
          
          plotResult(result)
        }
        }
      })
      
      
          
          #scatterplots
          output$plotInputs <- shiny::renderUI({
            shiny::div(
              shiny::fluidRow(
                shiny::column(width = 5,
                              shinyWidgets::pickerInput(
                                inputId = session$ns('xAxis'), 
                                label = 'X-Axis Database: ',
                                choices = unique(plotResult()$covRef$databaseName),
                                selected = unique(plotResult()$covRef$databaseName)[1],
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
                shiny::column(width = 5,
                              shinyWidgets::pickerInput(
                                inputId = session$ns('yAxis'), 
                                label = 'Y-Axis Database: ',
                                choices = unique(plotResult()$covRef$databaseName),
                                selected = unique(plotResult()$covRef$databaseName)[2],
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
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shiny::actionButton(
                  inputId = session$ns('generatePlot'), 
                  label = 'Generate Plot'
                  )
                )
              )
            )
          })
          
          #plot when generate plot is pressed
          output$scatterPlot <- NULL
          shiny::observeEvent(input$generatePlot,{
            
            # TODO add a check to make sure plotResult() has results
            
            countInd1 <- which.max(plotResult()$covRef$databaseName == input$xAxis)
            countInd2 <- which.max(plotResult()$covRef$databaseName == input$yAxis)
            
            plotData <- plotResult()$covariates %>%
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
          
          #plot
          output$scatterPlot <- plotly::renderPlotly({
              
              # TODO - edit this to jsut use plotly...
            
              # Create hover text for plotly
              plotData$hoverText <- paste(
                "Covariate Name:", plotData$covariateName, 
                "<br>", plotResult()$covRef$databaseName[countInd1], ":", scales::percent(plotData[[paste0("averageValue_",plotResult()$covRef$id[countInd1])]]), 
                "<br>", plotResult()$covRef$databaseName[countInd2], ":", scales::percent(plotData[[paste0("averageValue_",plotResult()$covRef$id[countInd2])]])
              )
              
              # Create the scatter plot with the diagonal line (x = y)
              p <- ggplot2::ggplot(plotData, ggplot2::aes_string(x = paste0("averageValue_",plotResult()$covRef$id[countInd1]),
                                                                 y = paste0("averageValue_",plotResult()$covRef$id[countInd2]),
                                                                 color = "domain",
                                                                 text = "hoverText")) +  # Use hoverText for hover labels
                ggplot2::geom_point(size = 2) +    # Smaller point size
                ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Diagonal x=y line in black
                ggplot2::labs(
                  x = paste0(plotResult()$covRef$databaseName[countInd1], " %"),
                  y = paste0(plotResult()$covRef$databaseName[countInd2], " %"),
                  color = "Domain"
                ) +
                ggplot2::theme_minimal() +          # Optional: use a clean theme
                ggplot2::theme(
                  legend.position = "right",        # Position legend as needed
                  axis.title = ggplot2::element_text(size = 12),  # Adjust axis title size
                  axis.text = ggplot2::element_text(size = 10)    # Adjust axis text size
                ) +
                ggplot2::scale_x_continuous(labels = scales::percent_format()) +  # Format x-axis as percentage
                ggplot2::scale_y_continuous(labels = scales::percent_format())    # Format y-axis as percentage
              
              # Convert to a plotly object for interactivity
              plotly::ggplotly(p, tooltip = "text")  # Use the custom hover text
            })
          
          
          }) # end generate plot observe event
          
      return(invisible(NULL))
      
    }) #end server
  
}



getMinCovaraiteThreshold <- function(
  connectionHandler,
  resultDatabaseSettings ){
  
  sql <- "select top 1  min_characterization_mean val from
     @schema.@c_table_prefixcovariates;"
  
  res <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix
  )
  
  return(res$val)
}

