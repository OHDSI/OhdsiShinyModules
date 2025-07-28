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
          countTable <- characterizatonGetCohortCounts(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetIds = reactiveTargetRow()$cohortId,
            databaseIds = databaseIds()[databaseNames() %in% input$databaseNames]
          )
          
          output$helpTextBinary <- shiny::renderUI(
            shiny::helpText(paste0("This analysis shows the fraction of patients in the target cohort (restricted to first index date and requiring ",
                            countTable$minPriorObservation[1]," days observation prior to index) with a history of each binary features across databases."))
          )
          output$helpTextContinuous <- shiny::renderUI(
            shiny::helpText(paste0("This analysis shows the fraction of patients in the target cohort (restricted to first index date and requiring ",
                                   countTable$minPriorObservation[1]," days observation prior to index) with a history of each continuous features across databases."))
          )
          
          result <- characterizatonGetDatabaseComparisonData(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetIds = reactiveTargetRow()$cohortId,
            databaseIds = databaseIds()[databaseNames() %in% input$databaseNames],
            minThreshold = input$minThreshold
          )
          
          continuousTable <- characterizatonGetCohortComparisonDataContinuous(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetIds = reactiveTargetRow()$cohortId,
            databaseIds = databaseIds()[databaseNames() %in% input$databaseNames],
            pivot = F
          )
          
          databaseNamesResult <- result$databaseNames
          
          meanColumns <- lapply(1:nrow(databaseNamesResult), function(i){
            reactable::colDef(
              header = withTooltip(
                paste0('%'),
                paste0("The percentage of the target population in database ", databaseNamesResult$databaseName[i], ' who had the covariate prior.')
              ),
              cell = function(value) {
                if (value >= 0) paste0(round(value*100, digits = 3),' %') else '< min threshold'
              }
            )
          })
          names(meanColumns) <- unlist(lapply(1:nrow(databaseNamesResult), function(i) paste0('averageValue_',databaseNamesResult$id[i])))
          
          sumColumns <- lapply(1:nrow(databaseNamesResult), function(i){
            reactable::colDef(
              header = withTooltip(
                paste0("Count"),
                paste0("The number of people in the target cohort in database ", databaseNamesResult$databaseName[i], ' who have the covariate prior.')
              ),
              cell = function(value) {
                if (value >= 0) value else '< min threshold'
              }
            )
          })
          names(sumColumns) <- unlist(lapply(1:nrow(databaseNamesResult), function(i) paste0('sumValue_',databaseNamesResult$id[i])))
        
          # group columns with the counts
          
          getDbCount <- function(databaseName,minPriorObservation){
            countOfInt <- countTable %>% 
              dplyr::filter(.data$selection == !!databaseName) %>%
              dplyr::filter(.data$minPriorObservation == !!minPriorObservation) %>%
              dplyr::select("personCount")
            
            return(countOfInt$personCount)
          }
          
          groupColumns <- lapply(
            1:nrow(databaseNamesResult),
            function(i){
              reactable::colGroup(
                name = paste0(databaseNamesResult$databaseName[i], ' (N = ',getDbCount(databaseNamesResult$databaseName[i],countTable$minPriorObservation[1]),')'), 
                columns = c(
                  paste0('sumValue_',databaseNamesResult$id[i]), 
                  paste0('averageValue_',databaseNamesResult$id[i]))
                )
            }
          )

          # how to add counts here - in details?
          resultTableServer(
            id = 'mainTable',
            df = result$table %>% 
              dplyr::filter(.data$minPriorObservation == countTable$minPriorObservation[1]),
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
          
          resultTableServer(
            id = 'continuousTable',
            df = continuousTable %>% 
              dplyr::filter(.data$minPriorObservation == countTable$minPriorObservation[1]),
            details = data.frame(
              Target = reactiveTargetRow()$cohortName,
              Databases = selectedDatabases,
              `Minimum Covariate Threshold` = input$minThreshold,
              Analysis = 'Cohort comparison across databases'
            ),
            downloadedFileName = 'database_comparison_cont',
            colDefsInput = characterizationCohortsColumnsContinuous(
              elementId = session$ns('continuous-table-filter')
            ),
            elementId = session$ns('continuous-table-filter')
          )
          
        
          # get plot data - TODO replace this to do one data extraction!
          plotResult(characterizatonGetDatabaseComparisonDataRaw(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetIds = reactiveTargetRow()$cohortId,
            databaseIds = databaseIds()[databaseNames() %in% input$databaseNames],
            minThreshold = input$minThreshold
          ))
          
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
                                choices = unique(plotResult()$cdmSourceAbbreviation),
                                selected = unique(plotResult()$cdmSourceAbbreviation)[1],
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
                                choices = unique(plotResult()$cdmSourceAbbreviation),
                                selected = unique(plotResult()$cdmSourceAbbreviation)[2],
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
            
            plotDf <- plotResult() %>%
                dplyr::filter(.data$cdmSourceAbbreviation %in% c(input$xAxis, input$yAxis))
              
              # Group and split the data by cdmSourceAbbreviation
              plotDf <- plotDf %>%
                dplyr::group_by(.data$cdmSourceAbbreviation) %>%
                dplyr::group_split()
              
              # Initialize an empty list to store the processed dataframes
              processedDfs <- list()
              
              # Loop over the split datasets and process each one
              for (i in seq_along(plotDf)) {
                
                currentDbDf <- plotDf[[i]] %>%
                  dplyr::select("cdmSourceAbbreviation",
                                "covariateName",
                                "averageValue")
                
                # Ensure only rows with selected xAxis or yAxis inputs are kept
                currentDbDf <- currentDbDf %>%
                  dplyr::filter(.data$cdmSourceAbbreviation %in% c(input$xAxis, input$yAxis))
                
                # Get the name for this database (should be unique after filtering)
                dbName <- unique(currentDbDf$cdmSourceAbbreviation)
                
                # Rename the averageValue column based on the database name
                colnames(currentDbDf) <- c("cdmSourceAbbreviation", "covariateName", paste0(dbName, "_avg"))
                
                # Remove the cdmSourceAbbreviation column for joining later
                currentDbDf <- currentDbDf %>%
                  dplyr::select(-"cdmSourceAbbreviation")
                
                # Append the processed dataframe to the list
                processedDfs[[i]] <- currentDbDf
              }
              
              # Check if there's at least one dataframe to join
              if (length(processedDfs) > 1) {
                # Perform a left join across all processed dataframes
                plotDf <- Reduce(function(x, y) dplyr::left_join(x, y, by = "covariateName"), processedDfs)
              } else {
                # If there's only one dataframe, no need for joining
                plotDf <- processedDfs[[1]]
              }
              
              # Replace NA values with 0
              #plotDf[is.na(plotDf)] <- 0
              #plotDf <- plotDf %>%
              #  tidyr::replace_na(list(
              #    averageValue = 0
              #  ))
              
              plotDf <- plotDf %>%
                #replace(is.na(.), 0) %>%
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
              
              # Get the filtered and processed plot data
              plotData <- plotDf
              
              # Ensure that the reactive inputs are valid and accessible
              xAxisInput <- input$xAxis
              yAxisInput <- input$yAxis
              
              # Sanitize the xAxis and yAxis input values by replacing spaces with underscores
              xAxisSafe <- gsub(" ", "_", xAxisInput)
              yAxisSafe <- gsub(" ", "_", yAxisInput)
              
              # Sanitize column names in plotData to replace spaces with underscores
              colnames(plotData) <- gsub(" ", "_", colnames(plotData))
              
              # Ensure that the column names exist in plotData
              if (!all(c(paste0(xAxisSafe, "_avg"), paste0(yAxisSafe, "_avg")) %in% colnames(plotData))) {
                stop("Selected columns not found in data.")
              }
              
              # Create hover text for plotly
              plotData$hoverText <- paste(
                "Covariate Name:", plotData$covariateName, 
                "<br>", xAxisInput, ":", scales::percent(plotData[[paste0(xAxisSafe, "_avg")]]), 
                "<br>", yAxisInput, ":", scales::percent(plotData[[paste0(yAxisSafe, "_avg")]])
              )
              
              # Create the scatter plot with the diagonal line (x = y)
              p <- ggplot2::ggplot(plotData, ggplot2::aes_string(x = paste0(xAxisSafe, "_avg"),
                                                                 y = paste0(yAxisSafe, "_avg"),
                                                                 color = "domain",
                                                                 text = "hoverText")) +  # Use hoverText for hover labels
                ggplot2::geom_point(size = 2) +    # Smaller point size
                ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Diagonal x=y line in black
                ggplot2::labs(
                  x = paste0(xAxisInput, " %"),
                  y = paste0(yAxisInput, " %"),
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

characterizatonGetDatabaseComparisonData <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetIds,
    databaseIds,
    minThreshold
){
  
  result <- characterizatonGetCohortData(
    connectionHandler = connectionHandler,
    resultDatabaseSettings = resultDatabaseSettings,
    targetIds = targetIds,
    databaseIds = databaseIds,
    minThreshold = minThreshold,
    addSMD = length(databaseIds) == 2
  )
  
  databaseNames <- connectionHandler$queryDb(
    sql = "select cdm_source_abbreviation as database_name, database_id
     from @schema.@database_table;",
    schema = resultDatabaseSettings$schema,
    database_table = resultDatabaseSettings$databaseTable
  )
  
  databaseNames <- merge(
    databaseNames,
    data.frame(
      id = 1:length(databaseIds),
      databaseId = databaseIds
    ), 
    by = 'databaseId'
  )
  
  return(
    list(
      table = result, 
      databaseNames = databaseNames
    )
  )
  
}

characterizatonGetDatabaseComparisonDataRaw <- function(
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
  
  sql <- "select  d.cdm_source_abbreviation,
          ref.covariate_name, 
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
    inner join 
    @schema.@database_table d
    on cov.database_id = d.database_id
    
    where 
    cov.target_cohort_id in (@target_ids) 
    and cov.cohort_type = 'Target'
    AND cov.database_id in (@database_ids)
    AND cov.average_value >= @min_threshold;"
  
  # settings.min_characterization_mean needed?
  res <- connectionHandler$queryDb(
    sql = sql,
    target_ids = paste0(targetIds, collapse = ','),
    database_ids = paste0("'",databaseIds,"'", collapse = ','),
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    min_threshold = minThreshold,
    database_table = resultDatabaseSettings$databaseTable
  )
  
  return(res)
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

