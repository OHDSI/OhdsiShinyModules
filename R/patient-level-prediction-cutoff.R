# @file patient-level-prediction-cutoff.R
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

patientLevelPredictionCutoffViewer <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::div(
    shinydashboard::box(
      width = 12,
      title = "Cutoff Slider: ",
      status = "info", 
      solidHeader = TRUE,
      
      shiny::fluidRow(
        shiny::column(
          width = 8,
          
          shiny::sliderInput(
            inputId = ns("slider1"), 
            shiny::span(
              "Pick Threshold ", 
              shiny::textOutput('threshold'), 
              style="font-family: Arial;font-size:14px;"
            ), 
            min = 1, 
            max = 100, 
            value = 50, 
            ticks = F
          )
        ),
        shiny::column(
          width = 4,
          shinydashboard::infoBoxOutput(
            outputId = ns("performanceBoxThreshold")
          )
        )
      )
    ),
    
    shinydashboard::box(
      width = 12,
      title = "Cutoff Performance",
      status = "info", solidHeader = TRUE,
      reactable::reactableOutput(ns('performance'))
    )
  )
  
}


patientLevelPredictionCutoffServer <- function(
    id, 
    performances,
    performanceRowIds,
    connectionHandler,
    resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      thresholdSummary <- shiny::reactiveVal(NULL)
      probabilities <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(
        eventExpr = performanceRowIds(), {
          
          if(length(performanceRowIds()) > 0 & max(performanceRowIds()) != 0){
            
            result <- OhdsiReportGenerator::getPredictionPerformanceTable(
              connectionHandler = connectionHandler, 
              schema = resultDatabaseSettings$schema, 
              plpTablePrefix = resultDatabaseSettings$plpTablePrefix, 
              databaseTable = resultDatabaseSettings$databaseTable, 
              table = 'threshold_summary', 
              performanceIds = performances()$performanceId[performanceRowIds()]
            )
            
            if( nrow(result) > 0){
              # restrict to the selected performance/evals
              result <- merge(
                x = result, 
                y = performances()[performanceRowIds(), c('performanceId','evaluation')]
              )
            
              thresholdSummary(result)
              probabilities(sort(unique(result$predictionThreshold)))
            }
          }
          
        })
      
      shiny::observeEvent(
        thresholdSummary(),
        {
          if(!is.null(thresholdSummary())){
            n <- length(unique(thresholdSummary()$predictionThreshold))
          }else{
            n <- 100
          }
          shiny::updateSliderInput(
            session, 
            inputId = "slider1",  
            min = 1, 
            max = n, 
            value = round(n/2)
          )
        }
      )
      
 
      performance <- shiny::reactive(
        if(is.null(thresholdSummary()) | is.null(input$slider1)){
          return(NULL)
        } else {
          result <- processedThresholds(
            data = thresholdSummary(),
            probability = probabilities()[input$slider1],
            performances = performances()[performanceRowIds(),]
          )
          return(result)
        }
      )
      
      # Do the tables and plots:
      output$performance <- reactable::renderReactable(
        reactable::reactable(
          data = performance()$table, 
          columns = list(
            model = reactable::colDef(minWidth = 200),
            incidence = reactable::colDef(show = FALSE),
            truePositiveCount = reactable::colDef(
              name = "TP", 
              format = reactable::colFormat(digits = 0)
            ),
            falsePositiveCount = reactable::colDef(
              name = "FP", 
              format = reactable::colFormat(digits = 0)
            ),
            trueNegativeCount = reactable::colDef(
              name = "TN", 
              format = reactable::colFormat(digits = 0)
            ),
            falseNegativeCount = reactable::colDef(
              name = "FN", 
              format = reactable::colFormat(digits = 0)
            ),
            sensitivity = reactable::colDef(
              name = "Sensitivity", 
              format = reactable::colFormat(digits = 3)
            ),
            specificity = reactable::colDef(
              name = "Specificity", 
              format = reactable::colFormat(digits = 3)
            ),
            positivePredictiveValue = reactable::colDef(
              name = "PPV", 
              format = reactable::colFormat(digits = 3)
            ),
            negativePredictiveValue = reactable::colDef(
              name = "NPV", 
              format = reactable::colFormat(digits = 3)
            )
          )
        )
      )
      
      output$threshold <- shiny::renderText(
        format(performance()$threshold, digits=5)
      )
      
      # dashboard
      output$performanceBoxThreshold <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          "Threshold", format((performance()$threshold), scientific = F, digits=3), icon = shiny::icon("edit"),
          color = "black"
        )
      })
      
      
    } 
  )
}


processedThresholds <- function(
    data,
    probability,
    performances
){
  
  #get the last prob before probability
  
  lastRow <- data %>% 
    dplyr::filter(.data$predictionThreshold < !!probability) %>%
    dplyr::group_by(.data$performanceId, .data$evaluation) %>% 
    dplyr::arrange(.data$predictionThreshold) %>%  
    dplyr::slice(dplyr::n()) %>%
    dplyr::mutate(
      incidence = .data$trueCount/(.data$trueCount + .data$falseCount)
    ) %>%
    dplyr::select(
      'predictionThreshold',
      'performanceId',
      'evaluation',
      'incidence',
      'truePositiveCount',
      'falsePositiveCount',
      'trueNegativeCount',
      'falseNegativeCount',
      'sensitivity',
      'specificity',
      'positivePredictiveValue',
      'negativePredictiveValue'
    )
  
  # add databaseName, targetName, outcomeName, tar
  lastRow <- merge(
    x= lastRow,
    y = unique(performances[, c('performanceId','developmentDatabase',
                     'developmentTargetName','developmentOutcomeName',
                     'developmentTimeAtRisk')]),
    by = 'performanceId'
  )
  
  if(FALSE){
  lastRow <- lastRow %>% tidyr::pivot_longer(
    cols = c('incidence',
             'truePositiveCount',
             'falsePositiveCount',
             'trueNegativeCount',
             'falseNegativeCount',
             'sensitivity',
             'specificity',
             'positivePredictiveValue',
             'negativePredictiveValue'
             ), 
    names_to = 'metric', 
    values_to = 'value'
      ) %>%
    tidyr::pivot_wider(
      id_cols = 'metric', 
      names_from = c('performanceId','evaluation', 'developmentDatabase',
                     'developmentTargetName', 'developmentOutcomeName',
                     'developmentTimeAtRisk'), 
      names_glue = 'Performance {performanceId}: {evaluation} data in {developmentDatabase} for task predicting {developmentOutcomeName} in {developmentTargetName} during {developmentTimeAtRisk}',
      values_from = 'value'
    )
}
  
  lastRow$model <- paste0('Performance ',lastRow$performanceId,': ',lastRow$evaluation,' data in ',lastRow$developmentDatabase,' for task predicting ',lastRow$developmentOutcomeName,' in ',lastRow$developmentTargetName,' during ',lastRow$developmentTimeAtRisk)
  
  lastRow <- lastRow %>% dplyr::select(
    c('model',
      'incidence',
      'truePositiveCount',
      'falsePositiveCount',
      'trueNegativeCount',
      'falseNegativeCount',
      'sensitivity',
      'specificity',
      'positivePredictiveValue',
      'negativePredictiveValue'
    )
  )
  
  return(
    list(
      table = lastRow,
      threshold = probability
    )
  )
}