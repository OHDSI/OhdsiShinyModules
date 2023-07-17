# @file patient-level-prediction-netbenefit.R
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


#' The module viewer for exploring prediction net-benefit results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the net-benefit module
#'
#' @export
patientLevelPredictionNbViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info', 
        width = 12,
        title = 'Select net benefit type to view:',
        solidHeader = TRUE,
        shiny::uiOutput(ns('nbSelect'))
      )
    ),
    
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info', 
        width = 6,
        title = 'Net Benefit Plot',
        solidHeader = TRUE,
        side = "right",
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns('nbPlot'))
        )
      ),
      
      shinydashboard::box(
        status = 'info', 
        width = 6,
        title = 'Summary',
        solidHeader = TRUE,
        DT::dataTableOutput(ns('nbTable'))
      )
    )
  )
}

#' The module server for exploring prediction net-benefit results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param performanceId the performance id in the database
#' @param connectionHandler the connection to the prediction result database
#' @param inputSingleView the current tab 
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#' 
#' @return
#' The server to the net-benefit module
#'
#' @export
patientLevelPredictionNbServer <- function(
  id, 
  performanceId, # reactive
  connectionHandler,
  inputSingleView,
  resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      thresholdSummary <- shiny::reactive(
        {
          if(!is.null(performanceId()) & inputSingleView() == 'Net Benefit'){
            
           getPredictionResult(
              performanceId = performanceId, 
              connectionHandler= connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              tableName = 'threshold_summary'
            )
          } else{
            NULL
          } 
        }        
      )
      
      output$nbSelect = shiny::renderUI({
        shiny::selectInput(
          inputId = session$ns('nbSelectInput'), 
          label = 'Type:', 
          choices = unique(thresholdSummary()$evaluation), 
          multiple = F, 
          selectize=FALSE
        )
      })
      
      output$nbTable <- DT::renderDataTable({
        if(is.null(thresholdSummary)){
          return(NULL)
        } else{
          result <- extractNetBenefit(
            thresholdSummary = thresholdSummary(), 
            type=trimws(input$nbSelectInput)
          )
          #unique(result)
          result$treatAll <- format(result$treatAll, digits = 2, scientific = F)
          result$netBenefit <- format(result$netBenefit, digits = 2, scientific = F)
          result
        }
      })
      
      output$nbPlot <- shiny::renderPlot({
        if(is.null(thresholdSummary())){
          return(NULL)
        } else{
          result <- extractNetBenefit(
            thresholdSummary = thresholdSummary(), 
            type = trimws(input$nbSelectInput)
          )
          result <- unique(result)
          ind <- !is.na(result$netBenefit) & is.finite(result$netBenefit) & !is.null(result$netBenefit) & is.finite(result$pt)
          
          df2 <- tidyr::pivot_longer(
            data = result, 
            cols = colnames(result)[colnames(result) != 'pt'], 
            names_to = 'variable', 
            values_to = 'value'
          )
          
          
          ggplot2::ggplot(
            data = df2, 
            ggplot2::aes(
              x = .data$pt, 
              y = .data$value, 
              group = .data$variable, 
              color = .data$variable
            )
          ) +
            ggplot2::geom_line(
              ggplot2::aes(
                linetype = .data$variable
                )
              )+
            ggplot2::geom_point(
              ggplot2::aes(
                shape = .data$variable
                )
              )
        }
      })
      
    }
  )
}

extractNetBenefit <- function(thresholdSummary, type=NULL, modelId=NULL){
  data <- thresholdSummary
  
  if(!is.null(type)){
    if(!is.null(data$evaluation[1])){
      data <- data[data$evaluation==type,]
    }
  }
  
  pt <- data$predictionThreshold
  TP <- data$truePositiveCount
  FP <- data$falsePositiveCount
  n <- data$positiveCount + data$negativeCount
  
  treatAll <- data$trueCount/n-data$falseCount/n*(pt/(1-pt))
  
  if(!is.null(modelId[1])){
    netbenefit <- data.frame(
      modelId = modelId, 
      pt = pt, 
      netBenefit = TP/n-(FP/n)*(pt/(1-pt)),
      treatAll = treatAll
      )
  }else{
    netbenefit <- data.frame(
      pt = pt, 
      netBenefit = TP/n-(FP/n)*(pt/(1-pt)),
      treatAll = treatAll
      )
  }
  
  return(netbenefit)
}
