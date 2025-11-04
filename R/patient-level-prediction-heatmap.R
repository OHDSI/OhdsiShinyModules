# @file patient-level-prediction-discrimination.R
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


#' The module viewer for exploring prediction model discrimination results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @family PatientLevelPrediction
#' @return
#' The user interface to the model discrimination results module
#'
#' @export
patientLevelPredictionHeatmapViewer <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::div(
    # table to select performance rows
    
    shiny::uiOutput(ns('plotOptions')),
    
    shiny::conditionalPanel(
      condition = "output.viewPlot == 1",
      ns = ns,
      
      shinydashboard::box( 
        title = 'Heatmap',
        solidHeader = TRUE,
        width = 12,
        status = 'info',
        
        shinycssloaders::withSpinner(
          shiny::plotOutput(
            outputId = ns('heatmap'), 
            width = "100%"
          )
        )
      )
      
    ) # cond panel
  )
}

#' The module server for exploring prediction model discrimination results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param performances a reactive val with the performance data.frame
#' @param performanceRowIds a vector of selected performance rows
#' @param connectionHandler the connection to the prediction result database
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#' @family PatientLevelPrediction
#' @return
#' The server to the model discrimination module
#'
#' @export
patientLevelPredictionHeatmapServer <- function(
    id, 
    performances, # reactive val
    performanceRowIds, # reactive val
    connectionHandler,
    resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$viewPlot <- shiny::reactive(0)
      shiny::outputOptions(output, "viewPlot", suspendWhenHidden = FALSE)
      
      output$plotOptions <- shiny::renderUI(
        
        shinydashboard::box(
          title = 'Heatmap Options',
          width = 12, 
          collapsible = TRUE,
          
          shiny::fluidRow(
            style = "background-color: #DCDCDC; width: 98%; margin-left: 1%;margin-right: 1%;", # Apply style directly to fluidRow
            
            shiny::column(
              width = 3,
              shiny::selectInput(
                inputId = session$ns('xAxis'),
                label = 'xAxis', 
                choices = c('developmentTargetName', 'validationTargetName', 
                            'developmentOutcomeName', 'validationOutcomeName',
                            'developmentTimeAtRisk','validationTimeAtRisk',
                            'developmentDatabase', 'validationDatabase',
                            'modelType', 'evaluation'
                            ), 
                selected = 'developmentTargetName', 
                multiple = FALSE
              )
            ),
            
            shiny::column(
              width = 3,
              shiny::selectInput(
                inputId = session$ns('yAxis'),
                label = 'yAxis', 
                choices = c('developmentTargetName', 'validationTargetName', 
                            'developmentOutcomeName', 'validationOutcomeName',
                            'developmentTimeAtRisk','validationTimeAtRisk',
                            'developmentDatabase', 'validationDatabase',
                            'modelType', 'evaluation'
                ),
                selected = 'developmentOutcomeName', 
                multiple = FALSE
              )
            ),
            
            shiny::column(
              width = 3,
              shiny::selectInput(
                inputId = session$ns('metric'),
                label = 'Metric', 
                choices = c('AUROC', 'AUPRC','brier score','brier score scaled', 'Eavg', 'CalibrationInTheLargeRatio','CalibrationInTheLargeDiff'), 
                selected = 'AUROC', 
                multiple = FALSE
              )
            ),
          
            
            shiny::column(
              width = 3,
              shiny::div(
                style = "padding-top: 25px; padding-left: 10px;",
                shiny::actionButton(
                  inputId = session$ns('generateHeatmap'), 
                  label = 'Generate Heatmap' 
                )
              )
            )
            
          ) # fluid row
        ) # box
      ) # renderUI
      
      # remove plot if the performance selection changes
      shiny::observeEvent(
        eventExpr = performanceRowIds(), {
          output$viewPlot <- shiny::reactive(0)
        })
      
      shiny::observeEvent(
        input$generateHeatmap,
        {
          if(!is.null(performanceRowIds())){
            if(sum(performanceRowIds()) != 0){
            
                output$viewPlot <- shiny::reactive(1)
                
                shiny::withProgress(
                  message = 'Generating plot', 
                  detail = 'This may take a while...', 
                  value = 0.2,
                  expr = {
                    plotObject <-tryCatch(
                      expr = {
                        
                        if(input$metric %in% c('CalibrationInTheLargeRatio','CalibrationInTheLargeDiff')){
                          data <- performances()[performanceRowIds(),] %>%
                            dplyr::mutate(
                              CalibrationInTheLargeRatio = .data$`calibrationInLarge mean prediction`/.data$`calibrationInLarge observed risk`,
                              CalibrationInTheLargeDiff = abs(.data$`calibrationInLarge mean prediction` - .data$`calibrationInLarge observed risk`)
                            )
                        } else{
                          data <- performances()[performanceRowIds(),]
                        }
                        
                        data$xAxis <- unlist(lapply(data[,input$xAxis], function(x){paste(strwrap(x, width = 20), collapse = "\n")}))
                        data$yAxis <- unlist(lapply(data[,input$yAxis], function(x){paste(strwrap(x, width = 20), collapse = "\n")}))
                        data$fill <- data[,input$metric]
                        
                        
                        
                        ggplot2::ggplot(
                          data = data, 
                          mapping = ggplot2::aes(
                            x = .data$xAxis, 
                            y = .data$yAxis, 
                            fill = .data$fill
                            )
                          ) +
                          ggplot2::geom_tile()
                      },
                      error = function(e){print(e);NULL}
                    )
                    
                    shiny::incProgress(1)
                    
                  })
                
              } else{
                output$viewPlot <- shiny::reactive(0)
                plotObject <- NULL
              }
              
              output$heatmap <- shiny::renderPlot({
                tryCatch({plotObject}, error = function(err){print(err);NULL})
              })
              
          } else{
            output$viewPlot <- shiny::reactive(0)
          }
          
        }
      )
      
    } 
  )
}
