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


#' The module viewer for exploring prediction cut-off results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @family PatientLevelPrediction
#' @return
#' The user interface to the prediction cut-off module
#'
#' @export
patientLevelPredictionCutoffViewer <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    
    shiny::column(width = 12,
                  
                  shinydashboard::box(
                    width = 12,
                    title = "Probability threshold plot: ",
                    status = "info", 
                    solidHeader = TRUE,
                    plotly::plotlyOutput(ns("ptp"))                 
                  ),
                  
                  shinydashboard::box(
                    width = 12,
                    title = "Cutoff Slider: ",
                    status = "info", 
                    solidHeader = TRUE,
                    shiny::sliderInput(
                      ns("slider1"), 
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
                  
                  shinydashboard::box(
                    width = 12,
                    title = "Dashboard",
                    status = "warning", solidHeader = TRUE,
                    shinydashboard::infoBoxOutput(ns("performanceBoxThreshold")),
                    shinydashboard::infoBoxOutput(ns("performanceBoxIncidence")),
                    shinydashboard::infoBoxOutput(ns("performanceBoxPPV")),
                    shinydashboard::infoBoxOutput(ns("performanceBoxSpecificity")),
                    shinydashboard::infoBoxOutput(ns("performanceBoxSensitivity")),
                    shinydashboard::infoBoxOutput(ns("performanceBoxNPV")
                    )
                  ),
                  
                  shinydashboard::box(
                    width = 12,
                    title = "Cutoff Performance",
                    status = "warning", solidHeader = TRUE,
                    shiny::tableOutput(ns('twobytwo'))
                  )
    )
  )
}

#' The module server for exploring prediction cut-off results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param performanceId the performance id in the database
#' @param connectionHandler the connection to the prediction result database
#' @param inputSingleView the current tab 
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#' @family PatientLevelPrediction
#' @return
#' The server to the prediction cut-off module
#'
#' @export
patientLevelPredictionCutoffServer <- function(
  id, 
  performanceId, 
  connectionHandler,
  inputSingleView,
  resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      thresholdSummary <- shiny::reactive({
        if(!is.null(performanceId()) & inputSingleView() == 'Threshold Dependant'){

          value <- getPredictionResult(
            performanceId = performanceId,
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            tableName = 'threshold_summary'
          )
          return(value)
        } else{
          return(NULL)
        }
        
      })
      
      shiny::observeEvent(
        thresholdSummary(),
        {
          if(!is.null(thresholdSummary())){
            n <- nrow(thresholdSummary()[thresholdSummary()$evaluation%in%c('Test','Validation'),])
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
      
      # add probability threshold plot
      output$ptp <- plotly::renderPlotly(
        if(is.null(thresholdSummary())){
          return(NULL)
        } else {
          probThresPlot(
            thresholdSummary = thresholdSummary(), 
            pointOfInterest = input$slider1
          )
        }
      )
      
      
      performance <- shiny::reactive(
        if(is.null(thresholdSummary()) | is.null(input$slider1)){
          return(NULL)
        } else {
          intPlot <- getORC(thresholdSummary(), input$slider1)
          
          TP <- intPlot$TP
          FP <- intPlot$FP
          TN <- intPlot$TN
          FN <- intPlot$FN
          twobytwo <- as.data.frame(matrix(c(FP,TP,TN,FN), byrow=T, ncol=2))
          colnames(twobytwo) <- c('Ground Truth Negative','Ground Truth Positive')
          rownames(twobytwo) <- c('Predicted Positive','Predicted Negative')
          
          list(
            threshold = intPlot$threshold, 
            prefthreshold = intPlot$prefthreshold,
            twobytwo = twobytwo,
            Incidence = (TP+FN)/(TP+TN+FP+FN),
            Threshold = intPlot$threshold,
            Sensitivity = TP/(TP+FN),
            Specificity = TN/(TN+FP),
            PPV = TP/(TP+FP),
            NPV = TN/(TN+FN) 
          )
        }
      )
      
      
      # Do the tables and plots:
      
      output$performance <- shiny::renderTable(
        performance()$performance, 
        rownames = F, 
        digits = 3
      )
      
      output$twobytwo <- shiny::renderTable(
        performance()$twobytwo, 
        rownames = T, 
        digits = 0
      )
      
      output$threshold <- shiny::renderText(
        format(performance()$threshold, digits=5)
      )
      
      # dashboard
      
      output$performanceBoxIncidence <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          "Incidence", paste0(round(performance()$Incidence*100, digits=3),'%'), icon = shiny::icon("ambulance"),
          color = "green"
        )
      })
      
      output$performanceBoxThreshold <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          "Threshold", format((performance()$Threshold), scientific = F, digits=3), icon = shiny::icon("edit"),
          color = "yellow"
        )
      })
      
      output$performanceBoxPPV <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          "PPV", paste0(round(performance()$PPV*1000)/10, "%"), icon = shiny::icon("thumbs-up"),
          color = "orange"
        )
      })
      
      output$performanceBoxSpecificity <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          "Specificity", paste0(round(performance()$Specificity*1000)/10, "%"), icon = shiny::icon("bullseye"),
          color = "purple"
        )
      })
      
      output$performanceBoxSensitivity <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          "Sensitivity", paste0(round(performance()$Sensitivity*1000)/10, "%"), icon = shiny::icon("low-vision"),
          color = "blue"
        )
      })
      
      output$performanceBoxNPV <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          "NPV", paste0(round(performance()$NPV*1000)/10, "%"), icon = shiny::icon("minus-square"),
          color = "black"
        )
      })
      
    } 
  )
}




getORC <- function(thresholdSummary, pointOfInterest){
  
  data <- thresholdSummary[thresholdSummary$evaluation%in%c('Test','Validation'),]
  data <- data[order(data$predictionThreshold),]
  pointOfInterest <- data[pointOfInterest,]
  
  threshold <- pointOfInterest$predictionThreshold
  TP <- pointOfInterest$truePositiveCount
  TN <- pointOfInterest$trueNegativeCount
  FP <- pointOfInterest$falsePositiveCount
  FN <- pointOfInterest$falseNegativeCount
  preferenceThreshold <- pointOfInterest$preferenceThreshold
  return(list(threshold = threshold, prefthreshold=preferenceThreshold,
              TP = TP, TN=TN,
              FP= FP, FN=FN))
}

probThresPlot <- function(thresholdSummary, pointOfInterest){
  
  eval <- thresholdSummary[thresholdSummary$evaluation%in%c('Test','Validation'),]
  eval <- eval[order(eval$predictionThreshold),]
  
  ay <- list(
    tickfont = list(color = "red"),
    overlaying = "y",
    side = "right",
    title = "positivePredictiveValue y-axis"
  )
  vline <- function(x = 0, color = "green") {
    list(
      type = "line",
      y0 = 0,
      y1 = 1,
      yref = "paper",
      x0 = x,
      x1 = x,
      line = list(color = color, dash="dot")
    )
  }
  
  eval$popfrac <- eval$positiveCount/(eval$positiveCount+eval$negativeCount)
  
  fig <- plotly::plot_ly(
    data = eval, 
    x = ~ predictionThreshold, 
    y = ~ sensitivity, 
    name = 'sensitivity', 
    color = 'blue', 
    type = 'scatter', 
    mode = 'lines'
  ) %>% 
    plotly::add_trace(
      yaxis = "y2",
      y = ~ positivePredictiveValue, 
      name = 'positivePredictiveValue', 
      color = 'red', 
      mode = 'lines'
    ) %>% 
    plotly::add_trace(
      y = ~ negativePredictiveValue, 
      name = 'negativePredictiveValue', 
      color = 'green', 
      mode = 'lines'
    ) %>% 
    plotly::add_trace(
      y = ~ popfrac, 
      name = 'Fraction flagged',
      color = 'black', 
      mode = 'lines'
    ) %>% 
    plotly::layout(
      title = "Probability Threshold Plot", 
      yaxis2 = ay,
      #xaxis = list(title="Prediction Threshold"),
      #yaxis = list(title="Metric yaxis")
      plot_bgcolor='#e5ecf6',
      xaxis = list(
        title = "Prediction Threshold",
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff'
      ),
      yaxis = list(
        title = "Metric yaxis",
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff'
      ),
      shapes = list(vline(eval$predictionThreshold[pointOfInterest])),
      margin = 1,
      legend = list(
        orientation = "h",   # show entries horizontally
        xanchor = "center",  # use center of legend as anchor
        x = 0.5
        )
    )
  
  return(fig)
  
}


