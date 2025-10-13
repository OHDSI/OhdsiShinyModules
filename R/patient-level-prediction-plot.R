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
patientLevelPredictionPlotViewer <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::div(
    # table to select performance rows
    
    shiny::uiOutput(ns('plotOptions')),

    shiny::conditionalPanel(
      condition = "output.viewPlot == 1",
      ns = ns,
      
      shinydashboard::box( 
        title = shiny::actionLink(
          ns("help"), 
          "Plot", 
          icon = shiny::icon("info")
        ),
        solidHeader = TRUE,
        width = 12,
        status = 'info',
        
        shinycssloaders::withSpinner(
          shiny::plotOutput(
            outputId = ns('plot'), 
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
patientLevelPredictionPlotServer <- function(
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
      
      plotList <- c('rocPlot', 'prPlot', 'f1Plot',
                    'boxPlot','predDistPlot','prefDistPlot',
                    'demographicCalibrationPlot',
                    'smoothCalibrationPlot',
                    'netBenefitPlot'
                    )
      names(plotList) <- c('ROC Plot', 'Precision-recall Plot', 'F1-score Plot', 
                           'Prediction box-plot', 'Prediction distribution', 'Preference distribution',
                           'Demographic Calibration',
                           'Calibration',
                           'Net Benefit'
                           )
      
      output$plotOptions <- shiny::renderUI(
        
        shinydashboard::box(
          title = 'Discrimination Options',
          width = 12, 
          collapsible = TRUE,
          
          shiny::fluidRow(
            style = "background-color: #DCDCDC;", # Apply style directly to fluidRow
            
            shiny::column(
              width = 4,
              shiny::selectInput(
                inputId = session$ns('plotType'),
                label = 'Select Plot', 
                choices = plotList, 
                selected = plotList[1], 
                multiple = FALSE#, 
                #size = 
              )
            ),
            
            shiny::column(
              width = 5,
              shiny::selectInput(
                inputId = session$ns('nameCols'),
                label = 'Label Columns', 
                choices = c('performanceId','modelDesignId','evaluation',
                            'developmentDatabase','validationDatabase', 'developmentTargetName',
                            'validationTargetName', 'developmentOutcomeName', 'validationOutcomeName',
                            'developmentTimeAtRisk', 'validationTimeAtRisk'), 
                selected = c('performanceId','evaluation'), 
                multiple = TRUE#, 
                #size = 
              )
            ),
            
            shiny::column(
              width = 3,
              shiny::div(
                style = "padding-top: 25px; padding-left: 10px;",
                shiny::actionButton(
                  inputId = session$ns('generatePlot'), 
                  label = 'Generate Plot' 
                )
              )
            )
            
          ) # fluid row
        ) # box
      ) # renderUI
      
      
      shiny::observeEvent(
        input$generatePlot,
        {
          if(!is.null(performanceRowIds())){
            if(sum(performanceRowIds()) != 0){
              
              if(input$plotType %in% plotList){
                
                output$viewPlot <- shiny::reactive(1)
                
                if(input$plotType %in% c('rocPlot', 'prPlot','f1Plot','predDistPlot','prefDistPlot', 'netBenefitPlot')){
                data <- do.call('rbind', lapply(1:length(performanceRowIds()), function(i){
                  OhdsiReportGenerator::getPredictionPerformanceTable(
                    connectionHandler = connectionHandler, 
                    schema = resultDatabaseSettings$schema, 
                    plpTablePrefix = resultDatabaseSettings$plpTablePrefix, 
                    table = 'threshold_summary', 
                    performanceIds = performances()$performanceId[performanceRowIds()][i], 
                    evaluations = performances()$evaluation[performanceRowIds()][i]
                  )}))
              } else if(input$plotType %in% 'boxPlot'){
                data <- do.call('rbind', lapply(1:length(performanceRowIds()), function(i){
                  OhdsiReportGenerator::getPredictionPerformanceTable(
                    connectionHandler = connectionHandler, 
                    schema = resultDatabaseSettings$schema, 
                    plpTablePrefix = resultDatabaseSettings$plpTablePrefix, 
                    table = 'prediction_distribution', 
                    performanceIds = performances()$performanceId[performanceRowIds()][i], 
                    evaluations = performances()$evaluation[performanceRowIds()][i]
                  )}))
              } else if(input$plotType %in% c('demographicCalibrationPlot')){
                data <- do.call('rbind', lapply(1:length(performanceRowIds()), function(i){
                  OhdsiReportGenerator::getPredictionPerformanceTable(
                    connectionHandler = connectionHandler, 
                    schema = resultDatabaseSettings$schema, 
                    plpTablePrefix = resultDatabaseSettings$plpTablePrefix, 
                    table = 'demographic_summary', 
                    performanceIds = performances()$performanceId[performanceRowIds()][i], 
                    evaluations = performances()$evaluation[performanceRowIds()][i]
                  )}))
              } else if(input$plotType %in% c('smoothCalibrationPlot')){
                data <- do.call('rbind', lapply(1:length(performanceRowIds()), function(i){
                  OhdsiReportGenerator::getPredictionPerformanceTable(
                    connectionHandler = connectionHandler, 
                    schema = resultDatabaseSettings$schema, 
                    plpTablePrefix = resultDatabaseSettings$plpTablePrefix, 
                    table = 'calibration_summary', 
                    performanceIds = performances()$performanceId[performanceRowIds()][i], 
                    evaluations = performances()$evaluation[performanceRowIds()][i]
                  )}))
              }
                
                shiny::withProgress(
                  message = 'Generating plot', 
                  detail = 'This may take a while...', 
                  value = 0.2,
                  expr = {
                plotObject <-tryCatch(
                  expr = do.call(
                    what = input$plotType, 
                    args = list(
                      data = data, 
                      performance = performances()[performanceRowIds(),],
                      labelColumns = input$nameCols
                     )
                    ),
                  error = function(e){print(e);NULL}
                  )
                
                shiny::incProgress(1)
                
                  })
                
              } else{
                output$viewPlot <- shiny::reactive(0)
                plotObject <- NULL
              }
              
              output$plot <- shiny::renderPlot({
                tryCatch({plotObject}, error = function(err){print(err);NULL})
              })
              
             
            }
          } else{
            output$viewPlot <- shiny::reactive(0)
          }
        }
      )
   
     # Help code  - use input$plotType to figure this out   
      shiny::observeEvent(input$help, {
        shiny::showModal(shiny::modalDialog(
          title = "Help",
          easyClose = TRUE,
          footer = NULL,
          size = "l",
          shiny::HTML(readChar(getPredictionHelp(paste0(input$plotType,".html")), file.info(getPredictionHelp(paste0(input$plotType,".html")))$size) )
        ))
      })
      
    }
  )
}


colors25 <- function(){
  res <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)
  
  return(res)
}


# plotting
rocPlot <- function(data, performance, labelColumns = NULL){
  
  data <- addGroupId(
    data = data, 
    performance = performance, 
    labelColumns = labelColumns
    )
  
  plot <- ggplot2::ggplot(
    data = data, 
    ggplot2::aes(
      x = .data$falsePositiveRate, 
      y = .data$sensitivity, 
      color = .data$groupId, 
      group = .data$groupId
    )
  ) +
    ggplot2::geom_step(linewidth = 1) +
    ggplot2::geom_abline(
      intercept = 0, 
      slope = 1,
      linetype = 2
    ) +
    ggplot2::scale_x_continuous(
      "1 - specificity", 
      limits=c(0,1)
    ) +
    ggplot2::scale_y_continuous(
      "Sensitivity", 
      limits=c(0,1)
    ) +
    ggplot2::scale_color_discrete(name = 'Result') +
    ggplot2::scale_fill_discrete(guide = "none") + 
    ggplot2::theme(
      legend.position = "right", 
      legend.direction = "vertical"
    )
  
  return(plot)
  
}


prPlot <- function(data, performance, labelColumns = NULL){
  
  data <- addGroupId(
    data = data, 
    performance = performance, 
    labelColumns = labelColumns
  )
  
  plot <- ggplot2::ggplot(
    data = data, 
    ggplot2::aes(
      x = .data$sensitivity, 
      y = .data$positivePredictiveValue, 
      color = .data$groupId, 
      group = .data$groupId
    )
  ) +
    ggplot2::geom_step(linewidth = 1) +
    ggplot2::scale_x_continuous("Recall", limits=c(0,1)) + 
    ggplot2::scale_y_continuous("Precision", limits = c(0, NA)) + 
    ggplot2::scale_color_discrete(name = 'Result') +
    ggplot2::scale_fill_discrete(guide = "none") + 
    ggplot2::theme(
      legend.position = "right", 
      legend.direction = "vertical"
    )
  
  return(plot)
  
}



f1Plot <- function(data, performance, labelColumns = NULL){
  
  data <- addGroupId(
    data = data, 
    performance = performance, 
    labelColumns = labelColumns
  )
  
  plot <- ggplot2::ggplot(
    data = data, 
    ggplot2::aes(
      x = .data$predictionThreshold, 
      y = .data$f1Score, 
      color = .data$groupId, 
      group = .data$groupId
    )
  ) +
    ggplot2::geom_step(linewidth = 1) +
    ggplot2::scale_x_continuous("Prediction Threshold", limits=c(0,NA)) + 
    ggplot2::scale_y_continuous("F1-Score", limits = c(0, NA)) + 
    ggplot2::scale_color_discrete(name = 'Result') +
    ggplot2::scale_fill_discrete(guide = "none") + 
    ggplot2::theme(
      legend.position = "right", 
      legend.direction = "vertical"
    )
  
  return(plot)
}



predDistPlot <- function(data, performance, labelColumns = NULL){
  
  # TODO note: right now this recreates similar full data - so could create a large data.frame
  # perhaps do some sampling if the orginal data was > 100,000
  
  data <- addGroupId(
    data = data, 
    performance = performance, 
    labelColumns = labelColumns
  )
  
   x <- data %>%
     dplyr::select("groupId","predictionThreshold","truePositiveCount","falsePositiveCount", "trueCount", "falseCount") %>%
     dplyr::arrange(.data$groupId, .data$predictionThreshold, -.data$truePositiveCount, -.data$falsePositiveCount) %>%
     dplyr::group_by(.data$groupId) %>%
     dplyr::mutate(
       predictedRiskStart = .data$predictionThreshold,
       predictedRiskEnd = ifelse(is.na(dplyr::lead(.data$predictionThreshold)), .data$predictionThreshold*1.01,dplyr::lead(.data$predictionThreshold)),
       outcomes = (.data$truePositiveCount - ifelse(is.na(dplyr::lead(.data$truePositiveCount)), 0,dplyr::lead(.data$truePositiveCount))),
       nonOutcomes = (.data$falsePositiveCount - ifelse(is.na(dplyr::lead(.data$falsePositiveCount)), 0,dplyr::lead(.data$falsePositiveCount)))
     )
   

   x <- tidyr::pivot_longer(
     data = x %>%
       dplyr::select("groupId","predictedRiskStart","predictedRiskEnd","outcomes","nonOutcomes"), 
     cols = c("outcomes","nonOutcomes"), 
     names_to = 'class', 
     values_to = 'count'
   ) %>%
     tidyr::uncount(weights = .data$count)
   
   x$predictedThreshold <- unlist(lapply(1:nrow(x), function(i){
     x$predictedRiskStart[i]+stats::runif(1)*(x$predictedRiskEnd[i] - x$predictedRiskStart[i])
   }))
   
   
   plotResult <- ggplot2::ggplot(
      x, 
      ggplot2::aes(
        x = .data$predictedThreshold,
        group = .data$class,
        color = .data$class,
        fill = .data$class
        )
      ) +
     ggplot2::geom_density(
        ggplot2::aes(
          x = .data$predictedThreshold,
          group = .data$class,
          color = .data$class
          ), 
        alpha = 0.3
        ) +
      ggplot2::facet_grid(rows = 'groupId', switch = "y", scales = 'free') + 
      ggplot2::theme(
        strip.text.y.left = ggplot2::element_text(angle = 0),
        axis.text.y = ggplot2::element_blank(),  #remove y axis labels
        axis.ticks.y = ggplot2::element_blank()
        ) +
      ggplot2::scale_x_continuous("Predicted Risk")+#, limits=c(0,1)) +
      ggplot2::scale_y_continuous("Density") + 
      ggplot2::scale_fill_manual(
        values = list(
          'nonOutcomes' = 'green',
          'outcomes' = 'red'
        )
        ) + 
     ggplot2::scale_color_manual(
       values = list(
         'nonOutcomes' = 'green',
         'outcomes' = 'red'
       )
     )
      #ggplot2::guides(
      #  fill = ggplot2::guide_legend(title="Class")
      #  )
    
  return(plotResult)
}




prefDistPlot <- function(data, performance, labelColumns = NULL){
  
  # TODO note: right now this recreates similar full data - so could create a large data.frame
  # perhaps do some sampling if the orginal data was > 100,000
  
  data <- addGroupId(
    data = data, 
    performance = performance, 
    labelColumns = labelColumns
  )
  
  
  x <- data %>%
    dplyr::select("groupId","preferenceThreshold","truePositiveCount","falsePositiveCount", "trueCount", "falseCount") %>%
    dplyr::arrange(.data$groupId, .data$preferenceThreshold, -.data$truePositiveCount, -.data$falsePositiveCount) %>%
    dplyr::group_by(.data$groupId) %>%
    dplyr::mutate(
      predictedRiskStart = .data$preferenceThreshold,
      predictedRiskEnd = ifelse(is.na(dplyr::lead(.data$preferenceThreshold)), .data$preferenceThreshold+0.01,dplyr::lead(.data$preferenceThreshold)),
      outcomes = (.data$truePositiveCount - ifelse(is.na(dplyr::lead(.data$truePositiveCount)), 0,dplyr::lead(.data$truePositiveCount))),
      nonOutcomes = (.data$falsePositiveCount - ifelse(is.na(dplyr::lead(.data$falsePositiveCount)), 0,dplyr::lead(.data$falsePositiveCount)))
    )
  
  
  x <- tidyr::pivot_longer(
    data = x %>%
      dplyr::select("groupId","predictedRiskStart","predictedRiskEnd","outcomes","nonOutcomes"), 
    cols = c("outcomes","nonOutcomes"), 
    names_to = 'class', 
    values_to = 'count'
  ) %>%
    tidyr::uncount(weights = .data$count)
  
  x$predictedThreshold <- unlist(lapply(1:nrow(x), function(i){
    x$predictedRiskStart[i]+stats::runif(1)*(x$predictedRiskEnd[i] - x$predictedRiskStart[i])
  }))
  
  
  plotResult <- ggplot2::ggplot(
    x, 
    ggplot2::aes(
      x = .data$predictedThreshold,
      group = .data$class,
      color = .data$class,
      fill = .data$class
    )
  ) +
    ggplot2::geom_density(
      ggplot2::aes(
        x = .data$predictedThreshold,
        group = .data$class,
        color = .data$class
      ), 
      alpha = 0.3
    ) +
    ggplot2::facet_grid(rows = 'groupId', switch = "y", scales = 'free') + 
    ggplot2::theme(
      strip.text.y.left = ggplot2::element_text(angle = 0),
      axis.text.y = ggplot2::element_blank(),  #remove y axis labels
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::scale_x_continuous("Preference Threshold")+#, limits=c(0,1)) +
    ggplot2::scale_y_continuous("Density") + 
    ggplot2::scale_fill_manual(
      values = list(
        'nonOutcomes' = 'green',
        'outcomes' = 'red'
      )
    ) + 
    ggplot2::scale_color_manual(
      values = list(
        'nonOutcomes' = 'green',
        'outcomes' = 'red'
      )
    )
  
  return(plotResult)
}

boxPlot <- function(
    data,
    performance, 
    labelColumns = NULL
    ){
  
  data <- addGroupId(
    data = data, 
    performance = performance, 
    labelColumns = labelColumns
  )
  
    plotResult <-   ggplot2::ggplot(
      data = data, 
      ggplot2::aes(
        x = as.factor(.data$groupId),
        ymin = .data$p05PredictedProbability,
        lower = .data$p25PredictedProbability,
        middle = .data$medianPredictedProbability,
        upper = .data$p75PredictedProbability, 
        ymax = .data$p95PredictedProbability, 
        fill = sapply(.data$classLabel, FUN = function(x) if(x==0){'NoOutcome'}else{'Outcome'})
      )
    ) + 
      ggplot2::coord_flip() +
      ggplot2::geom_boxplot(stat="identity",alpha = 0.2)  +
      ggplot2::scale_x_discrete("Model Performance") + 
      ggplot2::scale_y_continuous("Predicted Probability") + 
      ggplot2::theme(
        legend.position="none",
        strip.text.y.left = ggplot2::element_text(angle = 0)
        ) +
      ggplot2::scale_fill_manual(
        values = list(
          NoOutcome = 'green4',
          Outcome = '#E31A1C'
        )
      ) + 
      ggplot2::labs(fill="Class")
      
  return(plotResult)
}



demographicCalibrationPlot <- function(
    data, 
    performance,
    labelColumns = NULL
){
  
  data <- addGroupId(
    data = data, 
    performance = performance, 
    labelColumns = labelColumns
  )
  
  # TODO add code to filter censored?
    x <- data %>%
      dplyr::select(
        'groupId',
        'ageGroup','genGroup',
        'averagePredictedProbability',
        'personCountAtRisk', 
        'personCountWithOutcome',
        'stDevPredictedProbability'
      ) %>%
      dplyr::mutate(
        observed = .data$personCountWithOutcome/.data$personCountAtRisk,
        lower = .data$averagePredictedProbability -  1.96*ifelse(is.na(.data$stDevPredictedProbability), 0, .data$stDevPredictedProbability),
        upper = .data$averagePredictedProbability +  1.96*ifelse(is.na(.data$stDevPredictedProbability), 0, .data$stDevPredictedProbability)
      ) 
    
    x <- tidyr::pivot_longer(
      data = x, 
      cols = c("averagePredictedProbability","observed"), 
      names_to = 'variable', 
      values_to = 'value'
    )

    x$age <- gsub(' ' ,'', gsub('Age group:','', x$ageGroup))
    x$age <- factor(
      x$age,
      levels = c(
        "0-4","5-9","10-14",
        "15-19","20-24","25-29",
        "30-34","35-39","40-44",
        "45-49","50-54","55-59",
        "60-64","65-69","70-74",
        "75-79","80-84","85-89",
        "90-94","95-99","-1"
      ),
      ordered = TRUE
    )
    
    plotResult <- ggplot2::ggplot(
      data = x, 
      ggplot2::aes( 
        x = .data$age, 
        group = .data$variable
      )
    ) +
      
      ggplot2::geom_line(
        ggplot2::aes(
          y = .data$value, 
          group = .data$variable,
          color = .data$variable,
          linetype = .data$variable
        )
      )+
      ggplot2::geom_ribbon(
        data=x[x$variable!='observed',],
        ggplot2::aes(
          ymin = .data$lower, 
          ymax = .data$upper
        ), 
        fill = "green4", 
        alpha = 0.2
      ) +
      ggplot2::facet_grid(
        .data$groupId ~ .data$genGroup, 
        scales = "free"
        ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
        strip.text.y.right = ggplot2::element_text(angle = 0)
        ) +
      ggplot2::scale_y_continuous("Fraction") +
      ggplot2::scale_x_discrete("Age") +
      ggplot2::scale_color_manual(
        values = c(
          averagePredictedProbability = "green4",
          observed = "#E31A1C"
          ),
        guide = ggplot2::guide_legend(title = NULL),
        labels = c("Expected", "Observed")
      ) +
      ggplot2::guides(linetype = 'none') # edit from FALSE
    
    return(plotResult)
}


smoothCalibrationPlot <- function(
    data, 
    performance,
    labelColumns = NULL
) {
  
  print('got to smoothCalibrationPlot')
  
  data <- addGroupId(
      data = data, 
      performance = performance, 
      labelColumns = labelColumns
    )
  
  limValMin <- min(
    min(data$averagePredictedProbability),
    min(data$observedIncidence)
  )
  
  limValMax <- max(
    max(data$averagePredictedProbability),
    max(data$observedIncidence)
  )
  
  smooth_plot <- ggplot2::ggplot(
    data = data, 
    ggplot2::aes(
      x = .data$averagePredictedProbability, 
      y = .data$observedIncidence, 
      color = .data$groupId,
      group = .data$groupId,
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::stat_smooth(
      linetype = 1,
      method = "loess",
      se = TRUE,
      size = 1,
      show.legend = FALSE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        xend = 1,
        y = 0,
        yend = 1
      ),
      color = "red",
      linetype = 2
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0,limValMax),
      ylim = c(0,limValMax)
    ) + 
    ggplot2::labs(
      x = "Predicted Probability", 
      y = "Observed Probability"
    )
  

  # Histogram object detailing the distibution of event/noevent for each probability interval
  
  popData1 <- data[,c('averagePredictedProbability', 'personCountWithOutcome', 'groupId')]
  popData1$label <- "Outcome" #cbind(popData1, rep("Outcome", nrow(sparsePred)))
  colnames(popData1) <- c('averagePredictedProbability','personCount','groupId',"label")
  
  popData2 <- data[,c('averagePredictedProbability', 'personCountAtRisk','groupId')]
  popData2$label <- "No Outcome" #cbind(popData2,rep("No Outcome", nrow(sparsePred)))
  popData2$personCountAtRisk <- -1*(popData2$personCountAtRisk -popData1$personCount)
  colnames(popData2) <- c('averagePredictedProbability','personCount','groupId',"label")
  
  popData <- rbind(popData1, popData2)
  popData$averagePredictedProbability <- factor(popData$averagePredictedProbability)
  
  hist_plots <- ggplot2::ggplot(
    data = popData,
    mapping = ggplot2::aes(
      x = as.double(as.character(.data$averagePredictedProbability)), 
      y = .data$personCount, 
      fill = .data$groupId,
      color = .data$groupId
    )
  )  +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_y_continuous(labels = abs) +
    ggplot2::coord_cartesian(
      ylim = c(min(popData2$personCount),max(popData1$personCount)),
      xlim = c(0,limValMax),
    ) +
    ggplot2::labs(
      x = "Predicted Probability", 
      y = "Person Count"
    )
  
  plot <- gridExtra::marrangeGrob(
    grobs = list(
      smooth_plot,
      hist_plots
      ),
    nrow = 2, 
    ncol = 1,
    heights = c(3,1)
    )
  
  return(plot)
}


netBenefitPlot <- function(
    data, 
    performance,
    labelColumns = NULL
    ){
  
  data <- addGroupId(
    data = data, 
    performance = performance, 
    labelColumns = labelColumns
  )
  
  n <- data$positiveCount + data$negativeCount
  
  data <- data %>%
    dplyr::mutate(
      model = (.data$truePositiveCount/!!n) - (.data$falsePositiveCount/!!n)*(.data$predictionThreshold/(1-.data$predictionThreshold)),
      treatAll = (.data$trueCount/!!n) - .data$falseCount/!!n*(.data$predictionThreshold/(1-.data$predictionThreshold)),
      treatNone = 0
    ) %>%
    tidyr::pivot_longer(
      cols = c("model", "treatAll", "treatNone"), 
      names_to = 'variable', 
      values_to = 'value'
        )
  
  plotResult <- ggplot2::ggplot(
    data = data, 
    ggplot2::aes(
      x = .data$predictionThreshold, 
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
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$groupId), 
      scales = 'free'
    ) +
    ggplot2::labs(
      y = "Net Benefit"
    )
  
  return(plotResult)
}


addGroupId <- function(data, performance, labelColumns = NULL){
  # merging performance to get the names for the groupId
  
  overlappingColsInd <- colnames(performance) %in% colnames(data) & !colnames(performance) %in% c('performanceId', 'evaluation')
  if( sum(overlappingColsInd) > 0 ){
    performance <- performance[,!overlappingColsInd]
  }
  
  data <- merge(data, performance, by = c('performanceId', 'evaluation'))
  
  # create labels
  if(is.null(labelColumns)){
    data$groupId <- paste0(
      'PerformanceId-',
      data$performanceId, 
      '-', 
      data$evaluation
    )
  } else{
    if(length(labelColumns) > 1){
      data$groupId <- apply(
        X = data[, labelColumns], 
        MARGIN = 1, 
        FUN = function(x) paste0(x, collapse ='-')
      )
    } else{
      data$groupId <- as.character(data[,labelColumns])
    }
  }
  
  # only take the first 50 characters
  data$groupId <- sapply(
    X = data$groupId,
    FUN = function(x){
      if(nchar(x) > 50){
        paste0(
          substr(
            x = data$groupId, 
            start = 1, 
            stop = 50
          ), 
          '...'
        )} else{
          x
        }
    })
  
  return(data)
}
