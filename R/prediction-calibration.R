# @file prediction-calibration.R
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


#' The module viewer for exploring prediction model calibration results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the prediction model calibration module
#'
#' @export
predictionCalibrationViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info', width = 12,
        title = 'Summary',
        solidHeader = TRUE,
        shiny::p('Click view to see corresponding plots:'),
        reactable::reactableOutput(ns('calTable')
        )
      )
    ),
    
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info',
        title = shiny::actionLink(
          ns("calHelp"),
          "Calibration Plot", 
          icon = shiny::icon("info")
        ),
        solidHeader = TRUE,
        shinycssloaders::withSpinner(shiny::plotOutput(ns('cal')))
      ),
      shinydashboard::box(
        status = 'info',
        title = shiny::actionLink(
          ns("demoHelp"),
          "Demographic Plot", 
          icon = shiny::icon("info")
        ),
        solidHeader = TRUE,
        side = "right",
        shinycssloaders::withSpinner(shiny::plotOutput(ns('demo')))
      )
    )
  )
}


#' The module server for exploring prediction validation results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param performanceId the performance id in the database
#' @param con the connection to the prediction result database
#' @param inputSingleView the current tab 
#' @param mySchema the database schema for the model results
#' @param targetDialect the database management system for the model results
#' @param myTableAppend a string that appends the tables in the result schema
#' 
#' @return
#' The server to the prediction calibration module
#'
#' @export
predictionCalibrationServer <- function(
  id, 
  performanceId,
  con,
  inputSingleView,
  mySchema, 
  targetDialect,
  myTableAppend
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      sumTable <- shiny::reactive({
        if(!is.null(performanceId()) & inputSingleView() == 'Calibration'){
          print('Extracting calibration')
          data <- getPredictionResult(
            performanceId = performanceId, 
            con = con,
            tableName = paste0(myTableAppend,'evaluation_statistics'), 
            mySchema = mySchema, 
            targetDialect = targetDialect 
          )
        } else{
          data <- NULL
        }
        
        if(is.null(data)){
          return(NULL)
        }
        
        for(i in 1:ncol(data)){
          data[,i] <- unlist(data[,i])
        }
        
        data$value <- as.double(as.character(data$value))
        data$value <- format(data$value, digits = 4, scientific = F)
        ind <- data$metric %in% c(
          'calibrationInLarge intercept', 
          'weak calibration intercept',
          'weak calibration gradient',
          'calibrationInLarge mean prediction',
          'calibrationInLarge observed risk',
          'ici',
          'Emean',
          'E90',
          'Emax', 
          'correctionFactor',
          'adjustGradient',
          'adjustIntercept'
        )
        
        tidyr::pivot_wider(
          data[ind,],
          names_from = 'metric', 
          values_from = 'value'
        )
        
      })
      
      output$calTable <- reactable::renderReactable({
        reactable::reactable(
          data =  
            if(is.null(sumTable())){
              NULL
            } else{
              cbind(
                view = rep("",nrow( sumTable())),
                sumTable()
              )
            },
          columns = 
            list(
              view = reactable::colDef(
                name = "",
                sortable = FALSE,
                cell = function() htmltools::tags$button("View")
              )
            ),onClick = reactable::JS(paste0("function(rowInfo, column) {
    // Only handle click events on the 'details' column
    if (column.id !== 'view' ) {
      return
    }

    // Send the click event to Shiny, which will be available in input$show_details
    // Note that the row index starts at 0 in JavaScript, so we add 1
    if (window.Shiny) {
    if(column.id == 'view'){
      Shiny.setInputValue('",session$ns('show_view'),"', { index: rowInfo.index + 1 }, { priority: 'event' })
    }
    }
  }")
            ),
          filterable = TRUE
        )
      })
      
      calibrationSummary <- shiny::reactiveVal(NULL)
      demographicSummary <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(
        inputSingleView(),
        {
          
          output$cal <- NULL
          output$demo <- NULL
          
          if(!is.null(performanceId()) & inputSingleView() == 'Calibration'){
            
            value <- getPredictionResult(
              performanceId = performanceId,
              con = con,
              tableName = paste0(myTableAppend,'calibration_summary'), 
              mySchema = mySchema, 
              targetDialect = targetDialect 
            )
            calibrationSummary(value)
            
            value <- getPredictionResult(
              performanceId = performanceId,
              con = con,
              tableName = paste0(myTableAppend,'demographic_summary'), 
              mySchema = mySchema, 
              targetDialect = targetDialect 
            )
            demographicSummary(value)
          }
        })
      
      shiny::observeEvent(
        input$show_view, {
          
          output$cal <- shiny::renderPlot({
            type <- trimws(sumTable()$evaluation[input$show_view$index])
            tryCatch(
              {plotSparseCalibration2(
                calibrationSummary = calibrationSummary(), 
                type =  type
              )
              },
              error = function(err){emptyPlot(title = err)}
            )
          })
          
          output$demo <- shiny::renderPlot({
            type <- trimws(sumTable()$evaluation[input$show_view$index])
            tryCatch(
              plotDemographicSummary(
                demographicSummary = demographicSummary(), 
                type = type
              ),
              error= function(cond){return(NULL)}
            )
          })
          
        })
      
      
      shiny::observeEvent(input$calHelp, {
        shiny::showModal(shiny::modalDialog(
          title = "Calibration Help",
          easyClose = TRUE,
          footer = NULL,
          size = "l", 
          shiny::HTML(readChar(getPredictionHelp("calHelp.html"), file.info(getPredictionHelp("calHelp.html"))$size) )
        ))
      })
      shiny::observeEvent(input$demoHelp, {
        shiny::showModal(shiny::modalDialog(
          title = "Demographic Help",
          easyClose = TRUE,
          footer = NULL,
          size = "l",
          shiny::HTML(readChar(getPredictionHelp("demoHelp.html"), file.info(getPredictionHelp("demoHelp.html"))$size) )
        ))
      })
      
      
    }
  )
}



plotDemographicSummary <- function(
  demographicSummary, 
  type = NULL
){
  if (!all(is.na(demographicSummary$averagePredictedProbability))){
    # remove spaces
    demographicSummary$evaluation <- gsub(' ', '', demographicSummary$evaluation)
    
    ind <- 1:nrow(demographicSummary)
    if(is.null(type)){
      if(!is.null(demographicSummary$evaluation)){
        ind <- demographicSummary$evaluation%in%c('Test','validation')
      }
    } else{
      ind <- demographicSummary$evaluation == type
    }
    
    x <- demographicSummary[
      ind,
      colnames(demographicSummary) %in% c(
        'ageGroup','genGroup','averagePredictedProbability',
        'personCountAtRisk', 'personCountWithOutcome'
      )
    ]
    
    
    # remove -1 values:
    x$averagePredictedProbability[is.na(x$averagePredictedProbability)] <- 0
    x <- x[x$personCountWithOutcome != -1,]
    if(nrow(x)==0){
      return(NULL)
    }
    
    x$observed <- x$personCountWithOutcome/x$personCountAtRisk
    
    
    x <- x[, 
           colnames(x) %in% 
             c('ageGroup','genGroup',
               'averagePredictedProbability','observed'
             )
    ]
    
    
    x <- tidyr::pivot_longer(
      data = x, 
      cols = !colnames(x)[colnames(x) %in% c('ageGroup','genGroup')], 
      names_to = 'variable', 
      values_to = 'value'
    )
    #x <- reshape2::melt(x, id.vars=c('ageGroup','genGroup'))
    
    # 1.96*StDevPredictedProbability
    ci <- demographicSummary[ind,colnames(demographicSummary)%in%c('ageGroup','genGroup','averagePredictedProbability','stDevPredictedProbability')]
    ci$stDevPredictedProbability[is.na(ci$stDevPredictedProbability)] <- 1
    ci$lower <- ci$averagePredictedProbability-1.96*ci$stDevPredictedProbability
    ci$lower[ci$lower <0] <- 0
    ci$upper <- ci$averagePredictedProbability+1.96*ci$stDevPredictedProbability
    ci$upper[ci$upper >1] <- max(ci$upper[ci$upper <1])
    
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
    
    
    x <- merge(
      x, 
      ci[,c('ageGroup','genGroup','lower','upper')], 
      by = c('ageGroup','genGroup')
    )
    x <- x[!is.na(x$value),]
    
    plot <- ggplot2::ggplot(
      data=x, 
      ggplot2::aes( 
        x = .data$age, 
        group = interaction(
          .data$variable, 
          .data$genGroup
        )
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
          ymax = .data$upper, 
          group = .data$genGroup
        ), 
        fill = "blue", 
        alpha = 0.2
      ) +
      ggplot2::facet_grid(.~ genGroup, scales = "free") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      #ggplot2::coord_flip() +
      ggplot2::scale_y_continuous("Fraction") +
      ggplot2::scale_x_discrete("Age") +
      ggplot2::scale_color_manual(
        values = c("royalblue4","red"),
        guide = ggplot2::guide_legend(title = NULL),
        labels = c("Expected", "Observed")
        ) +
      ggplot2::guides(linetype = FALSE)
    
    return(plot)
  }
}

plotSparseCalibration2 <- function(
  calibrationSummary,
  smooth = "loess",
  span = 1,
  nKnots = 5,
  scatter = T,
  bins = 20,
  zoom =  "data",
  sample = T,
  type = NULL
) {
  
  ind <- 1:nrow(calibrationSummary)
  
  if(is.null(type)){
    if(!is.null(calibrationSummary$evaluation)){
      ind <- calibrationSummary$evaluation %in% c('Test','validation')
    }
  } else{
    ind <- calibrationSummary$evaluation == type
  }
  
  if(sum(ind) == 0){
    return(NULL)
  }
  
  sparsePred <- calibrationSummary[ind,]
  
  limVal <- max(
    max(sparsePred$averagePredictedProbability),
    max(sparsePred$observedIncidence)
  )
  
  smooth_plot <- ggplot2::ggplot(
    data = sparsePred, 
    ggplot2::aes(
      x = .data$averagePredictedProbability, 
      y = .data$observedIncidence
      )
    ) +
    ggplot2::stat_smooth(
      ggplot2::aes(
        color = "Loess", 
        linetype = "Loess"
      ),
      method = "loess",
      se = TRUE,
      #span = span,
      size = 1,
      show.legend = F
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        xend = 1,
        y = 0,
        yend = 1,
        color = "Ideal",
        linetype = "Ideal"
        )
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0,limVal),
      ylim = c(0,limVal)
    ) + 
    ggplot2::scale_linetype_manual(
      name = "Models",
      values = c(
        Loess = "solid",
        Ideal = "dashed"
        )
      ) + 
    ggplot2::scale_color_manual(
      name = "Models", 
      values = c(
        Loess = "blue", 
        Ideal = "red")
      ) + 
    ggplot2::labs(
      x = "Predicted Probability", 
      y = "Observed Probability"
      )
  
  # construct the plot grid
  if (scatter) {
    smooth_plot <- smooth_plot + 
      ggplot2::geom_point(
        data = sparsePred,
        ggplot2::aes(
          x = .data$averagePredictedProbability,
          y = .data$observedIncidence
        ),
        color = "black",
        size = 2
      )
  }
  
  # Histogram object detailing the distibution of event/noevent for each probability interval
  
  popData1 <- sparsePred[,c('averagePredictedProbability', 'personCountWithOutcome')]
  popData1$label <- "Outcome" #cbind(popData1, rep("Outcome", nrow(sparsePred)))
  colnames(popData1) <- c('averagePredictedProbability','personCount',"label")
  
  popData2 <- sparsePred[,c('averagePredictedProbability', 'personCountAtRisk')]
  popData2$label <- "No Outcome" #cbind(popData2,rep("No Outcome", nrow(sparsePred)))
  popData2$personCountAtRisk <- -1*(popData2$personCountAtRisk -popData1$personCount)
  colnames(popData2) <- c('averagePredictedProbability','personCount',"label")
  
  popData <- rbind(popData1, popData2)
  popData$averagePredictedProbability <- factor(popData$averagePredictedProbability)
  
  hist_plot <- ggplot2::ggplot(
    data = popData, 
    ggplot2::aes(
      y = .data$averagePredictedProbability, 
      x = .data$personCount, 
      fill = .data$label
    )
  ) + 
    ggplot2::geom_bar(
      data = popData[popData$label == "Outcome",], 
      stat = "identity"
    ) + 
    ggplot2::geom_bar(
      data = popData[popData$label == "No Outcome",],
      stat = "identity"
      ) + 
    ggplot2::geom_bar(stat = "identity") + 
    ggplot2::scale_x_continuous(labels = abs) + 
    ggplot2::coord_flip( ) +
    ggplot2::theme_bw() + 
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  
  # testting whether this is installed in shinydeploy
  plot <- gridExtra::grid.arrange(
    smooth_plot,
    hist_plot,
    ncol = 1,
    heights=c(2,1)
  )
  
  return(plot)
}
