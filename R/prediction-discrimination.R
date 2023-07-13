# @file prediction-discrimination.R
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


#' The module viewer for exploring prediction model discrimination results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the model discrimination results module
#'
#' @export
predictionDiscriminationViewer <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::div(
    
    # summary table
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info', 
        width = 12,
        title = 'Summary',
        solidHeader = TRUE,
        shiny::p('Click view to see the corresponding plots:'),
        resultTableViewer(ns('summaryTable'))
      )
    ),
    
    #shiny::conditionalPanel(
     # condition = "output.generate == 1",
     # ns = ns,
      
    shiny::fluidRow(
      shinydashboard::box( 
        status = 'info',
        title = shiny::actionLink(
          ns("rocHelp"), 
          "ROC Plot", 
          icon = shiny::icon("info")
        ),
        solidHeader = TRUE,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns('roc'))
        )
      ),
      shinydashboard::box(
        status = 'info',
        title = shiny::actionLink(
          ns("prcHelp"), 
          "Precision recall plot", 
          icon = shiny::icon("info")
        ),
        solidHeader = TRUE,
        side = "right",
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns('pr'))
        )
      )
    ),
    
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info',
        title = shiny::actionLink(
          ns("f1Help"), 
          "F1 Score Plot", 
          icon = shiny::icon("info")
        ),
        solidHeader = TRUE,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns('f1'))
        )
      ),
      shinydashboard::box(
        status = 'info',
        title = shiny::actionLink(
          ns("boxHelp"),
          "Box Plot", 
          icon = shiny::icon("info")
        ),
        solidHeader = TRUE,
        side = "right",
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns('box'))
        )
      )
    ),
    
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info',
        title = shiny::actionLink(
          ns("predDistHelp"),
          "Prediction Score Distribution", 
          icon = shiny::icon("info")
        ),
        solidHeader = TRUE,
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns('preddist'))
        )
      ),
      shinydashboard::box(
        status = 'info',
        title = shiny::actionLink(
          ns("prefDistHelp"),
          "Preference Score Distribution", 
          icon = shiny::icon("info")
        ),
        solidHeader = TRUE,
        side = "right",
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns('prefdist'))
        )
      )
      
    )
    
    #) # cond panel
    
  )
}

#' The module server for exploring prediction model discrimination results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param performanceId the performance id in the database
#' @param connectionHandler the connection to the prediction result database
#' @param inputSingleView the current tab 
#' @param schema the database schema for the model results
#' @param plpTablePrefix a string that appends the tables in the result schema
#' 
#' @return
#' The server to the model discrimination module
#'
#' @export
predictionDiscriminationServer <- function(
  id, 
  performanceId, 
  connectionHandler,
  inputSingleView,
  schema, 
  plpTablePrefix
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      sumTable <- shiny::reactive({
        if(!is.null(performanceId()) & inputSingleView() == 'Discrimination'){
          print('Discrimination started')
          data <- getPredictionResult(
            performanceId = performanceId, 
            connectionHandler = connectionHandler,
            tableName = paste0(plpTablePrefix,'evaluation_statistics'), 
            schema = schema
          )
          print('Discrimination ended')
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
        ind <- data$metric %in% c('AUROC', 
                                  '95% lower AUROC',
                                  '95% upper AUROC',
                                  'AUPRC'
                                  
        )
        
        data <- tidyr::pivot_wider(
          data = data[ind,], 
          names_from = 'metric', 
          values_from = 'value'
        )
        
        cbind(
          actions = rep('', nrow(data)),
          data
          )

      })
      
      modelTableOutputs <- resultTableServer(
        id = "summaryTable",
        colDefsInput = NULL,
        df = sumTable,
        addActions = c('performance')
      )
      

      predictionDistribution <- shiny::reactiveVal(NULL)
      thresholdSummary <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(
        inputSingleView(),
        {
          
          output$roc <- NULL
          output$pr <- NULL
          output$f1 <- NULL
          output$prefdist <- NULL
          output$preddist <- NULL
          output$box <- NULL
          
          if(!is.null(performanceId()) & inputSingleView() == 'Discrimination'){
            value <- getPredictionResult(
              performanceId = performanceId, 
              connectionHandler = connectionHandler,
              tableName = paste0(plpTablePrefix,'prediction_distribution'), 
              schema = schema
            )
            predictionDistribution(value)
            
            value <- getPredictionResult(
              performanceId = performanceId, 
              connectionHandler = connectionHandler,
              tableName = paste0(plpTablePrefix,'threshold_summary'), 
              schema = schema
            )
            thresholdSummary(value)
          }
        }
      )
      
      plots <-  shiny::reactive({
        
        result <- list(roc = tryCatch({rocPlot(thresholdSummary = thresholdSummary())},
                                      error = function(cond){
                                        list(train = emptyPlot(title = 'No performanceEvaluation'))
                                      }),
                       pr = tryCatch({prPlot(thresholdSummary = thresholdSummary())},
                                     error = function(cond){
                                       list(train = emptyPlot(title = 'No performanceEvaluation'))
                                     }),
                       f1 = tryCatch({f1Plot(thresholdSummary = thresholdSummary())},
                                     error = function(cond){
                                       list(train = emptyPlot(title = 'No performanceEvaluation'))
                                     }),
                       prefpdf = tryCatch({plotPreferencePDF(thresholdSummary = thresholdSummary())},
                                          error = function(cond){
                                            NULL
                                          }),
                       predpdf = tryCatch({plotPredictedPDF(thresholdSummary = thresholdSummary())},
                                          error = function(cond){
                                            NULL
                                          }),
                       box = tryCatch({plotPredictionDistribution(predictionDistribution = predictionDistribution())},
                                      error = function(cond){
                                        NULL
                                      })
        )
        
        return(result)
      }
      )
      
      #generate <- shiny::reactiveVal(0)
      #output$generate <- generate()
      #shiny::outputOptions(output,"generate",suspendWhenHidden=FALSE)
      
      shiny::observeEvent(
        modelTableOutputs$actionCount(), {
          #generate(1)
          
          output$roc <- plotly::renderPlotly({
            type <- trimws(sumTable()$evaluation[modelTableOutputs$actionIndex()$index])
            tryCatch({plots()$roc[[type]]}, error = function(err){emptyPlot(title = err)})
          })
          
          output$pr <- plotly::renderPlotly({
            type <- trimws(sumTable()$evaluation[modelTableOutputs$actionIndex()$index])
            tryCatch({plots()$pr[[type]]}, error = function(err){emptyPlot(title = err)})
          })
          
          output$f1 <- plotly::renderPlotly({
            type <- trimws(sumTable()$evaluation[modelTableOutputs$actionIndex()$index])
            tryCatch({plots()$f1[[type]]}, error = function(err){emptyPlot(title = err)})
          })
          
          # preference plot
          output$prefdist <- shiny::renderPlot({
            type <- trimws(sumTable()$evaluation[modelTableOutputs$actionIndex()$index])
            tryCatch({plots()$prefpdf[[type]]}, error = function(err){emptyPlot(title = err)})
          })
          
          output$preddist <- shiny::renderPlot({
            type <- trimws(sumTable()$evaluation[modelTableOutputs$actionIndex()$index])
            tryCatch({plots()$predpdf[[type]]}, error = function(err){emptyPlot(title = err)})
          })
          
          output$box <- shiny::renderPlot({
            type <- trimws(sumTable()$evaluation[modelTableOutputs$actionIndex()$index])
            tryCatch({plots()$box[[type]]}, error = function(err){emptyPlot(title = err)})
          })
        }
      )
      
      shiny::observeEvent(input$rocHelp, {
        shiny::showModal(shiny::modalDialog(
          title = "ROC Help",
          easyClose = TRUE,
          footer = NULL,
          size = "l",
          shiny::HTML(readChar(getPredictionHelp("rocHelp.html"), file.info(getPredictionHelp("rocHelp.html"))$size) )
        ))
      })
      shiny::observeEvent(input$prcHelp, {
        shiny::showModal(shiny::modalDialog(
          title = "PRC Help",
          easyClose = TRUE,
          footer = NULL,
          size = "l",
          shiny::HTML(readChar(getPredictionHelp("prcHelp.html"), file.info(getPredictionHelp("prcHelp.html"))$size) )
        ))
      })
      shiny::observeEvent(input$f1Help, {
        shiny::showModal(shiny::modalDialog(
          title = "F1 Score Plot Help",
          easyClose = TRUE,
          footer = NULL,
          size = "l",
          shiny::HTML(readChar(getPredictionHelp("f1Help.html"), file.info(getPredictionHelp("f1Help.html"))$size) )
        ))
      })
      shiny::observeEvent(input$boxHelp, {
        shiny::showModal(shiny::modalDialog(
          title = "Box Plot Help",
          easyClose = TRUE,
          footer = NULL,
          size = "l",
          shiny::HTML(readChar(getPredictionHelp("boxHelp.html"), file.info(getPredictionHelp("boxHelp.html"))$size) )
        ))
      })
      shiny::observeEvent(input$predDistHelp, {
        shiny::showModal(shiny::modalDialog(
          title = "Predicted Risk Distribution Help",
          easyClose = TRUE,
          footer = NULL,
          size = "l",
          shiny::HTML(readChar(getPredictionHelp("predDistHelp.html"), file.info(getPredictionHelp("predDistHelp.html"))$size) )
        ))
      })
      shiny::observeEvent(input$prefDistHelp, {
        shiny::showModal(shiny::modalDialog(
          title = "Preference Score Distribution Help",
          easyClose = TRUE,
          footer = NULL,
          size = "l",
          shiny::HTML(readChar(getPredictionHelp("prefDistHelp.html"), file.info(getPredictionHelp("prefDistHelp.html"))$size) )
        ))
      })
      
    }
  )
}




# pltting
rocPlot <- function(thresholdSummary, type){
  
  types <- unique(thresholdSummary$evaluation)
  rocobject <- list()
  length(rocobject) <- length(types)
  names(rocobject) <- types
  
  shiny::withProgress(message = 'Plotting AUROCs', value = 0, {
    
    i <- 0
    for(type in types){
      i <- i + 1
      shiny::incProgress(i/length(types), detail = paste("Plotting for type ", type))
      
    data <- thresholdSummary[thresholdSummary$evaluation%in%type,]
    
    rocobject[[type]] <- plotly::plot_ly(x = 1-c(0,data$specificity,1)) %>%
      plotly::add_lines(y = c(1,data$sensitivity,0),name = "hv", 
                        text = paste('Risk Threshold:',c(0,data$predictionThreshold,1)),
                        line = list(shape = "hv",
                                    color = 'rgb(22, 96, 167)'),
                        fill = 'tozeroy') %>%
      plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                        line = list(dash = "dash"), color = I('black'),
                        type='scatter') %>%
      plotly::layout(title = "ROC Plot",
                     xaxis = list(title = "1-specificity"),
                     yaxis = list (title = "Sensitivity"),
                     showlegend = FALSE)
    }
  })
  
  return(rocobject)
}

prPlot <- function(thresholdSummary, type){
  types <- unique(thresholdSummary$evaluation)
  probject <- list()
  length(probject) <- length(types)
  names(probject) <- types
  
  shiny::withProgress(message = 'Plotting AUPRCs', value = 0, {
    
    i <- 0
    for(type in types){
      i <- i + 1
      shiny::incProgress(i/length(types), detail = paste("Plotting for type ", type))
      
    data <- thresholdSummary[thresholdSummary$evaluation%in%type,]
    
    popAv <- data$trueCount[1]/(data$trueCount[1] + data$falseCount[1])
    probject[[type]]  <- plotly::plot_ly(x = data$sensitivity) %>%
      plotly::add_lines(y = data$positivePredictiveValue, name = "hv", 
                        text = paste('Risk Threshold:',data$predictionThreshold),
                        line = list(shape = "hv",
                                    color = 'rgb(22, 96, 167)'),
                        fill = 'tozeroy') %>%
      plotly::add_trace(x= c(0,1), y = c(popAv,popAv),mode = 'lines',
                        line = list(dash = "dash"), color = I('red'),
                        type='scatter') %>%
      plotly::layout(title = "PR Plot",
                     xaxis = list(title = "Recall"),
                     yaxis = list (title = "Precision"),
                     showlegend = FALSE)
    
    }
    
  })
  
  return(probject)
}

f1Plot <- function(thresholdSummary, type){
  types <- unique(thresholdSummary$evaluation)
  f1object <- list()
  length(f1object) <- length(types)
  names(f1object) <- types
  
  shiny::withProgress(message = 'Plotting f1s', value = 0, {
    
    i <- 0
    for(type in types){
      i <- i + 1
      shiny::incProgress(i/length(types), detail = paste("Plotting for type ", type))
      
    data <- thresholdSummary[thresholdSummary$evaluation%in%type,]
    
    f1object[[type]]  <- plotly::plot_ly(x = data$predictionThreshold) %>%
      plotly::add_lines(y = data$f1Score, name = "hv", 
                        text = paste('Risk Threshold:',data$predictionThreshold),
                        line = list(shape = "hv",
                                    color = 'rgb(22, 96, 167)'),
                        fill = 'tozeroy') %>%
      plotly::layout(title = "F1-Score Plot",
                     xaxis = list(title = "Prediction Threshold"),
                     yaxis = list (title = "F1-Score"),
                     showlegend = FALSE)
    
    }
    
  })
  return(f1object)
}






# adding plots from PLP temporarily as shiny deploy doesnt have PatientLevelPrediction

plotPredictedPDF <- function(thresholdSummary, fileName=NULL){
  
  if(!is.null(thresholdSummary$evaluation)){
    types <- unique(thresholdSummary$evaluation)
  } else{
    thresholdSummary$evaluation <- 'na'
    types <- 'na'
  }
  
  plotResult <- list()
  length(plotResult) <- length(types)
  names(plotResult) <- types
  
  shiny::withProgress(message = 'Plotting predictedPDF', value = 0, {
    
    i <- 0
    for(type in types){
      i <- i + 1
      shiny::incProgress(i/length(types), detail = paste("Plotting for type ", type))
      
    ind <- 1:nrow(thresholdSummary)
    if(!is.null(thresholdSummary$evaluation)){
      ind <- thresholdSummary$evaluation == type
    }
    
    
    x<- thresholdSummary[ind,c('predictionThreshold','truePositiveCount','trueNegativeCount',
                               'falsePositiveCount','falseNegativeCount')]
    x<- x[order(x$predictionThreshold,-x$truePositiveCount, -x$falsePositiveCount),]
    x$out <- c(x$truePositiveCount[-length(x$truePositiveCount)]-x$truePositiveCount[-1], x$truePositiveCount[length(x$truePositiveCount)])
    x$nout <- c(x$falsePositiveCount[-length(x$falsePositiveCount)]-x$falsePositiveCount[-1], x$falsePositiveCount[length(x$falsePositiveCount)])
    
    vals <- c()
    for(i in 1:length(x$predictionThreshold)){
      if(i!=length(x$predictionThreshold)){
        upper <- x$predictionThreshold[i+1]} else {upper <- min(x$predictionThreshold[i]+0.01,1)}
      val <- x$predictionThreshold[i]+ stats::runif(x$out[i])*(upper-x$predictionThreshold[i])
      vals <- c(val, vals)
    }
    vals[!is.na(vals)]
    
    vals2 <- c()
    for(i in 1:length(x$predictionThreshold)){
      if(i!=length(x$predictionThreshold)){
        upper <- x$predictionThreshold[i+1]} else {upper <- min(x$predictionThreshold[i]+0.01,1)}
      val2 <- x$predictionThreshold[i]+ stats::runif(x$nout[i])*(upper-x$predictionThreshold[i])
      vals2 <- c(val2, vals2)
    }
    vals2[!is.na(vals2)]
    
    x <- rbind(data.frame(variable=rep('outcome',length(vals)), value=vals),
               data.frame(variable=rep('No outcome',length(vals2)), value=vals2)
    )
    
    plot <- ggplot2::ggplot(
      x, 
      ggplot2::aes(
        x = .data$value,
        group = .data$variable,
        fill = .data$variable
        )
      ) +
      ggplot2::geom_density(
        ggplot2::aes(
          x = .data$value, 
          fill = .data$variable
          ), 
        alpha=.3
        ) +
      ggplot2::scale_x_continuous("Prediction Threshold")+#, limits=c(0,1)) +
      ggplot2::scale_y_continuous("Density") + 
      ggplot2::guides(
        fill = ggplot2::guide_legend(title="Class")
        )
    
    plotResult[[type]] <- plot
  }
  
})
  
  return(plotResult)
}




plotPreferencePDF <- function(thresholdSummary, fileName=NULL){
  
  if(!is.null(thresholdSummary$evaluation)){
    types <- unique(thresholdSummary$evaluation)
  } else{
    thresholdSummary$evaluation <- 'na'
    types <- 'na'
  }
  
  plotResult <- list()
  length(plotResult) <- length(types)
  names(plotResult) <- types
  
  shiny::withProgress(message = 'Plotting preferencePDF', value = 0, {
    
    i <- 0
  for(type in types){
    i <- i + 1
    shiny::incProgress(i/length(types), detail = paste("Plotting for type ", type))
    
    
    ind <- 1:nrow(thresholdSummary)
    if(!is.null(thresholdSummary$evaluation)){
      ind <- thresholdSummary$evaluation == type
    }
    
    x <- thresholdSummary[ind,c('preferenceThreshold','truePositiveCount','trueNegativeCount',
                                'falsePositiveCount','falseNegativeCount')]
    x <- x[order(x$preferenceThreshold,-x$truePositiveCount, x$trueNegativeCount),]
    x$out <- c(x$truePositiveCount[-length(x$truePositiveCount)]-x$truePositiveCount[-1], x$truePositiveCount[length(x$truePositiveCount)])
    x$nout <- c(x$falsePositiveCount[-length(x$falsePositiveCount)]-x$falsePositiveCount[-1], x$falsePositiveCount[length(x$falsePositiveCount)])
    
    vals <- c()
    for(i in 1:length(x$preferenceThreshold)){
      if(i!=length(x$preferenceThreshold)){
        upper <- x$preferenceThreshold[i+1]} else {upper <- 1}
      val <- x$preferenceThreshold[i]+ stats::runif(x$out[i])*(upper-x$preferenceThreshold[i])
      vals <- c(val, vals)
    }
    vals[!is.na(vals)]
    
    vals2 <- c()
    for(i in 1:length(x$preferenceThreshold)){
      if(i!=length(x$preferenceThreshold)){
        upper <- x$preferenceThreshold[i+1]} else {upper <- 1}
      val2 <- x$preferenceThreshold[i]+ stats::runif(x$nout[i])*(upper-x$preferenceThreshold[i])
      vals2 <- c(val2, vals2)
    }
    vals2[!is.na(vals2)]
    
    x <- rbind(data.frame(variable=rep('outcome',length(vals)), value=vals),
               data.frame(variable=rep('No outcome',length(vals2)), value=vals2)
    )
    
    plot <- ggplot2::ggplot(
      data = x, 
      ggplot2::aes(
        x = .data$value,
        group = .data$variable,
        fill = .data$variable
        )
      ) +
      ggplot2::geom_density(
        ggplot2::aes(
          x = .data$value, 
          fill = .data$variable
          ), 
        alpha=.3
        ) +
      ggplot2::scale_x_continuous("Preference Threshold")+#, limits=c(0,1)) +
      ggplot2::scale_y_continuous("Density") + 
      ggplot2::guides(
        fill = ggplot2::guide_legend(title="Class")
        )
    
    plotResult[[type]] <- plot
    
  }
    
  })
  
  return(plotResult)
}

plotPredictionDistribution <- function(predictionDistribution){
  
  if(!is.null(predictionDistribution$evaluation)){
    types <- unique(predictionDistribution$evaluation)
  } else{
    predictionDistribution$evaluation <- 'na'
    types <- 'na'
  }
  
  shiny::withProgress(message = 'Plotting distributions', value = 0, {
    
  
  plotResult <- list()
  length(plotResult) <- length(types)
  names(plotResult) <- types
  
  i <- 0
  for(type in types){
    i <- i + 1
    shiny::incProgress(i/length(types), detail = paste("Plotting for type ", type))
    
    ind <- 1:nrow(predictionDistribution)
    if(!is.null(predictionDistribution$evaluation)){
      ind <- predictionDistribution$evaluation == type
    }
    x<- predictionDistribution[ind,]
    
    non05 <- x$p05PredictedProbability[x$classLabel==0]
    non95 <- x$p95PredictedProbability[x$classLabel==0]
    one05 <- x$p05PredictedProbability[x$classLabel==1]
    one95 <- x$p95PredictedProbability[x$classLabel==1]
    
    plot <-   ggplot2::ggplot(
      data = x, 
      ggplot2::aes(
        x = as.factor(.data$classLabel),
        ymin = .data$minPredictedProbability,
        lower = .data$p25PredictedProbability,
        middle = .data$medianPredictedProbability,
        upper = .data$p75PredictedProbability, 
        ymax = .data$maxPredictedProbability, 
        color = as.factor(.data$classLabel)
      )
    ) + 
      ggplot2::coord_flip() +
      ggplot2::geom_boxplot(stat="identity")  +
      ggplot2::scale_x_discrete("Class") + 
      ggplot2::scale_y_continuous("Predicted Probability") + 
      ggplot2::theme(legend.position="none") +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = 0.9, 
          y = non05, 
          xend = 1.1, 
          yend = non05
          ), 
        color='red'
        ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = 0.9, 
          y = non95, 
          xend = 1.1, 
          yend = non95
          ), 
        color='red'
        ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = 1.9, 
          y = one05, 
          xend = 2.1, 
          yend = one05
          )
        ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = 1.9, 
          y = one95, 
          xend = 2.1, 
          yend = one95
          )
        )
    
    plotResult[[type]] <- plot
    
  }
  
  })
  
  return(plotResult)
}

