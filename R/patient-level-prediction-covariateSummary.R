# @file patient-level-prediction-covariateSummary.R
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


#' The module viewer for exploring prediction covariate summary results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @family PatientLevelPrediction
#' @return
#' The user interface to the covariate summary module
#'
#' @export
patientLevelPredictionCovariateSummaryViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::fluidRow(
      shinydashboard::box( 
        status = 'info',
        title = "Binary", 
        solidHeader = TRUE,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(
            ns('covariateSummaryBinary')
          )
        )
      ),
      shinydashboard::box(
        status = 'info',
        title = "Measurements", 
        solidHeader = TRUE,
        side = "right",
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(
            ns('covariateSummaryMeasure')
          )
        )
      )
    ),
    
    shiny::fluidRow(
      width=12,
      shinydashboard::box(
        status = 'info', width = 12,
        title = "Details", solidHeader = TRUE,
        shinydashboard::infoBoxOutput(ns("covariateCount"), width = 6),
        shinydashboard::infoBoxOutput(ns("nonZeroCount"), width = 6),
        shinydashboard::infoBoxOutput(ns("intercept"), width = 6),
        shinydashboard::infoBoxOutput(ns("hyperparameters"), width = 6)
      )
    ),
    
      resultTableViewer(ns('modelView'))
    
  )
}

#' The module server for exploring prediction covariate summary results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param modelDesignId unique id for the model design
#' @param developmentDatabaseId  unique id for the development database
#' @param performanceId unique id for the performance results
#' @param connectionHandler the connection to the prediction result database
#' @param inputSingleView the current tab 
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#' @family PatientLevelPrediction
#' @return
#' The server to the covariate summary module
#'
#' @export
patientLevelPredictionCovariateSummaryServer <- function(
  id, 
  modelDesignId,
  developmentDatabaseId,
  performanceId,
  connectionHandler,
  inputSingleView,
  resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      covariateSummary <- shiny::reactive({
        if(
          !is.null(performanceId()) & 
          inputSingleView() == 'Model'
          ){
          result <- loadPredictionCovSumFromDb(
            performanceId = performanceId, 
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings
          )
          
          ind <- c('covariateName','covariateValue','covariateCount','withOutcomeCovariateMean','withNoOutcomeCovariateMean','standardizedMeanDiff') %in% colnames(result) 
          result[,c('covariateName','covariateValue','covariateCount','withOutcomeCovariateMean','withNoOutcomeCovariateMean','standardizedMeanDiff')[ind]]
          
        } else{
          NULL
        }
      })
      
      intercept <- shiny::reactive({
        if(
          !is.null(modelDesignId()) & 
          !is.null(developmentDatabaseId()) & 
          inputSingleView() == 'Model'
        ){
          getPredictionIntercept(
            modelDesignId = modelDesignId,
            databaseId = developmentDatabaseId,
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings
          )
        } else{
          NULL
        }
      })
      
      hyperParamSearch <- shiny::reactive({getPredictionHyperParamSearch(
        modelDesignId = modelDesignId,
        databaseId = developmentDatabaseId,
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings
      ) })
      
      # hyper-param
      output$hyperparameters<- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          'Hyper-parameters',
          shiny::actionButton(session$ns("showHyperparameters"),"View"), 
          icon = shiny::icon('gear'),
          color = "light-blue"
        )
      })
      shiny::observeEvent(
        input$showHyperparameters, {
          
          #print('TESTING hyper-param')
          #print(hyperParamSearch())
          
          shiny::showModal(shiny::modalDialog(
            title = "Hyper-parameters",
            shiny::div(
              DT::renderDataTable(
                DT::datatable(
                  as.data.frame(
                    hyperParamSearch()
                  ),
                  options = list(scrollX = TRUE)#,
                  #colnames = 'Fold AUROC'
                )
              )
            ),
            easyClose = TRUE
          ))
        }
      )
      
      output$covariateCount <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          '# Covariates',
          nrow(covariateSummary()), 
          icon = shiny::icon('hashtag'),
          color = "light-blue"
        )
      })
        
      
      output$nonZeroCount <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          '# Non-zero covariates',
          sum(covariateSummary()$covariateValue!=0, na.rm = T), 
          icon = shiny::icon('square-full'),
          color = "light-blue"
        )
      })
        
      output$intercept <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          'Intercept',
          format(intercept(), digits =3), 
          icon = shiny::icon('a'),
          color = "light-blue"
        )
      })
      
      
      # covariate table
      colDefsInput = list(
        'covariateName' = reactable::colDef( 
          name = "Covariate Name", 
          header = withTooltip(
            "Covariate Name", 
            "The covariate name (this often includes the time period relative to index)"
          ), 
          minWidth = 200
        ),
        'covariateValue' = reactable::colDef( 
          name = "value",
          header = withTooltip(
            "value", 
            "The coeffcient for GLM or the variable importance for other models"
          )
        ),
        'covariateCount' = reactable::colDef( 
          name = "Count",
          header = withTooltip(
            "Count", 
            "The number of patients in the data who had the covariate"
          )
        ),
        'withOutcomeCovariateMean' = reactable::colDef( 
          name = "Outcome Mean",
          header = withTooltip(
            "Outcome Mean", 
            "The mean covariate value for patients who had the outcome during TAR"
          )
        ),
        'withNoOutcomeCovariateMean' = reactable::colDef( 
          name = "Non-outcome Mean", 
          header = withTooltip(
            "Non-outcome Mean", 
            "The mean covariate value for patients who did not have the outcome during TAR"
          )
        ),
        'standardizedMeanDiff' = reactable::colDef( 
          name = "Std Mean Diff",
          header = withTooltip(
            "Std Mean Diff", 
            "The standardized mean difference for the covariate comparing those who did and did not have the outcome during TAR"
          )
        )
      )
      
      modelTableOutputs <- resultTableServer(
        id = "modelView",
        df = covariateSummary,
        colDefsInput = colDefsInput,
        addActions = NULL,
        elementId = session$ns('modelView')
      )
      
      # covariate model plots
      covs <- shiny::reactiveVal(list(binary=NULL, meas = NULL))
      shiny::observe({
          covs(plotCovariateSummary(covariateSummary))
      })
      output$covariateSummaryBinary <- plotly::renderPlotly({ 
        covs()$binary 
        })
      output$covariateSummaryMeasure <- plotly::renderPlotly({ 
        covs()$meas 
        })
      
    }
  )
}


# get hyper parameters
getPredictionHyperParamSearch <- function(
    modelDesignId,
    databaseId,
    connectionHandler,
    resultDatabaseSettings
){
  
  if(!is.null(modelDesignId())){
    
    sql <- "SELECT train_details FROM @schema.@plp_table_prefixmodels WHERE database_id = @database_id
       and model_design_id = @model_design_id;"
    
    models <- connectionHandler$queryDb(
      sql = sql, 
      schema = resultDatabaseSettings$schema,
      database_id = databaseId(),
      model_design_id = modelDesignId(),
      plp_table_prefix = resultDatabaseSettings$plpTablePrefix
    )
    trainDetails <- ParallelLogger::convertJsonToSettings(models$trainDetails)
    
    return(trainDetails$hyperParamSearch)
  }
}


# format covariate summary table

plotCovariateSummary <- function(covariateSummary){
  
  if(is.null(covariateSummary())){
    return(
      list(
        binary = NULL, 
        meas = NULL
      )
    )
  }
  
  if(nrow(covariateSummary()) == 0){
    shiny::showNotification('No variables in model')
    return(
      list(
        binary = NULL, 
        meas = NULL
      )
    )
  }
  
  shiny::withProgress(message = 'Plotting covariates', value = 0, {
    
  covariateSummary <- covariateSummary()

  colnames(covariateSummary) <- gsub('_','', colnames(covariateSummary) )
  
  # remove na values 
  covariateSummary$withNoOutcomeCovariateMean[is.na(covariateSummary$withNoOutcomeCovariateMean)] <- 0
  covariateSummary$withOutcomeCovariateMean[is.na(covariateSummary$withOutcomeCovariateMean)] <- 0
  if(!'covariateValue'%in%colnames(covariateSummary)){
    covariateSummary$covariateValue <- 1
  }
  if(sum(is.na(covariateSummary$covariateValue))>0){
    covariateSummary$covariateValue[is.na(covariateSummary$covariateValue)] <- 0
  }
  
  shiny::incProgress(1/4, detail = paste("formatting data"))
  
  # SPEED EDIT remove the none model variables
  covariateSummary <- covariateSummary[covariateSummary$covariateValue!=0,]
  
  # save dots based on coef value 
  covariateSummary$size <- abs(covariateSummary$covariateValue)
  covariateSummary$size[is.na(covariateSummary$size)] <- 4
  covariateSummary$size <- 4+4*covariateSummary$size/max(covariateSummary$size)
  
  # color based on analysis id
  covariateSummary$color <- sapply(covariateSummary$covariateName, function(x) ifelse(is.na(x), '', strsplit(as.character(x), ' ')[[1]][1]))
  
  covariateSummary$times <- sapply(sapply(covariateSummary$covariateName, function(x) ifelse(is.na(x), '', strsplit(as.character(x), 'during day ')[[1]][2])),function(x) ifelse(is.na(x), '', strsplit(as.character(x), ': ')[[1]][1]))
  covariateSummary$desc <- sapply(covariateSummary$covariateName, function(x) ifelse(is.na(x), '', strsplit(as.character(x), ': ')[[1]][2]))
  
  
  l <- list(x = 0.01, y = 1,
            font = list(
              family = "sans-serif",
              size = 10,
              color = "#000"),
            bgcolor = "#E2E2E2",
            bordercolor = "#FFFFFF",
            borderwidth = 1)
  
  ind <- covariateSummary$withNoOutcomeCovariateMean <=1 & covariateSummary$withOutcomeCovariateMean <= 1
  
  shiny::incProgress(2/4, detail = paste("Generating binary plotly"))
  
  # create two plots -1 or less or g1
  binary <- plotly::plot_ly(x = covariateSummary$withNoOutcomeCovariateMean[ind],
                            #size = covariateSummary$size[ind],
                            showlegend = F) %>%
    plotly::add_markers(y = covariateSummary$withOutcomeCovariateMean[ind],
                        color=factor(covariateSummary$color[ind]),
                        hoverinfo = 'text',
                        text = ~paste('</br> Type: ', covariateSummary$color[ind],
                                      '</br> Time: ', covariateSummary$times[ind],
                                      '</br> Name: ', covariateSummary$desc[ind]),
                        showlegend = T
    ) %>%
    plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                      line = list(dash = "dash"), color = I('black'),
                      type='scatter', showlegend = FALSE) %>%
    plotly::layout(#title = 'Prevalance of baseline predictors in persons with and without outcome',
      xaxis = list(title = "Prevalance in persons without outcome",
                   range = c(0, 1)),
      yaxis = list(title = "Prevalance in persons with outcome",
                   range = c(0, 1)),
      #legend = l, showlegend = T,
      legend = list(orientation = 'h', y = -0.3), showlegend = T)
  
  shiny::incProgress(3/4, detail = paste("Generating measurement plotly"))
  
  if(sum(!ind)>0){
    maxValue <- max(c(covariateSummary$withNoOutcomeCovariateMean[!ind],
                      covariateSummary$withOutcomeCovariateMean[!ind]), na.rm = T)
    meas <- plotly::plot_ly(x = covariateSummary$withNoOutcomeCovariateMean[!ind] ) %>%
      plotly::add_markers(y = covariateSummary$withOutcomeCovariateMean[!ind],
                          hoverinfo = 'text',
                          text = ~paste('</br> Type: ', covariateSummary$color[!ind],
                                        '</br> Time: ', covariateSummary$times[!ind],
                                        '</br> Name: ', covariateSummary$desc[!ind])) %>%
      plotly::add_trace(x= c(0,maxValue), y = c(0,maxValue),mode = 'lines',
                        line = list(dash = "dash"), color = I('black'),
                        type='scatter', showlegend = FALSE) %>%
      plotly::layout(#title = 'Prevalance of baseline predictors in persons with and without outcome',
        xaxis = list(title = "Mean in persons without outcome"),
        yaxis = list(title = "Mean in persons with outcome"),
        showlegend = FALSE)
  } else {
    meas <- NULL
  }
  
  shiny::incProgress(4/4, detail = paste("Finished"))
  
  })
  
  return(
    list(
      binary=binary,
      meas = meas
      )
    )
}




# code for database covariate extract
loadPredictionCovSumFromDb <- function(
    performanceId, 
    connectionHandler, 
    resultDatabaseSettings
    ){
  ParallelLogger::logInfo("starting covsum")
  
  shiny::withProgress(message = 'Extracting covariate data', value = 0, {
    
  shiny::incProgress(2/3, detail = paste("Extracting data"))
  
  sql <- "SELECT * FROM @schema.@plp_table_prefixcovariate_summary WHERE performance_id = @performance_id;" 
  
  shiny::incProgress(2/3, detail = paste("Data extracted"))

  covariateSummary <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    performance_id = performanceId(),
    plp_table_prefix = resultDatabaseSettings$plpTablePrefix
    )
  
  # format
  for(coln in c('covariateValue','withOutcomeCovariateMean','withNoOutcomeCovariateMean','standardizedMeanDiff')){
    if(sum(colnames(covariateSummary == coln) > 0)){
      covariateSummary[,coln] <- format(round(covariateSummary[,coln], 4), nsmall = 4)
      class(covariateSummary[,coln]) <- "numeric"
    }
  }
  
  shiny::incProgress(3/3, detail = paste("Finished"))
  
  })
  
  ParallelLogger::logInfo("finishing covsum")
  return(covariateSummary)
}

getPredictionIntercept <- function(
  modelDesignId,
  databaseId,
  connectionHandler,
  resultDatabaseSettings
){
  sql <- "SELECT intercept FROM @schema.@plp_table_prefixmodels WHERE database_id = @database_id
       and model_design_id = @model_design_id"

  models <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    database_id = databaseId(),
    model_design_id = modelDesignId(),
    plp_table_prefix = resultDatabaseSettings$plpTablePrefix
  )
  
  intercept <- models$intercept
  
  if(is.null(intercept)){
    return(0)
  } 
  return(intercept)
}
