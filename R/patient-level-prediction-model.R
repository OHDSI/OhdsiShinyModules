# @file patient-level-prediction-model.R
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


#' The module viewer for exploring prediction models
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @family PatientLevelPrediction
#' @return
#' The user interface to the model module
#'
#' @export
patientLevelPredictionModelViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    style = "width: 100%; background-color: lightblue;",
    
    shiny::uiOutput(ns('modelOptions')),
    
    shiny::conditionalPanel(
      condition = "output.viewModelCoef == 1",
      ns = ns,
      
      shinydashboard::box( 
        status = 'info',
        width = 12,
        title = "Model Variable Importance", 
        solidHeader = TRUE,
        resultTableViewer(ns('variableImportance'))
      )
      
    ),
    
    shiny::conditionalPanel(
      condition = "output.viewModelSmd == 1",
      ns = ns,
      
      shinydashboard::box( 
        status = 'info',
        width = 12,
        title = "Univariate Variable Importance", 
        solidHeader = TRUE,
        resultTableViewer(ns('smd'))
      )
      
    ),
    
    shiny::conditionalPanel(
      condition = "output.viewModelDesign == 1",
      ns = ns,
      
      shinydashboard::box( 
        status = 'info',
        width = 12,
        title = "Model Design", 
        solidHeader = TRUE,
        shiny::uiOutput(ns('modelDesignInput')),
        shiny::conditionalPanel(
          condition = "output.viewsettingsView == 1",
          ns = ns,
          patientLevelPredictionSettingsViewer(ns('settingsView'))
        )
      ),
    
    ),
    
    shiny::conditionalPanel(
      condition = "output.viewModelHyperparameters == 1",
      ns = ns,
      
      shinydashboard::box( 
        status = 'info',
        width = 12,
        title = "Model Hyperparameters", 
        solidHeader = TRUE,
        shiny::div(
          shiny::uiOutput(ns('hyperparametersInput')),
          shiny::conditionalPanel(
            condition = "output.viewHyperparameterTable == 1",
            ns = ns,
            resultTableViewer(ns('hyperparameterTable'))
          )
        )
      )
      
    )
  )
    
}

#' The module server for exploring prediction models
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
#' 
#' @return
#' The server to the model module
#'
#' @export
#' 
patientLevelPredictionModelServer <- function(
  id, 
  performances,
  performanceRowIds,
  connectionHandler,
  resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$viewModelCoef <- shiny::reactive(0)
      shiny::outputOptions(output, "viewModelCoef", suspendWhenHidden = FALSE)
      output$viewModelSmd <- shiny::reactive(0)
      shiny::outputOptions(output, "viewModelSmd", suspendWhenHidden = FALSE)
      output$viewModelDesign <- shiny::reactive(0)
      shiny::outputOptions(output, "viewModelDesign", suspendWhenHidden = FALSE)
      output$viewModelHyperparameters<- shiny::reactive(0)
      shiny::outputOptions(output, "viewModelHyperparameters", suspendWhenHidden = FALSE)
      output$viewsettingsView <- shiny::reactive(0)
      shiny::outputOptions(output, "viewsettingsView", suspendWhenHidden = FALSE)
      output$viewHyperparameterTable <- shiny::reactive(0)
      shiny::outputOptions(output, "viewHyperparameterTable", suspendWhenHidden = FALSE)
      
      hyperparameterSettings <- shiny::reactiveVal(NULL)
      modelDesignSettings <- shiny::reactiveVal(NULL)
      
      output$modelOptions <- shiny::renderUI(
        shinydashboard::box(
          title = 'Model Options',
          width = 12, 
          collapsible = TRUE,
          
          shiny::fluidRow(
            style = "background-color: #DCDCDC;", # Apply style directly to fluidRow
            shiny::column(
              width = 8,
              shiny::selectInput(
                #width = '100%',
                width = "auto",
                inputId = session$ns('view'), 
                label = 'View: ', 
                choices = c('Model Variable Importance','Univariate Variable Importance', 'Model Design', 'Hyperparameters'), 
                selected = 1
              )
            ),
            
            shiny::column(
              width = 4,
              shiny::div(
                style = "padding-top: 25px; padding-left: 10px;",
                shiny::actionButton(
                  inputId = session$ns('select'), 
                  label = 'Select' 
                )
                
              )
            )
            
          )
        )
      )
      
      # hide results if models change
      shiny::observeEvent(
        eventExpr = performanceRowIds,{
          output$viewModelCoef <- shiny::reactive(0)
          output$viewModelHyperparameters <- shiny::reactive(0)
          output$viewModelDesign <- shiny::reactive(0)
          output$viewModelSmd <- shiny::reactive(0)
          output$viewsettingsView <- shiny::reactive(0)
          output$viewHyperparameterTable <- shiny::reactive(0)
        }
      )
      
      # hide results if input$view changes
      shiny::observeEvent(
        eventExpr = input$view,{
          output$viewModelCoef <- shiny::reactive(0)
          output$viewModelHyperparameters <- shiny::reactive(0)
          output$viewModelDesign <- shiny::reactive(0)
          output$viewModelSmd <- shiny::reactive(0)
          output$viewsettingsView <- shiny::reactive(0)
          output$viewHyperparameterTable <- shiny::reactive(0)
        }
      )
      
      shiny::observeEvent(input$select, {
        
        if(input$view == 'Model Variable Importance'){
          output$viewModelCoef <- shiny::reactive(1)
          output$viewModelHyperparameters <- shiny::reactive(0)
          output$viewModelDesign <- shiny::reactive(0)
          output$viewModelSmd <- shiny::reactive(0)
          
          result <- OhdsiReportGenerator::getPredictionPerformanceTable(
            connectionHandler = connectionHandler, 
            schema = resultDatabaseSettings$schema, 
            plpTablePrefix = resultDatabaseSettings$plpTablePrefix, 
            table = 'covariate_summary', 
            performanceId = unique(performances()$performanceId[performanceRowIds()])
          )
          
          result <- OhdsiReportGenerator::getPredictionCovariates(
            connectionHandler = connectionHandler, 
            schema = resultDatabaseSettings$schema, 
            plpTablePrefix = resultDatabaseSettings$plpTablePrefix, 
            cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
            databaseTable = resultDatabaseSettings$databaseTable,
            performanceIds = unique(performances()$performanceId[performanceRowIds()])
          )
          
          if( nrow(result) > 0){
            # get intercept
            interceptList <- lapply(performanceRowIds(), function(i){
              OhdsiReportGenerator::getPredictionIntercept(
                connectionHandler = connectionHandler, 
                schema = resultDatabaseSettings$schema, 
                plpTablePrefix = resultDatabaseSettings$plpTablePrefix,
                modelDesignId = performances()$modelDesignId[i],
                databaseId = performances()$developmentDatabaseId[i]
              )}
            )
            
            intercept <- data.frame(
              modelDesignId = performances()$modelDesignId[performanceRowIds()], 
              developmentDatabaseName = performances()$developmentDatabase[performanceRowIds()],
              targetName = performances()$developmentTargetName[performanceRowIds()],
              outcomeName = performances()$developmentOutcomeName[performanceRowIds()],
              timeAtRisk = performances()$developmentTimeAtRisk[performanceRowIds()],
              covariateId = 0,
              covariateName = 'intercept',
              covariateValue = unlist(interceptList)
            )
            
            # NOTE: select columns from result, rbind with interept and do this combined
            if( nrow(intercept) >0 ){
              result <- rbind(
                result %>% dplyr::select("modelDesignId", "developmentDatabaseName",
                                         "targetName","outcomeName","timeAtRisk",
                                         "covariateId", "covariateName", "covariateValue"),
                intercept
              )
            }
            
            # format:
            result <- tidyr::pivot_wider(
              data = result, 
              id_cols = c('covariateId','covariateName'), 
              names_from = c('developmentDatabaseName', 'modelDesignId','outcomeName', 'targetName', 'timeAtRisk'), 
              names_glue = '{developmentDatabaseName} Model: {modelDesignId} - predicting {outcomeName} within {targetName} during {timeAtRisk}',
              values_from = c('covariateValue'), 
              values_fn = max, 
              values_fill = 0
            )
            
            colDef <- predictionModelColumns()
            # add model columns and specify formatting
            for(name in colnames(result)[3:ncol(result)]){
              colDef[[length(colDef) + 1]] <- reactable::colDef( 
                name = name, 
                format = reactable::colFormat(digits = 2),
                style = function(value) {
                  color <- '#FAFAF5'
                  if(value < -0.2){
                    color <- '#6FECBC'
                  } else if(value > 0.2){
                    color <- '#EC846F'
                  }
                  list(background = color)
                }
              )
              names(colDef)[length(colDef)] <- name
            }
            
            modelTableOutputs <- resultTableServer(
              id = "variableImportance",
              df = result,
              colDefsInput = colDef,
              addActions = NULL,
              elementId = session$ns('variableImportanceElement')
            )
          } else{
            shiny::showNotification('No models to display feature importance')
          }
        }
        
        if(input$view == 'Univariate Variable Importance'){
          
          output$viewModelSmd <- shiny::reactive(1)
          output$viewModelCoef <- shiny::reactive(0)
          output$viewModelHyperparameters <- shiny::reactive(0)
          output$viewModelDesign <- shiny::reactive(0)
          
          result <- OhdsiReportGenerator::getPredictionCovariates(
            connectionHandler = connectionHandler, 
            schema = resultDatabaseSettings$schema, 
            plpTablePrefix = resultDatabaseSettings$plpTablePrefix,
            cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
            databaseTable = resultDatabaseSettings$databaseTable,
            performanceIds = unique(performances()$performanceId[performanceRowIds()])
          )
          
          # add validation friendly name
          result <- addValidationName(result)
          
          friendlyValidationName <- unique(result$friendlyValidationName)
          
          # format:
          result <- tidyr::pivot_wider(
            data = result, 
            id_cols = c('covariateId','covariateName'), 
            names_from = c('friendlyValidationName'), 
            names_glue = '{friendlyValidationName}:_{.value}',
            values_from = c('covariateCount','withOutcomeCovariateMean','withNoOutcomeCovariateMean','standardizedMeanDiff'), #,
            values_fn = max,
            values_fill = 0
          )
          
          # create column groups by friendlyValidationName
          colGroups <- list()
          colDefs <- list(
            covariateId = reactable::colDef(
              name = 'covariateId' 
                ),
            covariateName = reactable::colDef(
              name = 'Covariate', 
              minWidth = 150
            )
          )
          
          for(name in friendlyValidationName){
            
            colsofintId <- lapply(1:length(colnames(result)), function(i){
              if(strsplit(colnames(result)[i], ':_')[[1]][1] == name){
                return(i)
              }else{
                return(NULL)
              }
            })
            colsofintId <- unlist(colsofintId)
            
            if(length(colsofintId) > 0){
              colsofint <- colnames(result)[colsofintId]
              
              for(col in colsofint){
                colDefs[[length(colDefs) + 1]] <- reactable::colDef(
                  name = strsplit(col, ':_')[[1]][2],
                  format = reactable::colFormat(digits = 2),
                  style = function(value) {
                    color <- '#FAFAF5'
                    if(value < 0){
                      color <- '#6FECBC'
                    } else if(value > 0){
                      color <- '#EC846F'
                    }
                    list(background = color)
                  }
                )
                names(colDefs)[length(colDefs)] <- col
              }
            
              colGroups[[length(colGroups)+1]] <- reactable::colGroup(
                name = name, 
                columns = colsofint 
                  )
              
            }
          }
          
          # if no col groups set to NULL
          if(length(colGroups) == 0){
            colGroups <- NULL
          }
          
          modelTableOutputs <- resultTableServer(
            id = "smd",
            df = result,
            colDefsInput = colDefs,
            columnGroups = colGroups,
            addActions = NULL,
            elementId = session$ns('smd')
          )
        }
        
        if(input$view == 'Hyperparameters'){
          output$viewModelSmd <- shiny::reactive(0)
          output$viewModelCoef <- shiny::reactive(0)
          output$viewModelHyperparameters <- shiny::reactive(1)
          output$viewModelDesign <- shiny::reactive(0)
          
          uniqueModels <- unique(performances()[performanceRowIds(),] %>%
                                   dplyr::select(
                                     'modelDesignId',
                                     'modelType',
                                     'developmentDatabase',
                                     "developmentTargetName", 
                                     "developmentOutcomeName", 
                                     'developmentTimeAtRisk',
                                     'developmentDatabaseId'
                                   ))
          
          uniqueModels$name <- paste0('Model ', uniqueModels$modelDesignId, ' a ', uniqueModels$modelType,
                                      ' predicting ', uniqueModels$developmentOutcomeName, ' in ', 
                                      uniqueModels$developmentTargetName, ' during ', uniqueModels$developmentTimeAtRisk, 
                                      ' in ', uniqueModels$developmentDatabase )
          
          hyperparameterSettings(uniqueModels)
          
          models <- 1:nrow(uniqueModels)
          names(models) <- uniqueModels$name
          
          output$hyperparametersInput <- shiny::renderUI(
            
            shiny::fluidRow(
              style = "background-color: #DCDCDC;", # Apply style directly to fluidRow
              
              shiny::column(
                width = 10,
                shinyWidgets::pickerInput(
                  multiple = FALSE,
                  inputId = session$ns('hyperparameters'), 
                  label = 'Select Model', 
                  choices = models, 
                  selected = 1, 
                  width = '100%', 
                  options = shinyWidgets::pickerOptions(
                    liveSearch = TRUE,
                    dropupAuto = FALSE
                  )
                )
              ),
             
              shiny::column(
                width = 2,
                shiny::div(
                  style = "padding-top: 25px; padding-left: 10px;",
                  shiny::actionButton(
                    inputId = session$ns('hyperparametersGenerate'), 
                    label = 'Select' 
                  )
                )
              )
              
            )
          )
 
        }
        
        if(input$view == 'Model Design'){
          output$viewModelSmd <- shiny::reactive(0)
          output$viewModelCoef <- shiny::reactive(0)
          output$viewModelHyperparameters <- shiny::reactive(0)
          output$viewModelDesign <- shiny::reactive(1)
          
          uniqueModels <- unique(performances()[performanceRowIds(),] %>%
            dplyr::select(
              'modelDesignId',
              'modelType',
              "developmentTargetName", 
              "developmentOutcomeName", 
              'developmentTimeAtRisk'
              ))
          
          uniqueModels$name <- paste0('Model ', uniqueModels$modelDesignId, ' a ', uniqueModels$modelType,
          ' predicting ', uniqueModels$developmentOutcomeName, ' in ', 
          uniqueModels$developmentTargetName, ' during ', uniqueModels$developmentTimeAtRisk)
          
          modelDesignSettings(uniqueModels)
          
          modelDesignIds <- uniqueModels$modelDesignId
          names(modelDesignIds) <- uniqueModels$name
            
          output$modelDesignInput <- shiny::renderUI(
            
            shiny::fluidRow(
              style = "background-color: #DCDCDC;", # Apply style directly to fluidRow
              
              shiny::column(
                width = 10,
                shinyWidgets::pickerInput(
                  multiple = FALSE,
                  inputId = session$ns('modelDesign'), 
                  label = 'Select Model Design', 
                  choices = modelDesignIds, 
                  selected = 1, 
                  width = '100%', 
                  options = shinyWidgets::pickerOptions(
                    liveSearch = TRUE,
                    dropupAuto = FALSE
                  )
                )
              ),
              
              shiny::column(
                width = 2,
                shiny::div(
                  style = "padding-top: 25px; padding-left: 10px;",
                  shiny::actionButton(
                    inputId = session$ns('modelDesignGenerate'), 
                    label = 'Select' 
                  )
                )
              )
              
            )
            
          )
        }
      })
        
        
        # hyperparameter table select
        shiny::observeEvent(input$hyperparametersGenerate,{
          output$viewHyperparameterTable <- shiny::reactive(1)
      
          # add code to load the hyperparamter settings
          hyperparams <- OhdsiReportGenerator::getPredictionHyperParamSearch(
            connectionHandler = connectionHandler, 
            schema = resultDatabaseSettings$schema, 
            plpTablePrefix = resultDatabaseSettings$plpTablePrefix, 
            modelDesignId = hyperparameterSettings()$modelDesignId[as.double(input$hyperparameters)], 
            databaseId = hyperparameterSettings()$developmentDatabaseId[as.double(input$hyperparameters)]
          )
          
          # output the hyperparams as a table
          resultTableServer(
            id = "hyperparameterTable",
            df = hyperparams,
            colDefsInput = NULL,
            addActions = NULL,
            elementId = session$ns('hyperparameterTableElement')
          )
        })
      
      
      # hyperparameter table select
        singleModelDesign <- shiny::reactiveVal()
      shiny::observeEvent(input$modelDesignGenerate,{
        output$viewsettingsView <- shiny::reactive(1)
        print('model design test')
        
        modelDesignTemp <- OhdsiReportGenerator::getPredictionModelDesigns(
          connectionHandler = connectionHandler, 
          schema = resultDatabaseSettings$schema, 
          plpTablePrefix = resultDatabaseSettings$plpTablePrefix, 
          cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
          modelDesignIds = modelDesignSettings()$modelDesignId[as.double(input$modelDesign)]
        )
        singleModelDesign(modelDesignTemp)
        
      }) # END shiny observe event
      

      patientLevelPredictionSettingsServer(
        id = 'settingsView', 
        modelDesign = singleModelDesign
      )

    }
  )
}


predictionModelColumns <- function(){
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
  
  return(colDefsInput)
}



addValidationName <- function(result){
  
  if(nrow(result) > 0){
    
    result$friendlyValidationName <- paste0(
      'Predicting ', result$outcomeName, 
      ' in patients with ', result$targetName, 
      ' during ', result$timeAtRisk,
      ##  restrict setting ', # add extraction details
      ## result$plpDataSettingId, '-', result$populationSettingId,
      ' within database ', result$validationDatabaseName,
      ' performance ', result$performanceId
      # add plpRestrict and details
    )
    
  } 
  
  return(result)
}

