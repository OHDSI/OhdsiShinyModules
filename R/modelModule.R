# @file modelModule.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
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

#' The user interface for specifying the PatientLevelPrediction model
#' @param id   The module identifier
#' @param labelv  A string corresponding to the label for the form box
#'
#' @examples
#' \dontrun{
#' modelViewer('model1', labelv = "Model")
#' }
#' @export
modelViewer <- function(id, labelv = "Model") {
  ns <- shiny::NS(id)

  shinydashboard::box(title = labelv, width = '60%',

                      # add table with existing models

                      shiny::dataTableOutput(ns('modelTable')),

                      # add a model
                      shiny::selectInput(inputId = ns('modelSelect'), label = 'Model:',
                                         choices = c('LassoLogisticRegression', 'GradientBoostingMachine', 'RandomForest', 'AdaBoost', 'DecisionTree', 'CoxModel'),
                                         multiple = F),
                      shiny::actionButton(inputId = ns('modelAdd'),
                                          label = 'Add Model')

  )
}


#' The server for defining PatientLevelPrediction model settings
#' @param input   Standard for shiny modules
#' @param output  Standard for shiny modules
#' @param session Standard for shiny modules
#'
#' @examples
#' \dontrun{
#' modelList <- callModule(modelServer, 'model1')
#' }
#' @export
modelServer <- #function(id) {
  #moduleServer(
  #id,
  function(input, output, session) {

    # create the list of population settings
    modelList <- shiny::reactiveVal(list())
    # create an variable to show which index is being updated
    selectedRow <- shiny::reactiveVal()

    shiny::observeEvent(input$modelAdd, {

      # add a new model to the list
      oldList <- modelList()
      i <- length(oldList)
      oldList[[i+1]] <- list(name = input$modelSelect,
                             settings = list())
      modelList(oldList)

      # set the row selector
      selectedRow(i+1)

      # now show editor
      type <- getType(input$modelSelect)
      shiny::showModal(do.call(paste0('modelModal',type),
                        list(ns = session$ns, model = list())))

    })

    # create function for edit/delete buttoms in table:
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      inputs
    }

    # display the covariates
    output$modelTable <- shiny::renderDataTable({
      if(length(modelList())>0){
        data.frame(Model = unlist(lapply(modelList(), function(x) x$name)),
                   Setting = getSetting(modelList()),
                   Edit= shinyInput(shiny::actionButton, length(modelList()), 'pbutton_', label = "Edit", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("select_button"))  ),
                   stringsAsFactors = FALSE,
                   row.names = 1:length(modelList()),
                   Remove = shinyInput(shiny::actionButton, length(modelList()), 'pbutton_', label = "Remove", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("delete_button"))  )

        )} else{
          NULL
        }
    }, escape = FALSE)

    # UPDATE POP
    #=================
    # Show modal when button is clicked.
    shiny::observeEvent(input$select_button, {
      selectedRow(as.numeric(strsplit(input$select_button, "_")[[1]][2]))
      oldList <- modelList()[[selectedRow()]]

      type <- getType(oldList$name)
      shiny::showModal(do.call(paste0('modelModal',type), list(ns = session$ns, model = oldList)))

    })
    # When button is pressed, update covariate
    # update the covariate with new settings
    shiny::observeEvent(input$addLR, {
      indVal <- selectedRow()
      oldList <- modelList()
      oldList[[indVal]] <- list( name = 'LassoLogisticRegression',
                                 settings = list(variance = input$LRvariance,
                                                 threads = input$LRthreads,
                                                 seed = input$LRseed)
      )
      modelList(oldList)
      shiny::removeModal()
    })

    shiny::observeEvent(input$addGBM, {
      indVal <-selectedRow()
      oldList <- modelList()
      oldList[[indVal]] <- list( name = 'GradientBoostingMachine',
                                 settings = list(ntrees = input$GBMntrees,
                                                 nthread = input$GBMnthread,
                                                 earlyStopRound = input$GBMearlyStopRound,
                                                 maxDepth = input$GBMmaxDepth,
                                                 minRows = input$GBMminRows,
                                                 learnRate = input$GBMlearnRate,
                                                 seed = input$GBMseed
                                 ))
      modelList(oldList)
      shiny::removeModal()
    })

    shiny::observeEvent(input$addRF, {
      indVal <-selectedRow()
      oldList <- modelList()
      oldList[[indVal]] <- list( name = 'RandomForest',
                                 settings = list(mtries = input$RFmtries,
                                                 ntrees = input$RFntrees,
                                                 maxDepth = input$RFmaxDepth,
                                                 varImp = input$RFvarImp,
                                                 seed = input$RFseed
                                 ))
      modelList(oldList)
      shiny::removeModal()
    })

    shiny::observeEvent(input$addAB, {
      indVal <-selectedRow()
      oldList <- modelList()
      oldList[[indVal]] <- list( name = 'AdaBoost',
                                 settings = list(nEstimators = input$ABnEstimators,
                                                 learningRate= input$ABlearningRate,
                                                 seed = input$ABseed
                                 ))
      modelList(oldList)
      shiny::removeModal()
    })

    shiny::observeEvent(input$addDT, {
      indVal <-selectedRow()
      oldList <- modelList()
      oldList[[indVal]] <- list( name = 'DecisionTree',
                                 settings = list(maxDepth = input$DTmaxDepth,
                                                 minSamplesSplit = input$DTminSamplesSplit,
                                                 minSamplesLeaf = input$DTminSamplesLeaf,
                                                 minImpurityDecrease = input$DTminImpurityDecrease,
                                                 classWeight = input$DTclassWeight,
                                                 seed = input$DTseed
                                 ))
      modelList(oldList)
      shiny::removeModal()
    })

    shiny::observeEvent(input$addCM, {
      indVal <-selectedRow()
      oldList <- modelList()
      oldList[[indVal]] <- list( name = 'CoxModel',
                                 settings = list(variance = input$CMvariance,
                                                 seed = input$CMseed
                                 ))
      modelList(oldList)
      shiny::removeModal()
    })
    #=================

    # DELETE
    #=================
    # if clicked popup asks for confirmation and then deletes covariate setting
    shiny::observeEvent(input$delete_button, {
      selectedRow(as.numeric(strsplit(input$delete_button, "_")[[1]][2]))
      shiny::showModal(modelModalDelete(session$ns))
    })
    shiny::observeEvent(input$deleteModel, {
      indVal <- selectedRow()
      oldList <- modelList()
      oldList[[indVal]] <- NULL
      modelList(oldList)
      shiny::removeModal()
    })
    #=================


    return(modelList)
  }




modelModalGBM <- function(ns, model = list()) {
  shiny::modalDialog(

    shiny::textInput(inputId = ns('GBMntrees'),
                     label = 'N trees:',
                     value = ifelse(!is.null(model$settings$ntrees),model$settings$ntrees, '50,500'),
                     placeholder = ifelse(!is.null(model$settings$ntrees),model$settings$ntrees, '50,500')
    ),

    shiny::textInput(inputId = ns('GBMmaxDepth'),
                     label = 'Max Depth:',
                     value = ifelse(!is.null(model$settings$maxDepth),model$settings$maxDepth, '2,4,7,10'),
                     placeholder = ifelse(!is.null(model$settings$maxDepth),model$settings$maxDepth, '2,4,7,10')
    ),
    shiny::textInput(inputId = ns('GBMearlyStopRound'),
                     label = 'Early Stop Round:',
                     value = ifelse(!is.null(model$settings$earlyStopRound),model$settings$earlyStopRound,'10'),
                     placeholder = ifelse(!is.null(model$settings$earlyStopRound),model$settings$earlyStopRound,'10')
    ),
    shiny::textInput(inputId = ns('GBMminRows'),
                     label = 'Min Rows:',
                     value = ifelse(!is.null(model$settings$minRows),model$settings$minRows, '5,10,100'),
                     placeholder = ifelse(!is.null(model$settings$minRows),model$settings$minRows, '5,10,100')
    ),
    shiny::textInput(inputId = ns('GBMlearnRate'),
                     label = 'Learn Rate:',
                     value = ifelse(!is.null(model$settings$learnRate),model$settings$learnRate, '0.1'),
                     placeholder = ifelse(!is.null(model$settings$learnRate),model$settings$learnRate, '0.1')
    ),
    shiny::numericInput(inputId = ns('GBMseed'),
                        label = 'Seed:',
                        value = ifelse(!is.null(model$settings$seed),model$settings$seed,111),
                        min = 1,
                        max = 100000,
                        step = 1),
    shiny::numericInput(inputId = ns('GBMnthread'),
                        label = 'Ntread:',
                        value = ifelse(!is.null(model$settings$nthread),model$settings$nthread,-1),
                        min = -1,
                        max = 100,
                        step = 1),

    footer = shiny::tagList(
      shiny::actionButton(ns('addGBM'), 'Update GBM')
    )

  )}

modelModalLR <- function(ns, model = list()) {
  shiny::modalDialog(
    shiny::numericInput(inputId = ns('LRvariance'),
                        label = 'Variance:',
                        value = ifelse(!is.null(model$settings$variance),model$settings$variance, 0.001),
                        min = 0,
                        max = 1000,
                        step = 0.001),
    shiny::numericInput(inputId = ns('LRseed'),
                        label = 'Seed:',
                        value = ifelse(!is.null(model$settings$seed),model$settings$seed,111),
                        min = 1,
                        max = 100000,
                        step = 1),
    shiny::numericInput(inputId = ns('LRthreads'),
                        label = 'Treads:',
                        value = ifelse(!is.null(model$settings$nthread),model$settings$threads,-1),
                        min = -1,
                        max = 100,
                        step = 1),

    footer = shiny::tagList(
      shiny::actionButton(ns('addLR'), 'Update LR')
    )
  )
}

modelModalRF <- function(ns, model = list()) {
  shiny::modalDialog(

    shiny::textInput(inputId = ns('RFmtries'),
                     label = 'mtries:',
                     value = ifelse(!is.null(model$settings$mtries),model$settings$mtries, '-1'),
                     placeholder = ifelse(!is.null(model$settings$mtries),model$settings$mtries, '-1')
    ),

    shiny::textInput(inputId = ns('RFntrees'),
                     label = 'N trees:',
                     value = ifelse(!is.null(model$settings$ntrees),model$settings$ntrees, '50,500'),
                     placeholder = ifelse(!is.null(model$settings$ntrees),model$settings$ntrees, '50,500')
    ),

    shiny::textInput(inputId = ns('RFmaxDepth'),
                     label = 'Max Depth:',
                     value = ifelse(!is.null(model$settings$maxDepth),model$settings$maxDepth, '2,4,7,10'),
                     placeholder = ifelse(!is.null(model$settings$maxDepth),model$settings$maxDepth, '2,4,7,10')
    ),
    shiny::checkboxInput(inputId = ns('RFvarImp'),
                     label = 'Do a preliminary variable importance to filter variables to smaller number:',
                     value = ifelse(!is.null(model$settings$varImp),model$settings$varImp,T)
    ),
    shiny::numericInput(inputId = ns('RFseed'),
                        label = 'Seed:',
                        value = ifelse(!is.null(model$settings$seed),model$settings$seed,111),
                        min = 1,
                        max = 100000,
                        step = 1),

    footer = shiny::tagList(
      shiny::actionButton(ns('addRF'), 'Update RandomForest')
    )

  )}

modelModalAB <- function(ns, model = list()) {
  shiny::modalDialog(

    shiny::textInput(inputId = ns('ABnEstimators'),
                     label = 'nEstimators:',
                     value = ifelse(!is.null(model$settings$nEstimators),model$settings$nEstimators, '50'),
                     placeholder = ifelse(!is.null(model$settings$nEstimators),model$settings$nEstimators, '50')
    ),

    shiny::textInput(inputId = ns('ABlearningRate'),
                     label = 'learningRate:',
                     value = ifelse(!is.null(model$settings$learningRate),model$settings$learningRate, '1'),
                     placeholder = ifelse(!is.null(model$settings$learningRate),model$settings$learningRate, '1')
    ),

    shiny::numericInput(inputId = ns('ABseed'),
                        label = 'Seed:',
                        value = ifelse(!is.null(model$settings$seed),model$settings$seed,111),
                        min = 1,
                        max = 100000,
                        step = 1),

    footer = shiny::tagList(
      shiny::actionButton(ns('addAB'), 'Update AdaBoost')
    )

  )}

modelModalDT <- function(ns, model = list()) {
  shiny::modalDialog(

    shiny::textInput(inputId = ns('DTmaxDepth'),
                     label = 'maxDepth:',
                     value = ifelse(!is.null(model$settings$maxDepth),model$settings$maxDepth, '10'),
                     placeholder = ifelse(!is.null(model$settings$maxDepth),model$settings$maxDepth, '10')
    ),

    shiny::textInput(inputId = ns('DTminSamplesSplit'),
                     label = 'minSamplesSplit:',
                     value = ifelse(!is.null(model$settings$minSamplesSplit),model$settings$minSamplesSplit, '2'),
                     placeholder = ifelse(!is.null(model$settings$minSamplesSplit),model$settings$minSamplesSplit, '2')
    ),

    shiny::textInput(inputId = ns('DTminSamplesLeaf'),
                     label = 'minSamplesLeaf:',
                     value = ifelse(!is.null(model$settings$minSamplesSplit),model$settings$minSamplesSplit, '10'),
                     placeholder = ifelse(!is.null(model$settings$minSamplesSplit),model$settings$minSamplesSplit, '10')
    ),

    shiny::textInput(inputId = ns('DTminImpurityDecrease'),
                     label = 'minImpurityDecrease:',
                     value = ifelse(!is.null(model$settings$minImpurityDecrease),model$settings$minImpurityDecrease, '0.000001'),
                     placeholder = ifelse(!is.null(model$settings$minImpurityDecrease),model$settings$minImpurityDecrease, '0.000001')
    ),

    shiny::selectInput(inputId = ns('DTclassWeight'),
                       label = 'classWeight:',
                       choices = c('Balance', 'None'),
                       selected = ifelse(!is.null(model$settings$classWeight),model$settings$classWeight,'None')
    ),

    shiny::numericInput(inputId = ns('DTseed'),
                        label = 'Seed:',
                        value = ifelse(!is.null(model$settings$seed),model$settings$seed,111),
                        min = 1,
                        max = 100000,
                        step = 1),

    footer = shiny::tagList(
      shiny::actionButton(ns('addDT'), 'Update DecisionTree')
    )

  )}

modelModalCM <- function(ns, model = list()) {
  shiny::modalDialog(

    shiny::numericInput(inputId = ns('CMvariance'),
                        label = 'Variance:',
                        value = ifelse(!is.null(model$settings$variance),model$settings$variance, 0.001),
                        min = 0,
                        max = 1000,
                        step = 0.001),
    shiny::numericInput(inputId = ns('CMseed'),
                        label = 'Seed:',
                        value = ifelse(!is.null(model$settings$seed),model$settings$seed,111),
                        min = 1,
                        max = 100000,
                        step = 1),

    footer = shiny::tagList(
      shiny::actionButton(ns('addCM'), 'Update CoxModel')
    )

  )}

modelModalDelete <- function(ns) {
  shiny::modalDialog(

    # select the type of covariate to add to the covarite Name
    shiny::h2('Delete Model?'),

    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns('deleteModel'), 'Yes')
    )
  )
}


getType <- function(modelSelect){
  type <- 'GBM'
  if(modelSelect == 'LassoLogisticRegression'){
    type <- 'LR'
  }
  if(modelSelect == 'RandomForest'){
    type <- 'RF'
  }
  if(modelSelect == 'CoxModel'){
    type <- 'CM'
  }
  if(modelSelect == 'AdaBoost'){
    type <- 'AB'
  }
  if(modelSelect == 'DecisionTree'){
    type <- 'DT'
  }

  return(type)
}

getSetting <- function(modelList){

  unlist(lapply(modelList, function(x) paste0(rbind(names(unlist(x$settings)), unlist(x$settings)), collapse=':', sep=' ')))

}
