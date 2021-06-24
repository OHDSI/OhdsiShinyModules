# @file covariateModule.R
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

#' The user interface for creating covariate settings for FeatureExtraction
#' @param id   The module identifier
#' @param labelv A string label used for the form box
#'
#' @examples
#' \dontrun{
#' covariateViewer('covariate1', labelv = "Covariates")
#' }
#' @export
covariateViewer <- function(id, labelv = "Covariates") {
  ns <- shiny::NS(id)

  shinydashboard::box(title = labelv, width = '60%',

                      # add table with existing models

                      shiny::dataTableOutput(ns('covariateTable')),

                      # add a covariate
                      shiny::textInput(inputId = ns('covariateName'),
                                       label = 'New Covariate Name:'),
                      shiny::actionButton(inputId = ns('addCovariate'),
                                          label = 'Add New Covariate')

  )
}

#' The server for creating FeatureExtraction covariate settings
#' @param input   Standard for shiny modules
#' @param output  Standard for shiny modules
#' @param session Standard for shiny modules
#' @param cohortReactive The list of cohort in an ATLAS webApi
#'
#' @examples
#' \dontrun{
#' webApi <- callModule(webApiServer, 'webApiMain')
#' cohortReactive <- callModule(extractCohortsServer, 'extractCohort1',
#'                             webApi = webApi)
#' covList <- callModule(covariateServer, 'covariate1', cohortReactive)                            
#' }
#' @export
covariateServer <- #function(id) {
  #moduleServer(
  #id,
  function(input, output, session,
           cohortReactive) {

    # create the list of covariates
    covList <- shiny::reactiveVal(list())
    selectedRow <- shiny::reactiveVal(0)
    covIndex <- shiny::reactiveVal(0)

    shiny::observeEvent(input$addCovariate, {
      if(!is.null(input$covariateName) && input$covariateName!="" && !input$covariateName%in%names(covList())){
        oldList <- covList()
        i <- length(oldList)
        oldList[[i+1]] <- list()
        names(oldList)[i+1] <- input$covariateName
        covList(oldList)

        # update selector
        selectedRow(i+1)
        covIndex(1)

        # display editor
        shiny::showModal(covariateModuleViewer(session$ns))
      }
    })

    shiny::observeEvent(input$addNewCovariate, {
        oldList <- covList()
        i <- selectedRow()
        covInd <- length(oldList[[i]])
        oldList[[i]][[covInd+1]] <- list()
        covList(oldList)

        # update selector
        covIndex(covInd+1)

        # display editor
        shiny::showModal(covariateModuleSelector(session$ns))

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
    output$covariateTable <- shiny::renderDataTable({
      if(length(covList())>0){
        data.frame(Name = names(covList()),
                   #View = shinyInput(actionButton, length(names(covList())), 'button_', label = "View", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("view_button"))  ),
                   Edit = shinyInput(shiny::actionButton, length(names(covList())), 'button_', label = "Edit", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("select_button"))  ),
                   stringsAsFactors = FALSE,
                   row.names = 1:length(names(covList())),
                   Remove = shinyInput(shiny::actionButton, length(names(covList())), 'button_', label = "Remove", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("delete_button"))  )

        )} else{
          NULL
        }
    }, escape = FALSE)

    #=================

    # ADD COV
    #=================
    # Show modal when button is clicked.
    shiny::observeEvent(input$select_button, {
      selectedRow(as.numeric(strsplit(input$select_button, "_")[[1]][2]))

      typeNames <- lapply(covList()[[as.numeric(strsplit(input$select_button, "_")[[1]][2])]], function(x) x$fnct)#add
      if(length(typeNames)==0){
        covariateList <- NULL
      } else{
        covariateList <- as.list(1:length(typeNames))
        names(covariateList) <- paste( 1:length(typeNames), unlist(typeNames), sep=': ')
      }

      shiny::showModal(covariateModuleViewer(session$ns, covariateList))
    })

    # When button is pressed, update covariate
    # update the covariate with new settings
    shiny::observeEvent(input$selectCovariateType, {

      # open up the correct covariate module
      shiny::removeModal()

      # get settings
      indVal <-selectedRow()
      oldList <- covList()

      if(input$covariateType!='' && !is.null(input$covariateType)){
        shiny::showModal(do.call(paste0('covariateModule',input$covariateType),
                                 list(ns = session$ns, settings = oldList[[indVal]][[covIndex()]],
                                      cohortReactive = cohortReactive())))
      }
    })


    shiny::observeEvent(input$updateStandard, {
      indVal <-selectedRow()
      oldList <- covList()
      i <- covIndex()
      oldList[[indVal]][[i]] <- list(fnct = 'Standard',
                                     settings = list(useDemographicsGender = input$useDemographicsGender,
                                     useDemographicsAge = input$useDemographicsAge,
                                     useDemographicsAgeGroup = input$useDemographicsAgeGroup,
                                     useDemographicsRace = input$useDemographicsRace,
                                     useDemographicsEthnicity = input$useDemographicsEthnicity,
                                     useDemographicsIndexYear = input$useDemographicsIndexYear,
                                     useDemographicsIndexMonth = input$useDemographicsIndexMonth,
                                     useDemographicsPriorObservationTime = input$useDemographicsPriorObservationTime,
                                     useDemographicsPostObservationTime = input$useDemographicsPostObservationTime,
                                     useDemographicsTimeInCohort = input$useDemographicsTimeInCohort,
                                     useDemographicsIndexYearMonth = F,
                                     useConditionOccurrenceAnyTimePrior = input$useConditionOccurrenceAnyTimePrior,
                                     useConditionOccurrenceLongTerm = input$useConditionOccurrenceLongTerm,
                                     useConditionOccurrenceMediumTerm = input$useConditionOccurrenceMediumTerm,
                                     useConditionOccurrenceShortTerm = input$useConditionOccurrenceShortTerm,
                                     useConditionOccurrencePrimaryInpatientAnyTimePrior = input$useConditionOccurrencePrimaryInpatientAnyTimePrior,
                                     useConditionOccurrencePrimaryInpatientLongTerm = input$useConditionOccurrencePrimaryInpatientLongTerm,
                                     useConditionOccurrencePrimaryInpatientMediumTerm = input$useConditionOccurrencePrimaryInpatientMediumTerm,
                                     useConditionOccurrencePrimaryInpatientShortTerm = input$useConditionOccurrencePrimaryInpatientShortTerm,
                                     useConditionEraAnyTimePrior = input$useConditionEraAnyTimePrior,
                                     useConditionEraLongTerm = input$useConditionEraLongTerm,
                                     useConditionEraMediumTerm = input$useConditionEraMediumTerm,
                                     useConditionEraShortTerm = input$useConditionEraShortTerm,
                                     useConditionEraOverlapping = FALSE,
                                     useConditionEraStartLongTerm = FALSE,
                                     useConditionEraStartMediumTerm = FALSE,
                                     useConditionEraStartShortTerm = FALSE,
                                     useConditionGroupEraAnyTimePrior = input$useConditionGroupEraAnyTimePrior,
                                     useConditionGroupEraLongTerm = input$useConditionGroupEraLongTerm,
                                     useConditionGroupEraMediumTerm = input$useConditionGroupEraMediumTerm,
                                     useConditionGroupEraShortTerm = input$useConditionGroupEraShortTerm,
                                     useConditionGroupEraOverlapping = FALSE,
                                     useConditionGroupEraStartLongTerm = FALSE,
                                     useConditionGroupEraStartMediumTerm = FALSE,
                                     useConditionGroupEraStartShortTerm = FALSE,
                                     useDrugExposureAnyTimePrior = input$useDrugExposureAnyTimePrior,
                                     useDrugExposureLongTerm = input$useDrugExposureLongTerm,
                                     useDrugExposureMediumTerm = input$useDrugExposureMediumTerm ,
                                     useDrugExposureShortTerm = input$useDrugExposureShortTerm ,
                                     useDrugEraAnyTimePrior = input$useDrugEraAnyTimePrior,
                                     useDrugEraLongTerm = input$useDrugEraLongTerm,
                                     useDrugEraMediumTerm = input$useDrugEraMediumTerm,
                                     useDrugEraShortTerm = input$useDrugEraShortTerm,
                                     useDrugEraOverlapping = FALSE,
                                     useDrugEraStartLongTerm = FALSE,
                                     useDrugEraStartMediumTerm = FALSE,
                                     useDrugEraStartShortTerm = FALSE,
                                     useDrugGroupEraAnyTimePrior = input$useDrugGroupEraAnyTimePrior,
                                     useDrugGroupEraLongTerm = input$useDrugGroupEraLongTerm,
                                     useDrugGroupEraMediumTerm = input$useDrugGroupEraMediumTerm,
                                     useDrugGroupEraShortTerm = input$useDrugGroupEraShortTerm,
                                     useDrugGroupEraOverlapping = FALSE,
                                     useDrugGroupEraStartLongTerm = FALSE,
                                     useDrugGroupEraStartMediumTerm = FALSE,
                                     useDrugGroupEraStartShortTerm = FALSE,
                                     useProcedureOccurrenceAnyTimePrior = input$useProcedureOccurrenceAnyTimePrior,
                                     useProcedureOccurrenceLongTerm = input$useProcedureOccurrenceLongTerm,
                                     useProcedureOccurrenceMediumTerm = input$useProcedureOccurrenceMediumTerm ,
                                     useProcedureOccurrenceShortTerm = input$useProcedureOccurrenceShortTerm,
                                     useDeviceExposureAnyTimePrior = input$useDeviceExposureAnyTimePrior,
                                     useDeviceExposureLongTerm = input$useDeviceExposureLongTerm,
                                     useDeviceExposureMediumTerm = input$useDeviceExposureMediumTerm,
                                     useDeviceExposureShortTerm = input$useDeviceExposureShortTerm,
                                     useMeasurementAnyTimePrior = input$useMeasurementAnyTimePrior,
                                     useMeasurementLongTerm = input$useMeasurementLongTerm,
                                     useMeasurementMediumTerm = input$useMeasurementMediumTerm,
                                     useMeasurementShortTerm = input$useMeasurementShortTerm,
                                     useMeasurementValueAnyTimePrior = FALSE,
                                     useMeasurementValueLongTerm = FALSE,
                                     useMeasurementValueMediumTerm = FALSE,
                                     useMeasurementValueShortTerm = FALSE,
                                     useMeasurementRangeGroupAnyTimePrior = input$useMeasurementRangeGroupAnyTimePrior,
                                     useMeasurementRangeGroupLongTerm = input$useMeasurementRangeGroupLongTerm,
                                     useMeasurementRangeGroupMediumTerm = input$useMeasurementRangeGroupMediumTerm,
                                     useMeasurementRangeGroupShortTerm = input$useMeasurementRangeGroupShortTerm,
                                     useObservationAnyTimePrior = input$useObservationAnyTimePrior,
                                     useObservationLongTerm = input$useObservationLongTerm,
                                     useObservationMediumTerm = input$useObservationMediumTerm,
                                     useObservationShortTerm = input$useObservationShortTerm,
                                     useCharlsonIndex = input$useCharlsonIndex,
                                     useDcsi = input$useDcsi,
                                     useChads2 =  input$useChads2 ,
                                     useChads2Vasc = input$useChads2Vasc,
                                     useHfrs = input$useHfrs,
                                     useDistinctConditionCountLongTerm = input$useDistinctConditionCountLongTerm,
                                     useDistinctConditionCountMediumTerm = input$useDistinctConditionCountMediumTerm,
                                     useDistinctConditionCountShortTerm = input$useDistinctConditionCountShortTerm,
                                     useDistinctIngredientCountLongTerm = input$useDistinctIngredientCountLongTerm,
                                     useDistinctIngredientCountMediumTerm = input$useDistinctIngredientCountMediumTerm,
                                     useDistinctIngredientCountShortTerm = input$useDistinctIngredientCountShortTerm,
                                     useDistinctProcedureCountLongTerm = input$useDistinctProcedureCountLongTerm,
                                     useDistinctProcedureCountMediumTerm = input$useDistinctProcedureCountMediumTerm,
                                     useDistinctProcedureCountShortTerm = input$useDistinctProcedureCountShortTerm,
                                     useDistinctMeasurementCountLongTerm = input$useDistinctMeasurementCountLongTerm,
                                     useDistinctMeasurementCountMediumTerm = input$useDistinctMeasurementCountMediumTerm,
                                     useDistinctMeasurementCountShortTerm = input$useDistinctMeasurementCountShortTerm,
                                     useDistinctObservationCountLongTerm = input$useDistinctObservationCountLongTerm,
                                     useDistinctObservationCountMediumTerm = input$useDistinctObservationCountMediumTerm,
                                     useDistinctObservationCountShortTerm = input$useDistinctObservationCountShortTerm,
                                     useVisitCountLongTerm = input$useVisitCountLongTerm,
                                     useVisitCountMediumTerm = input$useVisitCountMediumTerm,
                                     useVisitCountShortTerm = input$useVisitCountShortTerm,
                                     useVisitConceptCountLongTerm = input$useVisitConceptCountLongTerm,
                                     useVisitConceptCountMediumTerm = input$useVisitConceptCountMediumTerm,
                                     useVisitConceptCountShortTerm = input$useVisitConceptCountShortTerm,
                                     longTermStartDays = input$longTermStartDays,
                                     mediumTermStartDays = input$mediumTermStartDays,
                                     shortTermStartDays = input$shortTermStartDays,
                                     endDays = input$endDays,
                                     includedCovariateConceptIds = input$includedCovariateConceptIds,
                                     addDescendantsToInclude = input$addDescendantsToInclude,
                                     excludedCovariateConceptIds = input$excludedCovariateConceptIds,
                                     addDescendantsToExclude = input$addDescendantsToExclude,
                                     includedCovariateIds = input$includedCovariateIds))
      covList(oldList)
      shiny::removeModal()
    })

    shiny::observeEvent(input$deleteStandard, {
      indVal <-selectedRow()
      oldList <- covList()
      i <- covIndex()
      if(i>1){
        oldList[[indVal]][[i]] <- NULL
        covIndex(i-1)
      } else{
        oldList[[indVal]] <- list()
        covIndex(0)
      }
      covList(oldList)
      shiny::removeModal()
    })

    shiny::observeEvent(input$updateCohort, {

      if(nrow(cohortReactive())>0){
        indVal <- selectedRow()
        oldList <- covList()
        i <- covIndex()
        oldList[[indVal]][[i]] <- list(fnct = 'Cohort', # createCohortCovariateSettings
                                       settings = list(cohortCovcovariateName =  cohortReactive()$name[cohortReactive()$id== input$cohortCovcohortId],
                                                       cohortCovcohortId = input$cohortCovcohortId,
                                                       covariateId = as.double(input$cohortCovcohortId)*1000+as.double(input$cohortCovanalysisId),
                                                       cohortCovstartDay= input$cohortCovstartDay,
                                                       cohortCovendDay= input$cohortCovendDay,
                                                       cohortCovcount=input$cohortCovcount,
                                                       cohortCovageInteraction = input$cohortCovageInteraction,
                                                       cohortCovlnAgeInteraction= input$cohortCovlnAgeInteraction,
                                                       cohortCovanalysisId = input$cohortCovanalysisId)
        )
        covList(oldList)
      } else{
        shiny::showNotification('Need to connect to a valid webApi to fetch the cohorts', duration = 5, type = 'error')
      }
      shiny::removeModal()
    })

    shiny::observeEvent(input$deleteCohort, {
      oldList <- covList()
      indVal <- selectedRow()
      i <- covIndex()
      if(i>1){
        oldList[[indVal]][[i]] <- NULL
        covIndex(i-1)
      } else{
        oldList[[indVal]] <- list()
        covIndex(0)
      }
      covList(oldList)
      shiny::removeModal()
    })

    shiny::observeEvent(input$updateAge, {

        indVal <- selectedRow()
        oldList <- covList()
        i <- covIndex()
        oldList[[indVal]][[i]] <- list(fnct = 'Age', # createAgeCovariateSettings
                                       settings = list(ageCovcovariateName =  input$ageCovcovariateName,
                                                       ageCovageMap = input$ageCovageMap,
                                                       ageCovcovariateId =  (indVal*1000+sample(9,1)*100+i)*1000+input$ageCovanalysisId,
                                                       ageCovanalysisId = input$ageCovanalysisId)
        )
        covList(oldList)

      shiny::removeModal()
    })

    shiny::observeEvent(input$deleteAge, {
      indVal <-selectedRow()
      oldList <- covList()
      i <- covIndex()
      if(i>1){
        oldList[[indVal]][[i]] <- NULL
        covIndex(i-1)
      } else{
        oldList[[indVal]] <- list()
        covIndex(0)
      }
      covList(oldList)
      shiny::removeModal()
    })

    shiny::observeEvent(input$updateMeasurement, {

      indVal <- selectedRow()
      oldList <- covList()
      i <- covIndex()
      oldList[[indVal]][[i]] <- list(fnct = 'Measurement', # createMeasurementCovariateSettings
                                     settings = list(measureCovcovariateName =  input$measureCovcovariateName,
                                                     measureCovconceptSet =  input$measureCovconceptSet,

                                                     measureCovstartDay =  input$measureCovstartDay,
                                                     measureCovendDay =  input$measureCovendDay,

                                                     measureCovscaleMap =  input$measureCovscaleMap,
                                                     measureCovaggregateMethod =  input$measureCovaggregateMethod,

                                                     measureCovimputationValue =  input$measureCovimputationValue,
                                                     measureCovageInteraction =  input$measureCovageInteraction,
                                                     measureCovlnAgeInteraction =  input$measureCovlnAgeInteraction,
                                                     measureCovlnValue =  input$measureCovlnValue,

                                                     measureCovcovariateId =  (indVal*1000+sample(9,1)*100+i)*1000+input$measureCovanalysisId,

                                                     measureCovanalysisId = input$measureCovanalysisId)
      )
      covList(oldList)

      shiny::removeModal()
    })

    shiny::observeEvent(input$deleteMeasurement, {
      indVal <-selectedRow()
      oldList <- covList()
      i <- covIndex()
      if(i>1){
        oldList[[indVal]][[i]] <- NULL
        covIndex(i-1)
      } else{
        oldList[[indVal]] <- list()
        covIndex(0)
      }
      covList(oldList)
      shiny::removeModal()
    })


    # update the settings
    shiny::observeEvent(input$updateCovariate, {
      shiny::removeModal()

      indVal <-selectedRow()
      oldList <- covList()
      i <- as.double(input$covariateId)
      covIndex(i)
      result <- oldList[[indVal]][[i]]
      type <- result$fnct

      if(type !='' && !is.null(type)){
        shiny::showModal(do.call(paste0('covariateModule',type),
                                 list(ns = session$ns, settings = result,
                                      cohortReactive = cohortReactive())))
      }

    })

    #=================

    # DELETE full COV
    #=================
    # if clicked popup asks for confirmation and then deletes covariate setting
    shiny::observeEvent(input$delete_button, {
      selectedRow(as.numeric(strsplit(input$delete_button, "_")[[1]][2]))
      shiny::showModal(covariateModalDelete(session$ns))
    })
    shiny::observeEvent(input$deleteCovariateSetting, {
      indVal <- selectedRow()
      oldList <- covList()
      oldList[[indVal]] <- NULL
      covList(oldList)
      shiny::removeModal()
    })
    #=================


    return(covList)
  }


covariateModuleViewer <- function(ns,covariateList = NULL) {
  shiny::modalDialog(

    # select the type of covariate to add to the covarite Name
    shinydashboard::box(width = 5, status = 'primary', height = '100%',
      title = 'Edit Existing Covariate:',
      shiny::selectInput(inputId = ns('covariateId'),
                       label = 'Covariate:',
                       choices = covariateList),
      shiny::actionButton(ns('updateCovariate'), 'Edit')
    ),
    shinydashboard::box(width = 5, status = 'success', height = '100%',
                        title = 'Add Covariate',
                        shiny::br(),
                        shiny::br(),
                        shiny::br(),
                        shiny::br(),
      shiny::actionButton(ns('addNewCovariate'), 'Add')
    )

  )
}

covariateModuleSelector <- function(ns, types = c('Standard', 'Cohort', 'Measurement', 'Age')) {
  shiny::modalDialog(

    # select the type of covariate to add to the covarite Name
    shiny::selectInput(inputId = ns('covariateType'), label = 'Covariate Type:',
                       choices = types
    ),

    footer = shiny::tagList(
      #modalButton("Cancel"),
      shiny::actionButton(ns('selectCovariateType'), 'Select Covariate')
    )
  )
}


covariateModuleStandard <- function(ns, settings, ...) { # settings is list(fnct, settings)
  shiny::modalDialog(

    shinydashboard::box(title = 'Time Settings (relative to index)', width = 12, status = 'info',
    shiny::fluidRow(
      shiny::column(width = 3,
             shiny::numericInput(ns("endDays"), "End Days:            ", value = ifelse(is.null(settings$settings$endDays), 0, settings$settings$endDays) )
      ),
      shiny::column(width = 3,
             shiny::numericInput(ns("longTermStartDays"), "Long Term Start Days:", value = ifelse(is.null(settings$settings$longTermStartDays), -365, settings$settings$longTermStartDays) )
      ),
      shiny::column(width = 3,
             shiny::numericInput(ns("mediumTermStartDays"), "Medium Term Start Days:", value = ifelse(is.null(settings$settings$mediumTermStartDays), -180, settings$settings$mediumTermStartDays) )
      ),
      shiny::column(width = 3,
             shiny::numericInput(ns("shortTermStartDays"), "Short Term Start Days:", value = ifelse(is.null(settings$settings$shortTermStartDays), -30, settings$settings$shortTermStartDays) )
      )
    )),

    shinydashboard::box(title = 'Demographics', width = 12,status = 'info',
                        shinyWidgets::prettyCheckbox( inputId = ns("useDemographicsGender"),
                                                      value = ifelse(is.null(settings$settings$useDemographicsGender), T, settings$settings$useDemographicsGender), inline = T,
                                                      label = "Gender", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDemographicsAgeGroup"),
                                                      value = ifelse(is.null(settings$settings$useDemographicsAgeGroup), T, settings$settings$useDemographicsAgeGroup),inline = T,
                                                      label = "Age (5-year bins)", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDemographicsAge"),
                                                      value = ifelse(is.null(settings$settings$useDemographicsAge), F, settings$settings$useDemographicsAge),inline = T,
                                                      label = "Age (years)", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDemographicsRace"),
                                                      value = ifelse(is.null(settings$settings$useDemographicsRace), F, settings$settings$useDemographicsRace), inline = T,
                                                      label = "Race", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDemographicsEthnicity"),
                                                      value = ifelse(is.null(settings$settings$useDemographicsEthnicity), F, settings$settings$useDemographicsEthnicity), inline = T,
                                                      label = "Ethnicity", icon = shiny::icon("check")
                        )
    ),

    shinydashboard::box(title = 'Index Information', width = 12, status = 'info',
                        shinyWidgets::prettyCheckbox( inputId = ns("useDemographicsIndexYear"),
                                                      value = ifelse(is.null(settings$settings$useDemographicsIndexYear), F, settings$settings$useDemographicsIndexYear), inline = T,
                                                      label = "Index Year", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDemographicsIndexMonth"),
                                                      value = ifelse(is.null(settings$settings$useDemographicsIndexMonth), F, settings$settings$useDemographicsIndexMonth),inline = T,
                                                      label = "Index Month", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDemographicsPriorObservationTime"),
                                                      value = ifelse(is.null(settings$settings$useDemographicsPriorObservationTime), F, settings$settings$useDemographicsPriorObservationTime),inline = T,
                                                      label = "Time In Database Before Index", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDemographicsPostObservationTime"),
                                                      value = ifelse(is.null(settings$settings$useDemographicsPostObservationTime), F, settings$settings$useDemographicsPostObservationTime), inline = T,
                                                      label = "Time In Database After Index", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDemographicsTimeInCohort"),
                                                      value = ifelse(is.null(settings$settings$useDemographicsTimeInCohort), F, settings$settings$useDemographicsTimeInCohort), inline = T,
                                                      label = "Time In Cohort", icon = shiny::icon("check")
                        )
    ),

    shinydashboard::box(title = 'Condition ', width = 12, status = 'info',
    shinydashboard::box(title = 'Condition Occurrence', width = 12,
      shinyWidgets::prettyCheckbox( inputId = ns("useConditionOccurrenceAnyTimePrior"),
                      value = ifelse(is.null(settings$settings$useConditionOccurrenceAnyTimePrior), F, settings$settings$useConditionOccurrenceAnyTimePrior), inline = T,
                      label = "Any Time", icon = shiny::icon("check")
      ),
      shinyWidgets::prettyCheckbox( inputId = ns("useConditionOccurrenceLongTerm"),
                      value = ifelse(is.null(settings$settings$useConditionOccurrenceLongTerm), F, settings$settings$useConditionOccurrenceLongTerm),inline = T,
                      label = "Long Term", icon = shiny::icon("check")
      ),
      shinyWidgets::prettyCheckbox( inputId = ns("useConditionOccurrenceMediumTerm"),
                      value = ifelse(is.null(settings$settings$useConditionOccurrenceMediumTerm), F, settings$settings$useConditionOccurrenceMediumTerm),inline = T,
                      label = "Medium Term", icon = shiny::icon("check")
      ),
      shinyWidgets::prettyCheckbox( inputId = ns("useConditionOccurrenceShortTerm"),
                      value = ifelse(is.null(settings$settings$useConditionOccurrenceShortTerm), F, settings$settings$useConditionOccurrenceShortTerm), inline = T,
                      label = "Short Term", icon = shiny::icon("check")
      )

    ),


    shinydashboard::box(title = 'Condition Occurrence Primary Inpatient', width = 12,
                        shinyWidgets::prettyCheckbox( inputId = ns("useConditionOccurrencePrimaryInpatientAnyTimePrior"),
                                                      value = ifelse(is.null(settings$settings$useConditionOccurrencePrimaryInpatientAnyTimePrior), F, settings$settings$useConditionOccurrencePrimaryInpatientAnyTimePrior), inline = T,
                                                      label = "Any Time", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useConditionOccurrencePrimaryInpatientLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useConditionOccurrencePrimaryInpatientLongTerm), F, settings$settings$useConditionOccurrencePrimaryInpatientLongTerm),inline = T,
                                                      label = "Long Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useConditionOccurrencePrimaryInpatientMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useConditionOccurrencePrimaryInpatientMediumTerm), F, settings$settings$useConditionOccurrencePrimaryInpatientMediumTerm),inline = T,
                                                      label = "Medium Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useConditionOccurrencePrimaryInpatientShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useConditionOccurrencePrimaryInpatientShortTerm), F, settings$settings$useConditionOccurrencePrimaryInpatientShortTerm), inline = T,
                                                      label = "Short Term", icon = shiny::icon("check")
                        )
    ),

    shinydashboard::box(title = 'Condition Era', width = 12,
                        shinyWidgets::prettyCheckbox( inputId = ns("useConditionEraAnyTimePrior"),
                                                      value = ifelse(is.null(settings$settings$useConditionEraAnyTimePrior), F, settings$settings$useConditionEraAnyTimePrior), inline = T,
                                                      label = "Any Time", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useConditionEraLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useConditionEraLongTerm), F, settings$settings$useConditionEraLongTerm),inline = T,
                                                      label = "Long Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useConditionEraMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useConditionEraMediumTerm), F, settings$settings$useConditionEraMediumTerm),inline = T,
                                                      label = "Medium Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useConditionEraShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useConditionEraShortTerm), F, settings$settings$useConditionEraShortTerm), inline = T,
                                                      label = "Short Term", icon = shiny::icon("check")
                        )

    ),


    shinydashboard::box(title = 'Condition Era (Grouped using hierarchy relationship)', width = 12,
                        shinyWidgets::prettyCheckbox( inputId = ns("useConditionGroupEraAnyTimePrior"),
                                                      value = ifelse(is.null(settings$settings$useConditionGroupEraAnyTimePrior), T, settings$settings$useConditionGroupEraAnyTimePrior), inline = T,
                                                      label = "Any Time", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useConditionGroupEraLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useConditionGroupEraLongTerm), F, settings$settings$useConditionGroupEraLongTerm),inline = T,
                                                      label = "Long Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useConditionGroupEraMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useConditionGroupEraMediumTerm), F, settings$settings$useConditionGroupEraMediumTerm),inline = T,
                                                      label = "Medium Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useConditionGroupEraShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useConditionGroupEraShortTerm), F, settings$settings$useConditionGroupEraShortTerm), inline = T,
                                                      label = "Short Term", icon = shiny::icon("check")
                        )

    )
    ),


    shinydashboard::box(title = 'Drug', width = 12, status = 'info',
    shinydashboard::box(title = 'Drug Exposure', width = 12,
                        shinyWidgets::prettyCheckbox( inputId = ns("useDrugExposureAnyTimePrior"),
                                                      value = ifelse(is.null(settings$settings$useDrugExposureAnyTimePrior), F, settings$settings$useDrugExposureAnyTimePrior), inline = T,
                                                      label = "Any Time", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDrugExposureLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useDrugExposureLongTerm), F, settings$settings$useDrugExposureLongTerm),inline = T,
                                                      label = "Long Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDrugExposureMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useDrugExposureMediumTerm), F, settings$settings$useDrugExposureMediumTerm),inline = T,
                                                      label = "Medium Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDrugExposureShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useDrugExposureShortTerm), F, settings$settings$useDrugExposureShortTerm), inline = T,
                                                      label = "Short Term", icon = shiny::icon("check")
                        )

    ),


    shinydashboard::box(title = 'Drug Era', width = 12,
                        shinyWidgets::prettyCheckbox( inputId = ns("useDrugEraAnyTimePrior"),
                                                      value = ifelse(is.null(settings$settings$useDrugEraAnyTimePrior), F, settings$settings$useDrugEraAnyTimePrior), inline = T,
                                                      label = "Any Time", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDrugEraLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useDrugEraLongTerm), F, settings$settings$useDrugEraLongTerm),inline = T,
                                                      label = "Long Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDrugEraMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useDrugEraMediumTerm), F, settings$settings$useDrugEraMediumTerm),inline = T,
                                                      label = "Medium Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDrugEraShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useDrugEraShortTerm), F, settings$settings$useDrugEraShortTerm), inline = T,
                                                      label = "Short Term", icon = shiny::icon("check")
                        )

    ),

    shinydashboard::box(title = 'Drug Era (Grouped using hierachy relationships)', width = 12,
                        shinyWidgets::prettyCheckbox( inputId = ns("useDrugGroupEraAnyTimePrior"),
                                                      value = ifelse(is.null(settings$settings$useDrugGroupEraAnyTimePrior), T, settings$settings$useDrugGroupEraAnyTimePrior), inline = T,
                                                      label = "Any Time", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDrugGroupEraLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useDrugGroupEraLongTerm), F, settings$settings$useDrugGroupEraLongTerm),inline = T,
                                                      label = "Long Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDrugGroupEraMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useDrugGroupEraMediumTerm), F, settings$settings$useDrugGroupEraMediumTerm),inline = T,
                                                      label = "Medium Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDrugGroupEraShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useDrugGroupEraShortTerm), F, settings$settings$useDrugGroupEraShortTerm), inline = T,
                                                      label = "Short Term", icon = shiny::icon("check")
                        )

    )),

    shinydashboard::box(title = 'Procedure Occurrence', width = 12, status = 'info',
                        shinyWidgets::prettyCheckbox( inputId = ns("useProcedureOccurrenceAnyTimePrior"),
                                                      value = ifelse(is.null(settings$settings$useProcedureOccurrenceAnyTimePrior), T, settings$settings$useProcedureOccurrenceAnyTimePrior), inline = T,
                                                      label = "Any Time", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useProcedureOccurrenceLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useProcedureOccurrenceLongTerm), F, settings$settings$useProcedureOccurrenceLongTerm),inline = T,
                                                      label = "Long Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useProcedureOccurrenceMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useProcedureOccurrenceMediumTerm), F, settings$settings$useProcedureOccurrenceMediumTerm),inline = T,
                                                      label = "Medium Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useProcedureOccurrenceShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useProcedureOccurrenceShortTerm), F, settings$settings$useProcedureOccurrenceShortTerm), inline = T,
                                                      label = "Short Term", icon = shiny::icon("check")
                        )

    ),


    shinydashboard::box(title = 'Device Exposure', width = 12, status = 'info',
                        shinyWidgets::prettyCheckbox( inputId = ns("useDeviceExposureAnyTimePrior"),
                                                      value = ifelse(is.null(settings$settings$useDeviceExposureAnyTimePrior), T, settings$settings$useDeviceExposureAnyTimePrior), inline = T,
                                                      label = "Any Time", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDeviceExposureLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useDeviceExposureLongTerm), F, settings$settings$useDeviceExposureLongTerm),inline = T,
                                                      label = "Long Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDeviceExposureMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useDeviceExposureMediumTerm), F, settings$settings$useDeviceExposureMediumTerm),inline = T,
                                                      label = "Medium Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDeviceExposureShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useDeviceExposureShortTerm), F, settings$settings$useDeviceExposureShortTerm), inline = T,
                                                      label = "Short Term", icon = shiny::icon("check")
                        )

    ),

    shinydashboard::box(title = 'Measurement', width = 12, status = 'info',
    shinydashboard::box(title = 'Measurement (indicating measurement was taken)', width = 12,
                        shinyWidgets::prettyCheckbox( inputId = ns("useMeasurementAnyTimePrior"),
                                                      value = ifelse(is.null(settings$settings$useMeasurementAnyTimePrior), T, settings$settings$useMeasurementAnyTimePrior), inline = T,
                                                      label = "Any Time", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useMeasurementLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useMeasurementLongTerm), F, settings$settings$useMeasurementLongTerm),inline = T,
                                                      label = "Long Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useMeasurementMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useMeasurementMediumTerm), F, settings$settings$useMeasurementMediumTerm),inline = T,
                                                      label = "Medium Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useMeasurementShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useMeasurementShortTerm), F, settings$settings$useMeasurementShortTerm), inline = T,
                                                      label = "Short Term", icon = shiny::icon("check")
                        )

    ),

    shinydashboard::box(title = 'Measurement Range (abnormal or normal)', width = 12,
                        shinyWidgets::prettyCheckbox( inputId = ns("useMeasurementRangeGroupAnyTimePrior"),
                                                      value = ifelse(is.null(settings$settings$useMeasurementRangeGroupAnyTimePrior), F, settings$settings$useMeasurementRangeGroupAnyTimePrior), inline = T,
                                                      label = "Any Time", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useMeasurementRangeGroupLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useMeasurementRangeGroupLongTerm), F, settings$settings$useMeasurementRangeGroupLongTerm),inline = T,
                                                      label = "Long Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useMeasurementRangeGroupMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useMeasurementRangeGroupMediumTerm), F, settings$settings$useMeasurementRangeGroupMediumTerm),inline = T,
                                                      label = "Medium Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useMeasurementRangeGroupShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useMeasurementRangeGroupShortTerm), F, settings$settings$useMeasurementRangeGroupShortTerm), inline = T,
                                                      label = "Short Term", icon = shiny::icon("check")
                        )

    )),


    shinydashboard::box(title = 'Observation', width = 12, status = 'info',
                        shinyWidgets::prettyCheckbox( inputId = ns("useObservationAnyTimePrior"),
                                                      value = ifelse(is.null(settings$settings$useObservationAnyTimePrior), T, settings$settings$useObservationAnyTimePrior), inline = T,
                                                      label = "Any Time", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useObservationLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useObservationLongTerm), F, settings$settings$useObservationLongTerm),inline = T,
                                                      label = "Long Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useObservationMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useObservationMediumTerm), F, settings$settings$useObservationMediumTerm),inline = T,
                                                      label = "Medium Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useObservationShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useObservationShortTerm), F, settings$settings$useObservationShortTerm), inline = T,
                                                      label = "Short Term", icon = shiny::icon("check")
                        )

    ),

    shinydashboard::box(title = 'Risk Scores', width = 12, status = 'info',
                        shinyWidgets::prettyCheckbox( inputId = ns("useCharlsonIndex"),
                                                      value = ifelse(is.null(settings$settings$useCharlsonIndex), F, settings$settings$useCharlsonIndex), inline = T,
                                                      label = "Charlson Index", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDcsi"),
                                                      value = ifelse(is.null(settings$settings$useDcsi), F, settings$settings$useDcsi),inline = T,
                                                      label = "DCSI", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useChads2"),
                                                      value = ifelse(is.null(settings$settings$useChads2), F, settings$settings$useChads2),inline = T,
                                                      label = "Chads2", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useChads2Vasc"),
                                                      value = ifelse(is.null(settings$settings$useChads2Vasc), F, settings$settings$useChads2Vasc), inline = T,
                                                      label = "Chads2Vasc", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useHfrs"),
                                                      value = ifelse(is.null(settings$settings$useHfrs), F, settings$settings$useHfrs), inline = T,
                                                      label = "HFRS", icon = shiny::icon("check")
                        )

    ),

    shinydashboard::box(title = 'Counts (Number of records)', width = 12, status = 'info',
    shinydashboard::box(title = 'Counts during Long Term', width = 12,
                        shinyWidgets::prettyCheckbox( inputId = ns("useDistinctConditionCountLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useDistinctConditionCountLongTerm), F, settings$settings$useDistinctConditionCountLongTerm), inline = T,
                                                      label = "Condition", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDistinctIngredientCountLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useDistinctIngredientCountLongTerm), F, settings$settings$useDistinctIngredientCountLongTerm),inline = T,
                                                      label = "Ingredient", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDistinctProcedureCountLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useDistinctProcedureCountLongTerm), F, settings$settings$useDistinctProcedureCountLongTerm),inline = T,
                                                      label = "Procedure", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDistinctMeasurementCountLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useDistinctMeasurementCountLongTerm), F, settings$settings$useDistinctMeasurementCountLongTerm), inline = T,
                                                      label = "Measurement", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDistinctObservationCountLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useDistinctObservationCountLongTerm), F, settings$settings$useDistinctObservationCountLongTerm), inline = T,
                                                      label = "Observation", icon = shiny::icon("check")
                        )

    ),

    shinydashboard::box(title = 'Counts during Medium Term', width = 12,
                        shinyWidgets::prettyCheckbox( inputId = ns("useDistinctConditionCountMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useDistinctConditionCountMediumTerm), F, settings$settings$useDistinctConditionCountMediumTerm), inline = T,
                                                      label = "Condition", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDistinctIngredientCountMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useDistinctIngredientCountMediumTerm), F, settings$settings$useDistinctIngredientCountMediumTerm),inline = T,
                                                      label = "Ingredient", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDistinctProcedureCountMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useDistinctProcedureCountMediumTerm), F, settings$settings$useDistinctProcedureCountMediumTerm),inline = T,
                                                      label = "Procedure", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDistinctMeasurementCountMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useDistinctMeasurementCountMediumTerm), F, settings$settings$useDistinctMeasurementCountMediumTerm), inline = T,
                                                      label = "Measurement", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDistinctObservationCountMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useDistinctObservationCountMediumTerm), F, settings$settings$useDistinctObservationCountMediumTerm), inline = T,
                                                      label = "Observation", icon = shiny::icon("check")
                        )

    ),

    shinydashboard::box(title = 'Counts during Short Term', width = 12,
                        shinyWidgets::prettyCheckbox( inputId = ns("useDistinctConditionCountShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useDistinctConditionCountShortTerm), F, settings$settings$useDistinctConditionCountShortTerm), inline = T,
                                                      label = "Condition", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDistinctIngredientCountShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useDistinctIngredientCountShortTerm), F, settings$settings$useDistinctIngredientCountShortTerm),inline = T,
                                                      label = "Ingredient", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDistinctProcedureCountShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useDistinctProcedureCountShortTerm), F, settings$settings$useDistinctProcedureCountShortTerm),inline = T,
                                                      label = "Procedure", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDistinctMeasurementCountShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useDistinctMeasurementCountShortTerm), F, settings$settings$useDistinctMeasurementCountShortTerm), inline = T,
                                                      label = "Measurement", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useDistinctObservationCountShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useDistinctObservationCountShortTerm), F, settings$settings$useDistinctObservationCountShortTerm), inline = T,
                                                      label = "Observation", icon = shiny::icon("check")
                        )

    )),

    shinydashboard::box(title = 'Visit Counts', width = 12, status = 'info',
    shinydashboard::box(title = 'Visit Counts (total)', width = 12,

                        shinyWidgets::prettyCheckbox( inputId = ns("useVisitCountLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useVisitCountLongTerm), F, settings$settings$useVisitCountLongTerm),inline = T,
                                                      label = "Long Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useVisitCountMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useVisitCountMediumTerm), F, settings$settings$useVisitCountMediumTerm),inline = T,
                                                      label = "Medium Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useVisitCountShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useVisitCountShortTerm), F, settings$settings$useVisitCountShortTerm), inline = T,
                                                      label = "Short Term", icon = shiny::icon("check")
                        )

    ),

    shinydashboard::box(title = 'Visit Counts (per visit concept type)', width = 12,

                        shinyWidgets::prettyCheckbox( inputId = ns("useVisitConceptCountLongTerm"),
                                                      value = ifelse(is.null(settings$settings$useVisitConceptCountLongTerm), F, settings$settings$useVisitConceptCountLongTerm),inline = T,
                                                      label = "Long Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useVisitConceptCountMediumTerm"),
                                                      value = ifelse(is.null(settings$settings$useVisitConceptCountMediumTerm), F, settings$settings$useVisitConceptCountMediumTerm),inline = T,
                                                      label = "Medium Term", icon = shiny::icon("check")
                        ),
                        shinyWidgets::prettyCheckbox( inputId = ns("useVisitConceptCountShortTerm"),
                                                      value = ifelse(is.null(settings$settings$useVisitConceptCountShortTerm), F, settings$settings$useVisitConceptCountShortTerm), inline = T,
                                                      label = "Short Term", icon = shiny::icon("check")
                        )

    )),

    shinydashboard::box(title = 'Inclusions/Exclusions', width = 12, status = 'info',
    shiny::fluidRow(
      shiny::column(width = 6,
             shiny::textInput(ns("includedCovariateConceptIds"), "Included Covariate Concept Ids:", value = '' )
      ),
      shiny::column(width = 3,
             shiny::checkboxInput(ns("addDescendantsToInclude"), "Add Descendants To Included :", value = F )
      )),
    shiny::fluidRow(
      shiny::column(width = 6,
             shiny::textInput(ns("excludedCovariateConceptIds"), "Excluded Covariate Concept Ids:", value = '' )
      ),
      shiny::column(width = 3,
             shiny::checkboxInput(ns("addDescendantsToExclude"), "Add Descendants To Exclude:", value = F )
      )
    ),
    shiny::fluidRow(
      shiny::column(width = 9,
             shiny::textInput(ns("includedCovariateIds"), "Included Covariate Ids:", value = '' )
      )
    )),

    footer = shiny::tagList(
      shiny::actionButton(ns('deleteStandard'), 'Delete'),
      shiny::actionButton(ns('updateStandard'), 'Update Standard')
    )
  )
}

covariateModuleCohort <- function(ns, settings, cohortReactive, ...) { # settings is list(fnct, settings)

  if(nrow(cohortReactive)>0){
    choices <- as.list(cohortReactive$id)
    names(choices ) <- cohortReactive$name
  } else {
    choices <- NULL
  }
  selectedId <- ifelse(is.null(settings$settings$cohortCovcohortId), 1, settings$settings$cohortCovcohortId)

  shiny::modalDialog(
    shiny::selectInput(inputId = ns('cohortCovcohortId'), label = 'Select covariate cohort: ',
                       choices = choices, selected = selectedId),
    #shiny::textInput(inputId = ns('cohortCovName'), label = 'Covariate name:'),
    shiny::numericInput(inputId = ns('cohortCovanalysisId'), label = 'analysisId (between 400 and 500): ', min=400, max = 500,
                        value = ifelse(is.null(settings$settings$cohortCovanalysisId), 457, settings$settings$cohortCovanalysisId)),
    shiny::numericInput(inputId = ns('cohortCovstartDay'), label = 'startDay: ',
                        value = ifelse(is.null(settings$settings$cohortCovstartDay), -9999, settings$settings$cohortCovstartDay)),
    shiny::numericInput(inputId = ns('cohortCovendDay'), label = 'endDay: ',
                        value = ifelse(is.null(settings$settings$cohortCovendDay), 0, settings$settings$cohortCovendDay)),
    shiny::checkboxInput(inputId = ns('cohortCovcount'), label = 'count:', value = ifelse(is.null(settings$settings$cohortCovcount),FALSE,settings$settings$cohortCovcount)),
    shiny::checkboxInput(inputId = ns('cohortCovageInteraction'), label = 'ageInteraction:', value = ifelse(is.null(settings$settings$cohortCovageInteraction),FALSE,settings$settings$cohortCovageInteraction)),
    shiny::checkboxInput(inputId = ns('cohortCovlnAgeInteraction'), label = 'lnAgeInteraction:', value = ifelse(is.null(settings$settings$cohortCovlnAgeInteraction),FALSE,settings$settings$cohortCovlnAgeInteraction)),

    footer = shiny::tagList(
      shiny::actionButton(ns('deleteCohort'), 'Delete'),
      shiny::actionButton(ns('updateCohort'), 'Update Cohort')
    )
  )
}


covariateModuleMeasurement <- function(ns, settings, ...) { # settings is list(fnct, settings)

  shiny::modalDialog(
    shiny::textInput(inputId = ns('measureCovcovariateName'), label = 'Covariate name:',
    value = ifelse(is.null(settings$settings$measureCovcovariateName),'Measurement X', settings$settings$measureCovcovariateName)),
    shiny::textInput(inputId = ns('measureCovconceptSet'), label = 'Concept set:',
                     value = ifelse(is.null(settings$settings$measureCovconceptSet),'Measurement X', settings$settings$measureCovconceptSet)),

    shiny::textInput(inputId = ns('measureCovscaleMap'), label = 'Scale map function written as text:',
                     value = ifelse(is.null(settings$settings$measureCovscaleMap),'function(x){return(x)}', settings$settings$measureCovscaleMap)),
    shiny::selectInput(inputId = ns('measureCovaggregateMethod'), label = 'Aggregate Method:',
                       choices = c('recent', 'max', 'min', 'mean', 'median'),
                       selected = ifelse(is.null(settings$settings$measureCovaggregateMethod), 'recent', settings$settings$measureCovaggregateMethod)
                         ),
    shiny::numericInput(inputId = ns('measureCovimputationValue'), label = 'imputationValue: ',
                        value = ifelse(is.null(settings$settings$measureCovimputationValue), 0, settings$settings$measureCovimputationValue)),

    shiny::numericInput(inputId = ns('measureCovstartDay'), label = 'startDay: ',
                        value = ifelse(is.null(settings$settings$measureCovstartDay), -9999, settings$settings$measureCovstartDay)),
    shiny::numericInput(inputId = ns('measureCovendDay'), label = 'endDay: ',
                        value = ifelse(is.null(settings$settings$measureCovendDay), 0, settings$settings$measureCovendDay)),

    shiny::checkboxInput(inputId = ns('measureCovageInteraction'), label = 'ageInteraction:', value = ifelse(is.null(settings$settings$measureCovageInteraction),FALSE,settings$settings$measureCovageInteraction)),
    shiny::checkboxInput(inputId = ns('measureCovlnAgeInteraction'), label = 'lnAgeInteraction:', value = ifelse(is.null(settings$settings$measureCovlnAgeInteraction),FALSE,settings$settings$measureCovlnAgeInteraction)),
    shiny::checkboxInput(inputId = ns('measureCovlnvalue'), label = 'lnValue:', value = ifelse(is.null(settings$settings$measureCovlnValue),FALSE,settings$settings$measureCovlnValue)),

    shiny::numericInput(inputId = ns('measureCovanalysisId'), label = 'analysisId (between 400 and 500): ', min=400, max = 500,
                        value = ifelse(is.null(settings$settings$measureCovanalysisId), 458, settings$settings$measureCovanalysisId)),

    footer = shiny::tagList(
      shiny::actionButton(ns('deleteMeasurement'), 'Delete'),
      shiny::actionButton(ns('updateMeasurement'), 'Update Measurement Covariate')
    )
  )
}

covariateModuleAge <- function(ns, settings, ...) { # settings is list(fnct, settings)

  shiny::modalDialog(
    shiny::textInput(inputId = ns('ageCovcovariateName'), label = 'Covariate name:',
                     value = ifelse(is.null(settings$settings$ageCovcovariateName),'Age X', settings$settings$ageCovcovariateName)),

    shiny::textInput(inputId = ns('ageCovageMap'), label = 'Age map function written as text:',
                     value = ifelse(is.null(settings$settings$ageCovageMap),'function(x){return(x)}', settings$settings$ageCovageMap)),

    shiny::numericInput(inputId = ns('ageCovanalysisId'), label = 'analysisId (between 400 and 500): ', min=400, max = 500,
                        value = ifelse(is.null(settings$settings$ageCovanalysisId), 459, settings$settings$ageCovanalysisId)),

    footer = shiny::tagList(
      shiny::actionButton(ns('deleteAge'), 'Delete'),
      shiny::actionButton(ns('updateAge'), 'Update Age Covariate')
    )
  )
}

covariateModalDelete <- function(ns) {
  shiny::modalDialog(

    # select the type of covariate to add to the covarite Name
    shiny::h1('Delete Covariate?'),

    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns('deleteCovariateSetting'), 'Yes')
    )
  )
}
