# @file cohortExtractModule.R
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

#' The user interface for extracting cohorts from an ATLAS webapi
#' @param id   The module identifier
#'
#' @examples
#' \dontrun{
#' extractCohortsViewer('extractCohort1')
#' }
#' @export
extractCohortsViewer <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(id = ns(id),
             shiny::actionButton(inputId = ns('extractCohorts'),
                                 label = 'Load Cohorts')

  )
}

#' The server for extracting cohorts from an ATLAS webapi
#' @param input   Standard for shiny modules
#' @param output  Standard for shiny modules
#' @param session Standard for shiny modules
#' @param webApi The webApi locations (output from the webApiServer module)
#'
#' @examples
#' \dontrun{
#' webApi <- callModule(webApiServer, 'webApiMain')
#' cohortReactive <- callModule(extractCohortsServer, 'extractCohort1',
#'                             webApi = webApi)
#' }
#' @export
extractCohortsServer <- function(input, output, session, webApi) {

  cohortReactive <- shiny::reactiveVal(data.frame())

  shiny::observeEvent(input$extractCohorts, {
    if(webApi() != ''){
      cohorts <- ROhdsiWebApi::getCohortDefinitionsMetaData(webApi())[,c('id', 'name')]
      cohortReactive(as.data.frame(cohorts))
      shiny::showNotification(paste("Cohort Extracted"), duration = 0, type = 'message')

    } else{
      cohortReactive(data.frame())
      shiny::showNotification(paste("Cohort Extraction Failed As No WebApi Connection"), duration = 0, type = 'error')
    }
  })

  return(cohortReactive)
}
