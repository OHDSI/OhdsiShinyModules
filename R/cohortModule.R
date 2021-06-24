# @file cohortModule.R
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

#' The user interface for viewing and extracting cohorts from an ATLAS webapi
#' @param id   The module identifier
#' @param labelv A string that is the header for the box containing the dropdown selector that lets you pick the cohort of interest into a list
#' @examples
#' \dontrun{
#' cohortViewer('cohort1', 'Target Cohort')
#' }
#' @export
cohortViewer <- function(id, labelv = "cohorts") {
  ns <- shiny::NS(id)
  shinydashboard::box(title = labelv, width = '60%',

                      # add table with existing models

                      shiny::dataTableOutput(ns('cohortTable')),

                      shiny::uiOutput(ns('cohortSelect')),
                      shiny::textInput(inputId = ns('cohortFilter'),
                                       label = 'Filter: ', value = ''),

                      shiny::actionButton(inputId = ns('addCohort'),
                                          label = paste0('Add ', labelv))
  )
}

#' The server for extracting cohorts from an ATLAS webapi
#' @param input   Standard for shiny modules
#' @param output  Standard for shiny modules
#' @param session Standard for shiny modules
#' @param cohortReactive The list of cohorts from the webApi (extracted using the extractCohortsServer module)
#'
#' @examples
#' \dontrun{
#' cohortReactive <- callModule(extractCohortsServer, 'cohortExtract', webApi = webApi)
#' targetList <- callModule(cohortServer, 'cohort1', cohortReactive)
#' }
#' @export
cohortServer <- #function(id) {
  #moduleServer(
  #id,
  function(input, output, session,
           cohortReactive) {

    # create the dropdown
    output$cohortSelect = shiny::renderUI({

      if(nrow(cohortReactive())>0){
        cohortVar <- as.list(cohortReactive()$id)
        names(cohortVar) <- paste0(cohortReactive()$id, ': ', cohortReactive()$name)

        if(input$cohortFilter!=''){
          ind <- grep(input$cohortFilter, names(cohortVar))
        }else{
          ind <- 1:length(cohortVar)
        }

        shiny::selectizeInput(inputId = session$ns('cohort'),
                              label = 'Cohorts',
                              choices = cohortVar[ind])
      }

    })

    cohortList <- shiny::reactiveVal(data.frame())

    shiny::observeEvent(input$addCohort, {

      if(nrow(cohortReactive())>0){
        ind <- cohortReactive()$id == input$cohort
        oldList <- cohortList()

        if(!cohortReactive()$id[ind] %in% oldList$Id){
          oldList <- rbind(oldList,
                           data.frame(Name = cohortReactive()$name[ind],
                                      Id = cohortReactive()$id[ind],
                                      Remove = as.character(shiny::actionButton(paste0('button_', cohortReactive()$id[ind]),label = "Delete", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("delete_button"))))
                           )
          )
          cohortList(oldList)
        }
      } else{
        shiny::showNotification('Need to connect to a valid webApi to fetch the cohorts', duration = 5, type = 'error')
      }
    })

    output$cohortTable <- shiny::renderDataTable({
      if(nrow(cohortList())>0){
        cohortList()
      } else{
        NULL
      }
    }, escape = FALSE)


    # DELETE COV
    #=================
    selectedRow <- shiny::reactiveVal()
    # if clicked popup asks for confirmation and then deletes covariate setting
    shiny::observeEvent(input$delete_button, {
      selectedRow(as.numeric(strsplit(input$delete_button, "_")[[1]][2]))
      #print(selectedRow())
      shiny::showModal(cohortModalDelete(session$ns))
    })
    shiny::observeEvent(input$deleteCohort, {
      indId <- selectedRow()
      oldList <- cohortList()
      oldList <- oldList[oldList$Id!=indId,]
      cohortList(oldList)
      shiny::removeModal()
    })
    #=================


    return(cohortList)
  }

cohortModalDelete <- function(ns) {
  shiny::modalDialog(
    shiny::h1('Delete Cohort?'),

    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns('deleteCohort'), 'Yes')
    )
  )
}
