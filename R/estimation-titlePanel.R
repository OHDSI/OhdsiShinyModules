# @file estimation-titlePanel
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


#' The module viewer for rendering the estimation title
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the estimation title module
#' 
#' @export
estimationTitlePanelViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("titleText"))
}


#' The module server for rendering the estimation title
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The server for the estimation title server
#' 
#' @export
estimationTitlePanelServer <- function(id) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$titleText <- renderUI({
        titlePanel("Cohort Method Evidence Explorer")
      })
      
    }
  )
}
