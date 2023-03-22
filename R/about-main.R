# @file about-main.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
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


#' The location of the about module helper file
#'
#' @details
#' Returns the location of the about helper file
#' 
#' @return
#' string location of the about helper file
#'
#' @export
aboutHelperFile <- function(){
  fileLoc <- system.file('about-www', "about.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The module viewer for the shiny app home
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the home page module
#'
#' @export
aboutViewer <- function(id = 'homepage') {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    status = 'info', width = 12,
    title =  shiny::span( shiny::icon("info"), "About OHDSI Viewer"),
    solidHeader = TRUE,

    shiny::fluidPage(
      shiny::fluidRow(
        shiny::includeMarkdown(
          path = system.file(
            'about-document', 
            "introduction.md", 
            package = "OhdsiShinyModules"
          )
        )
      )
    )
    
  )
}

#' The module server for the shiny app home
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The server for the shiny app home
#'
#' @export
aboutServer <- function(id = 'homepage') {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
    }
  )
}
