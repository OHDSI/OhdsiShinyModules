# @file webApiModule.R
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

#' The user interface for connecting to an ATLAS webapi
#' @param id   The module identifier
#'
#' @examples
#' \dontrun{
#' webApiViewer('webApi1')
#' }
#' @export
webApiViewer <- function(id) {
  ns <- shiny::NS(id)

                      shiny::div(id = ns(id),

                      shiny::textInput(inputId = ns('baseUrl'),
                                       label = shiny::textOutput(ns('baseUrlCheck')) ,
                                       placeholder = 'http://api.ohdsi.org:8080/WebAPI',
                                       value = 'http://api.ohdsi.org:8080/WebAPI'),

                      shiny::checkboxInput(inputId = ns('authorizeWebApi'),
                                           label = 'requires authorization',
                                           value = FALSE),

                      shiny::conditionalPanel(condition = "input.authorizeWebApi", ns = ns,
                                             shiny::selectInput(inputId = ns("authMethod"),
                                                                label = "authMethod",
                                                                multiple = F,
                                                                choices = list(db='db',
                                                                               ad= 'ad',
                                                                               windows = 'windows'),
                                                                selected = 'Development'),
                                             shiny::textInput(inputId = ns('webApiUsername'),
                                                              label = 'webApiUsername' ,
                                                              placeholder = 'username'),
                                             shiny::passwordInput(inputId = ns('webApiPassword'),
                                                                  label = 'webApiPassword',
                                                                  value = 'password')

                      ),

                      shiny::actionButton(inputId = ns('connectwebApi'),
                                          label = 'Connect to webApi')

  )
}

#' The server for connecting to an ATLAS webapi
#' @param input   Standard for shiny modules
#' @param output  Standard for shiny modules
#' @param session Standard for shiny modules
#'
#' @examples
#' \dontrun{
#' webApi <- callModule(webApiServer, 'webApi1')
#' }
#' @export
webApiServer <- function(input, output, session) {

    webApi <- shiny::reactiveVal('')
    baseUrlCheck <- shiny::reactiveVal('WebAPI: ')
    output$baseUrlCheck <- shiny::renderText(baseUrlCheck())

    shiny::observeEvent(input$connectwebApi, {

      if(!is.null(input$baseUrl)){

        # check connection
        if(input$authorizeWebApi){
          response <- tryCatch({ROhdsiWebApi::authorizeWebApi(baseUrl = input$baseUrl,
                                                              authMethod = input$authMethod,
                                                              webApiUsername = input$webApiUsername,
                                                              webApiPassword = input$webApiPassword
                                                                )},
                               error = function(e){print(e);return('fail')})
          if(!is.null(response)){
            response <- NULL
          }else{
            response <- 'connect'
          }

        } else{
          url <- paste0(input$baseUrl, "/info")
          response <- tryCatch({httr::GET(url)},
                               error = function(e){print(e);return(NULL)})
        }


        if(!is.null(response)){
          baseUrlCheck('WebAPI: (Connected)')
          shiny::showNotification(paste("WebAPI connection works..."), duration = 0, type = 'message')
          webApi(input$baseUrl)
        } else{
          baseUrlCheck('WebAPI:')
          shiny::showNotification(paste("WebAPI input did not connect"), duration = 0, type = 'error')
          webApi('')
        }
      }else{
        shiny::showNotification(paste("Missing WebAPI"), duration = 0, type = 'error')
      }
    })

    return(webApi)
  }



