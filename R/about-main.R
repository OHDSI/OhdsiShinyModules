# @file about-main.R
#
# Copyright 2024 Observational Health Data Sciences and Informatics
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
aboutViewer <- function(
    id = 'homepage'
    ) {
  ns <- shiny::NS(id)
  
  #shinydashboard::dashboardBody(
    shiny::div(
      shiny::fluidRow(
      tags$head(tags$style(HTML(".small-box {height: 200px; width: 100%;}"))),
      shinydashboard::box(
        width = "100%",
        shiny::htmlTemplate(system.file("about-www", "about.html", package = utils::packageName()))
      )
   # )
  ),
  shiny::fluidRow(
    shinydashboard::valueBoxOutput(ns("newOrdersBox"), width = 3),
    shinydashboard::valueBoxOutput(ns("progressBox"), width = 3),
    shinydashboard::valueBoxOutput(ns("approvalBox"), width = 3),
    shinydashboard::valueBoxOutput(ns("valueBox1"), width = 3)
  ),
  shiny::fluidRow(
    shinydashboard::valueBoxOutput(ns("valueBox2"), width = 3),
    shinydashboard::valueBoxOutput(ns("valueBox3"), width = 3),
    shinydashboard::valueBoxOutput(ns("valueBox4"), width = 3),
    shinydashboard::valueBoxOutput(ns("valueBox5"), width = 3)
  ),
  tags$script(HTML('
  $(document).ready(function(){
    $("#newOrdersBox").click(function(){
      $("#newOrdersModal").modal("show");
    });

    $("#progressBox").click(function(){
      $("#progressModal").modal("show");
    });

    $("#approvalBox").click(function(){
      $("#approvalModal").modal("show");
    });

    $(".value-box").click(function() {
      var modalId = $(this).attr("data-modal-id");
      $("#" + modalId).modal("show");
    });
  });
')),
  tags$div(
    id = "newOrdersModal",
    class = "modal fade",
    tags$div(
      class = "modal-dialog modal-dialog-centered",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          tags$h3(
            class = "modal-title",
            "New Orders Modal"
          ),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Close",
            tags$span(`aria-hidden` = "true", "×")
          )
        ),
        tags$div(
          class = "modal-body",
          "This is the New Orders modal."
        )
      )
    )
  ),
  tags$div(
    id = "progressModal",
    class = "modal fade",
    tags$div(
      class = "modal-dialog modal-dialog-centered",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          tags$h3(
            class = "modal-title",
            "Progress Modal"
          ),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Close",
            tags$span(`aria-hidden` = "true", "×")
          )
        ),
        tags$div(
          class = "modal-body",
          "This is the Progress modal."
        )
      )
    )
  ),
  tags$div(
    id = "approvalModal",
    class = "modal fade",
    tags$div(
      class = "modal-dialog modal-dialog-centered",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          tags$h3(
            class = "modal-title",
            "Approval Modal"
          ),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Close",
            tags$span(`aria-hidden` = "true", "×")
          )
        ),
        tags$div(
          class = "modal-body",
          "This is the Approval modal."
        )
      )
    )
  ),
  tags$div(
    id = "modal1",
    class = "modal fade",
    tags$div(
      class = "modal-dialog modal-dialog-centered",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          tags$h3(
            class = "modal-title",
            "Modal 1"
          ),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Close",
            tags$span(`aria-hidden` = "true", "×")
          )
        ),
        tags$div(
          class = "modal-body",
          "This is Modal 1."
        )
      )
    )
  ),
  tags$div(
    id = "modal2",
    class = "modal fade",
    tags$div(
      class = "modal-dialog modal-dialog-centered",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          tags$h3(
            class = "modal-title",
            "Modal 2"
          ),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Close",
            tags$span(`aria-hidden` = "true", "×")
          )
        ),
        tags$div(
          class = "modal-body",
          "This is Modal 2."
        )
      )
    )
  ),
  tags$div(
    id = "modal3",
    class = "modal fade",
    tags$div(
      class = "modal-dialog modal-dialog-centered",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          tags$h3(
            class = "modal-title",
            "Modal 3"
          ),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Close",
            tags$span(`aria-hidden` = "true", "×")
          )
        ),
        tags$div(
          class = "modal-body",
          "This is Modal 3."
        )
      )
    )
  ),
  tags$div(
    id = "modal4",
    class = "modal fade",
    tags$div(
      class = "modal-dialog modal-dialog-centered",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          tags$h3(
            class = "modal-title",
            "Modal 4"
          ),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Close",
            tags$span(`aria-hidden` = "true", "×")
          )
        ),
        tags$div(
          class = "modal-body",
          "This is Modal 4."
        )
      )
    )
  ),
  tags$div(
    id = "modal5",
    class = "modal fade",
    tags$div(
      class = "modal-dialog modal-dialog-centered",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          tags$h3(
            class = "modal-title",
            "Modal 5"
          ),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Close",
            tags$span(`aria-hidden` = "true", "×")
          )
        ),
        tags$div(
          class = "modal-body",
          "This is Modal 5."
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
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a list containing the characterization result schema, dbms, tablePrefix, databaseTable and cgTablePrefix
#' @param config the config from the app.R file that contains a list of which modules to include
#' 
#' @return
#' The server for the shiny app home
#'
#' @export
aboutServer <- function(
    id = 'homepage',
    connectionHandler = NULL,
    resultDatabaseSettings = NULL,
    config
    ) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      tab_names <- character()
      # Loop through shinyModules and extract tabName values
      for (i in seq_along(config[["shinyModules"]])) {
        tab_name <- config[["shinyModules"]][[i]][["tabName"]]
        tab_names <- c(tab_names, tab_name)
      }
      # View the extracted tabName values
      # print(tab_names)

      output$newOrdersBox <- shinydashboard::renderValueBox({
      if ("DataSources" %in% tab_names) {
        shinydashboard::valueBox(
          "Data Sources", "Databases used in this analysis", icon = shiny::icon("database"),
          color = "aqua",
          href = "#newOrdersModal"
        )
      } else {
        shinydashboard::valueBox(
          "Data Sources", "This module was not included in this analysis.", icon = shiny::icon("database"),
          color = "black",
          href = "#newOrdersModal"
        )
      }
      })
      
      output$progressBox <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          "100%", "Progress", icon = shiny::icon("list"),
          color = "purple",
          href = "#progressModal"
        )
      })
      
      output$approvalBox <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          "80%", "Approval", icon = shiny::icon("thumbs-up", lib = "glyphicon"),
          color = "teal",
          href = "#approvalModal"
        )
      })
      
      output$valueBox1 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          "Text 1", "Value Box 1", icon = shiny::icon("star"),
          color = "yellow",
          href = "#modal1"
        )
      })
      
      output$valueBox2 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          "Text 2", "Value Box 2", icon = shiny::icon("heart"),
          color = "maroon",
          href = "#modal2"
        )
      })
      
      output$valueBox3 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          "Text 3", "Value Box 3", icon = shiny::icon("globe"),
          color = "blue",
          href = "#modal3"
        )
      })
      
      output$valueBox4 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          "Text 4", "Value Box 4", icon = shiny::icon("car"),
          color = "red",
          href = "#modal4"
        )
      })
      
      output$valueBox5 <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          "Text 5", "Value Box 5", icon = shiny::icon("car"),
          color = "olive",
          href = "#modal4"
        )
      })
      
      
    }
  )
}
