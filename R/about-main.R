# @file about-main.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
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
#' @family About
#' @return
#' string location of the about helper file
#'
#' @export
aboutHelperFile <- function() {
  fileLoc <-
    system.file('about-www', "about.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The module viewer for the shiny app home
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @family About
#' @return
#' The user interface to the home page module
#'
#' @export
aboutViewer <- function(id = 'homepage') {
  ns <- shiny::NS(id)

  shiny::div(

    # ---- Hero banner ---------------------------------------------------------
    shiny::div(
      style = paste0(
        "background: linear-gradient(135deg, #1a3a4a 0%, #1f6b8a 100%); ",
        "color: white; padding: 28px 32px 24px; border-radius: 8px; ",
        "margin-bottom: 24px; display: flex; align-items: center; gap: 20px; ",
        "flex-wrap: wrap;"
      ),
      shiny::div(
        style = paste0(
          "background: rgba(255,255,255,0.15); border-radius: 50%; ",
          "width: 64px; height: 64px; flex-shrink: 0; ",
          "display: flex; align-items: center; justify-content: center;"
        ),
        shiny::icon("chart-bar", style = "font-size: 26px; color: white;")
      ),
      shiny::div(
        shiny::h2(
          "OHDSI Results Explorer",
          style = "margin: 0 0 6px 0; font-size: 1.6em; font-weight: 700; color: white;"
        ),
        shiny::p(
          paste0(
            "An interactive viewer for standardized analysis results across ",
            "characterization, estimation, and prediction studies."
          ),
          style = "margin: 0; opacity: 0.88; font-size: 0.95em; line-height: 1.5; color: white;"
        )
      )
    ),

    # ---- How to use ----------------------------------------------------------
    shiny::div(
      style = "margin-bottom: 24px;",
      shiny::h4(
        shiny::icon("circle-question"), " How to use this app",
        style = "margin: 0 0 14px 0; color: #2c3e50; font-weight: 600;"
      ),
      shiny::div(
        style = "display: flex; gap: 14px; flex-wrap: wrap;",
        lapply(list(
          list(
            step  = "1",
            title = "Navigate",
            text  = "Select a module from the sidebar on the left to switch between analyses."
          ),
          list(
            step  = "2",
            title = "Filter & Select",
            text  = "Use the input controls on each page to choose cohorts, databases, and settings."
          ),
          list(
            step  = "3",
            title = "Explore Results",
            text  = "Review tables and plots. Use the Report module to download a formatted summary."
          )
        ), function(s) {
          shiny::div(
            style = paste0(
              "flex: 1; min-width: 200px; background: #f8fafc; ",
              "border-radius: 8px; padding: 14px 16px; ",
              "border-left: 4px solid #1f6b8a;"
            ),
            shiny::div(
              style = "display: flex; align-items: center; gap: 10px; margin-bottom: 7px;",
              shiny::div(
                style = paste0(
                  "background: #1f6b8a; color: white; border-radius: 50%; ",
                  "width: 26px; height: 26px; flex-shrink: 0; font-weight: 700; ",
                  "font-size: 0.82em; display: flex; align-items: center; justify-content: center;"
                ),
                s$step
              ),
              shiny::strong(s$title, style = "color: #2c3e50; font-size: 0.92em;")
            ),
            shiny::p(
              s$text,
              style = "margin: 0; font-size: 0.83em; color: #555; line-height: 1.45;"
            )
          )
        })
      )
    ),

    # ---- Module cards (filled by server) ------------------------------------
    shiny::div(
      shiny::h4(
        shiny::icon("layer-group"), " Analysis Modules",
        style = "margin: 0 0 14px 0; color: #2c3e50; font-weight: 600;"
      ),
      shiny::uiOutput(ns('moduleCards'))
    ),

    # ---- Footer --------------------------------------------------------------
    shiny::div(
      style = paste0(
        "margin-top: 28px; padding: 12px 16px; background: #f0f4f8; ",
        "border-radius: 6px; font-size: 0.81em; color: #666; line-height: 1.55;"
      ),
      shiny::HTML(paste0(
        "<strong>Resources:</strong> Full documentation for all analysis tools is available on the ",
        "<a href='https://ohdsi.github.io/Hades/' target='_blank'>HADES website</a>. ",
        "For help with this viewer, visit the ",
        "<a href='https://ohdsi.github.io/OhdsiShinyModules/' target='_blank'>",
        "OhdsiShinyModules documentation</a>."
      ))
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
#' @family About
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
  shiny::moduleServer(id, function(input, output, session) {

    # Metadata for every analysis module: shown as a card on the home page.
    # accentColor is used for the card's top border and the icon circle.
    moduleCardInfo <- list(
      list(
        tabName     = "DataSources",
        title       = "Data Sources",
        description = "Databases and data sources included in this study, with record and patient counts.",
        icon        = "database",
        accentColor = "#27ae60",
        docUrl      = "https://ohdsi.github.io/OhdsiShinyModules/articles/DataSources.html"
      ),
      list(
        tabName     = "Cohorts",
        title       = "Cohorts",
        description = "Cohort definitions used in this analysis, including inclusion rule diagnostics.",
        icon        = "user-gear",
        accentColor = "#8e44ad",
        docUrl      = "https://ohdsi.github.io/OhdsiShinyModules/articles/Cohorts.html"
      ),
      list(
        tabName     = "Characterization",
        title       = "Characterization",
        description = "Descriptive statistics and baseline characteristics for study populations.",
        icon        = "table",
        accentColor = "#c0392b",
        docUrl      = "https://ohdsi.github.io/OhdsiShinyModules/articles/Characterization.html"
      ),
      list(
        tabName     = "CohortDiagnostics",
        title       = "Cohort Diagnostics",
        description = "Diagnostics to evaluate cohort validity, concept sets, and temporal patterns.",
        icon        = "users",
        accentColor = "#16a085",
        docUrl      = "https://ohdsi.github.io/OhdsiShinyModules/articles/CohortDiagnostics.html"
      ),
      list(
        tabName     = "Estimation",
        title       = "Estimation",
        description = "Population-level effect estimation using comparative cohort and SCCS methods.",
        icon        = "chart-column",
        accentColor = "#1a3a8f",
        docUrl      = "https://ohdsi.github.io/OhdsiShinyModules/articles/Estimation.html"
      ),
      list(
        tabName     = "Prediction",
        title       = "Prediction",
        description = "Patient-level prediction model development and evaluation results.",
        icon        = "chart-line",
        accentColor = "#2980b9",
        docUrl      = "https://ohdsi.github.io/OhdsiShinyModules/articles/Prediction.html"
      ),
      list(
        tabName     = "Report",
        title       = "Report Generator",
        description = "Generate and download a formatted report summarizing key study results.",
        icon        = "book",
        accentColor = "#0e7fa8",
        docUrl      = "https://ohdsi.github.io/OhdsiShinyModules/articles/ReportGenerator.html"
      )
    )

    # Build a single card element for one module.
    # isActive: TRUE when this module's tabName appears in the app config.
    makeModuleCard <- function(mod, isActive) {
      accentColor <- if (isActive) mod$accentColor else "#bdc3c7"

      cardStyle <- paste0(
        "background: ", if (isActive) "#ffffff" else "#f7f8fa", "; ",
        "border-radius: 8px; overflow: hidden; ",
        "box-shadow: ", if (isActive) "0 1px 6px rgba(0,0,0,0.10)" else "none", "; ",
        "border-top: 4px solid ", accentColor, "; ",
        "display: flex; flex-direction: column; ",
        if (!isActive) "opacity: 0.65;" else ""
      )

      shiny::div(
        style = cardStyle,
        # Body
        shiny::div(
          style = "padding: 14px 16px 10px; flex: 1;",
          shiny::div(
            style = "display: flex; align-items: center; gap: 12px; margin-bottom: 9px;",
            shiny::div(
              style = paste0(
                "width: 36px; height: 36px; border-radius: 50%; flex-shrink: 0; ",
                "background: ", accentColor, "; ",
                "display: flex; align-items: center; justify-content: center;"
              ),
              shiny::icon(mod$icon, style = "color: white; font-size: 14px;")
            ),
            shiny::div(
              shiny::strong(
                mod$title,
                style = "font-size: 0.92em; color: #2c3e50; line-height: 1.25;"
              ),
              if (!isActive) {
                shiny::div(
                  "Not included",
                  style = paste0(
                    "font-size: 0.68em; font-weight: 700; color: #e74c3c; ",
                    "text-transform: uppercase; letter-spacing: 0.05em; margin-top: 2px;"
                  )
                )
              }
            )
          ),
          shiny::p(
            mod$description,
            style = "margin: 0; font-size: 0.81em; color: #555; line-height: 1.45;"
          )
        ),
        # Footer link
        shiny::div(
          style = "padding: 7px 16px; border-top: 1px solid #f0f0f0; background: #fafafa;",
          shiny::tags$a(
            href   = mod$docUrl,
            target = "_blank",
            style  = paste0(
              "font-size: 0.78em; font-weight: 600; text-decoration: none; color: ",
              accentColor, ";"
            ),
            "View documentation ",
            shiny::icon("arrow-up-right-from-square", style = "font-size: 0.8em;")
          )
        )
      )
    }

    # Collect which tabNames are active in this app's config
    activeTabNames <- character()
    for (i in seq_along(config[["shinyModules"]])) {
      activeTabNames <- c(activeTabNames, config[["shinyModules"]][[i]][["tabName"]])
    }

    output$moduleCards <- shiny::renderUI({
      shiny::div(
        style = paste0(
          "display: grid; ",
          "grid-template-columns: repeat(auto-fill, minmax(230px, 1fr)); ",
          "gap: 16px;"
        ),
        lapply(moduleCardInfo, function(mod) {
          makeModuleCard(mod, isActive = mod$tabName %in% activeTabNames)
        })
      )
    })

  })
}
