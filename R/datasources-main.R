# @file datasources-main.R
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




#' Define the helper file for the module
#'
#' @return The helper html file for the datasources module
#' @family Utils
#' 
#' @export
datasourcesHelperFile <- function() {
  fileLoc <-
    system.file('datasources-www', "datasources.html", package = "OhdsiShinyModules")
  return(fileLoc)
}



#' The viewer function for hte datasources module
#'
#' @param id The unique id for the datasources viewer namespace
#'
#' @return The UI for the datasources module
#' @family Utils
#' 
#' @export
datasourcesViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    status = 'info',
    width = "100%",
    title =  shiny::span(shiny::icon("database"), "Data Sources"),
    solidHeader = TRUE,
    
    shiny::tags$style(
      '
      .ds-search-box {
        margin-bottom: 20px;
      }
      .ds-search-input-wrapper {
        position: relative;
        max-width: 100%;
      }
      .ds-search-input {
        width: 100%;
        padding: 12px 16px 12px 40px;
        border: 1px solid #d4e0ed;
        border-radius: 10px;
        font-size: 14px;
        background: #ffffff;
        box-shadow: 0 2px 8px rgba(15, 23, 42, 0.04);
        transition: border-color 0.2s ease, box-shadow 0.2s ease;
      }
      .ds-search-input:focus {
        outline: none;
        border-color: #2563eb;
        box-shadow: 0 4px 12px rgba(37, 99, 235, 0.12);
      }
      .ds-search-input::placeholder {
        color: #94a3b8;
      }
      .ds-search-icon {
        position: absolute;
        left: 12px;
        top: 50%;
        transform: translateY(-50%);
        color: #94a3b8;
        pointer-events: none;
        font-size: 14px;
      }
      .ds-container {
        display: grid;
        gap: 16px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
        box-sizing: border-box;
      }
      .ds-no-results {
        text-align: center;
        padding: 40px 20px;
        color: #64748b;
      }
      .ds-no-results-icon {
        font-size: 40px;
        color: #cbd5e1;
        margin-bottom: 12px;
      }
      .ds-no-results-text {
        font-size: 16px;
        font-weight: 500;
      }
      .ds-no-results-subtext {
        font-size: 13px;
        color: #94a3b8;
        margin-top: 6px;
      }
      .ds-card {
        border-radius: 16px;
        border: 1px solid #dbe5f1;
        background: linear-gradient(135deg, #ffffff 0%, #f8fbff 100%);
        box-shadow: 0 10px 24px rgba(15, 23, 42, 0.08);
        padding: 24px;
        width: 100%;
        max-width: 100%;
        min-width: 0;
        transition: transform 0.15s ease, box-shadow 0.15s ease;
      }
      .ds-card:hover {
        transform: translateY(-1px);
        box-shadow: 0 14px 32px rgba(15, 23, 42, 0.12);
      }
      .ds-card-header {
        display: flex;
        align-items: center;
        gap: 14px;
        margin-bottom: 20px;
        border-bottom: 2px solid #e5ecf6;
        padding-bottom: 16px;
      }
      .ds-card-icon {
        flex: 0 0 auto;
        width: 48px;
        height: 48px;
        border-radius: 12px;
        display: flex;
        align-items: center;
        justify-content: center;
        background: linear-gradient(135deg, #0f766e, #14b8a6);
        color: #ffffff;
        font-size: 24px;
      }
      .ds-card-title {
        font-size: 18px;
        font-weight: 700;
        color: #132238;
        margin: 0;
      }
      .ds-card-subtitle {
        font-size: 13px;
        color: #64748b;
        margin: 4px 0 0 0;
      }
      .ds-card-content {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
        gap: 20px;
      }
      .ds-field {
        display: flex;
        flex-direction: column;
        gap: 6px;
      }
      .ds-field-label {
        font-size: 12px;
        font-weight: 700;
        color: #64748b;
        text-transform: uppercase;
        letter-spacing: 0.05em;
      }
      .ds-field-value {
        font-size: 14px;
        color: #132238;
        line-height: 1.5;
        word-break: break-word;
      }
      .ds-field-value.description {
        font-size: 13px;
        color: #556271;
        line-height: 1.6;
      }
      .ds-field-links {
        display: flex;
        gap: 12px;
        flex-wrap: wrap;
      }
      .ds-link {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 6px 12px;
        border-radius: 8px;
        background: #e5ecf6;
        color: #2563eb;
        text-decoration: none;
        font-size: 12px;
        font-weight: 600;
        transition: background 0.15s ease;
      }
      .ds-link:hover {
        background: #d4e0ed;
        text-decoration: none;
      }
      .ds-link-disabled {
        color: #94a3b8;
        background: #f1f5f9;
        cursor: not-allowed;
      }
      .ds-divider {
        height: 1px;
        background: #e5ecf6;
        margin: 16px 0;
      }
      .ds-meta-row {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
        gap: 12px;
        padding-top: 12px;
        border-top: 1px solid #f1f5f9;
      }
      .ds-meta-item {
        display: flex;
        flex-direction: column;
        gap: 4px;
      }
      .ds-meta-label {
        font-size: 11px;
        font-weight: 600;
        color: #94a3b8;
        text-transform: uppercase;
      }
      .ds-meta-value {
        font-size: 13px;
        color: #475569;
        font-weight: 500;
      }
      '
    ),
    
    shiny::div(
      class = 'ds-container',
      shiny::div(
        class = 'ds-search-box',
        shiny::div(
          class = 'ds-search-input-wrapper',
          shiny::tags$input(
            type = 'text',
            id = ns('dbSearch'),
            class = 'ds-search-input',
            placeholder = 'Search databases by name...'
          ),
          shiny::div(class = 'ds-search-icon', shiny::icon('search'))
        )
      ),
      shiny::uiOutput(ns('databaseCards'))
    )
  )
}





#' The server function for the datasources module
#'
#' @param id The unique id for the datasources server namespace
#' @param connectionHandler A connection to the database with the results
#' @param resultDatabaseSettings A named list containing the cohort generator results database details (schema, table prefix)
#'
#' @return The server for the datasources module
#' @family Utils
#' 
#' @export
datasourcesServer <- function(
  id, 
  connectionHandler, 
  resultDatabaseSettings
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      datasourcesData <- shiny::reactive({
        getDatasourcesData(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings
        )
      })
      
      # Filter data based on search input
      filteredData <- shiny::reactive({
        data <- datasourcesData()
        searchTerm <- input$dbSearch
        
        if (is.null(searchTerm) || searchTerm == '') {
          return(data)
        }
        
        # Case-insensitive search on database full name and abbreviation
        searchPattern <- tolower(searchTerm)
        mask <- grepl(searchPattern, tolower(data$databaseFullName), fixed = TRUE) |
                grepl(searchPattern, tolower(data$databaseName), fixed = TRUE)
        
        return(data[mask, , drop = FALSE])
      })

      output$databaseCards <- shiny::renderUI({
        data <- filteredData()
        
        if (is.null(data) || nrow(data) == 0) {
          if (input$dbSearch != '') {
            return(
              shiny::div(
                class = 'ds-no-results',
                shiny::div(class = 'ds-no-results-icon', shiny::icon('search')),
                shiny::div(class = 'ds-no-results-text', 'No databases found'),
                shiny::div(
                  class = 'ds-no-results-subtext',
                  paste0('No databases match "', input$dbSearch, '"')
                )
              )
            )
          } else {
            return(shiny::helpText("No data sources available."))
          }
        }
        
        lapply(1:nrow(data), function(i) {
          db <- data[i, , drop = FALSE]
          
          shiny::div(
            class = 'ds-card',
            # Header with database name
            shiny::div(
              class = 'ds-card-header',
              shiny::div(
                class = 'ds-card-icon',
                shiny::icon('database')
              ),
              shiny::div(
                shiny::tags$h3(class = 'ds-card-title', db$databaseFullName[1]),
                shiny::tags$p(class = 'ds-card-subtitle', paste0('DB ID: ', db$databaseId[1]))
              )
            ),
            
            # Main content
            shiny::div(
              class = 'ds-card-content',
              
              # Database Name and Holder
              shiny::div(
                class = 'ds-field',
                shiny::div(class = 'ds-field-label', 'Database Abbreviation'),
                shiny::div(class = 'ds-field-value', db$databaseName[1])
              ),
              
              shiny::div(
                class = 'ds-field',
                shiny::div(class = 'ds-field-label', 'Database Holder'),
                shiny::div(class = 'ds-field-value', db$cdmHolder[1])
              ),
              
              # Description
              shiny::div(
                class = 'ds-field',
                style = 'grid-column: 1 / -1;',
                shiny::div(class = 'ds-field-label', 'Description'),
                shiny::div(
                  class = 'ds-field-value description',
                  db$sourceDescription[1]
                )
              )
            ),
            
            # Links section
            if (!is.na(db$sourceDocumentationReference[1]) || !is.na(db$cdmEtlReference[1])) {
              list(
                shiny::div(class = 'ds-divider'),
                shiny::div(
                  class = 'ds-field',
                  style = 'margin-bottom: 12px;',
                  shiny::div(class = 'ds-field-label', 'References'),
                  shiny::div(
                    class = 'ds-field-links',
                    if (!is.na(db$sourceDocumentationReference[1]) && db$sourceDocumentationReference[1] != 'None') {
                      shiny::a(
                        class = 'ds-link',
                        href = db$sourceDocumentationReference[1],
                        target = '_blank',
                        rel = 'noopener noreferrer',
                        shiny::icon('external-link-alt'),
                        'Documentation'
                      )
                    },
                    if (!is.na(db$cdmEtlReference[1]) && db$cdmEtlReference[1] != 'None') {
                      shiny::a(
                        class = 'ds-link',
                        href = db$cdmEtlReference[1],
                        target = '_blank',
                        rel = 'noopener noreferrer',
                        shiny::icon('external-link-alt'),
                        'ETL Reference'
                      )
                    }
                  )
                )
              )
            },
            
            # Metadata row
            shiny::div(
              class = 'ds-meta-row',
              
              shiny::div(
                class = 'ds-meta-item',
                shiny::div(class = 'ds-meta-label', 'Source Release'),
                shiny::div(class = 'ds-meta-value', db$sourceReleaseDate[1])
              ),
              
              shiny::div(
                class = 'ds-meta-item',
                shiny::div(class = 'ds-meta-label', 'CDM Release'),
                shiny::div(class = 'ds-meta-value', db$cdmReleaseDate[1])
              ),
              
              shiny::div(
                class = 'ds-meta-item',
                shiny::div(class = 'ds-meta-label', 'CDM Version'),
                shiny::div(class = 'ds-meta-value', db$cdmVersion[1])
              ),
              
              shiny::div(
                class = 'ds-meta-item',
                shiny::div(class = 'ds-meta-label', 'Vocabulary Version'),
                shiny::div(class = 'ds-meta-value', db$vocabularyVersion[1])
              ),
              
              shiny::div(
                class = 'ds-meta-item',
                shiny::div(class = 'ds-meta-label', 'Max Obs. Period'),
                shiny::div(class = 'ds-meta-value', db$maxObsPeriodEndDate[1])
              )
            )
          )
        })
      })
      
      return(invisible(NULL))
    })
}


#pull database meta data table
getDatasourcesData <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  result <- OhdsiReportGenerator::getDatabaseDetails(
    connectionHandler = connectionHandler,
    schema = resultDatabaseSettings$schema,
    databaseTable = resultDatabaseSettings$databaseTable
  )
  
  return(result)
}


datasourcesColList <- function(){
  
  result <- list(
    databaseFullName = reactable::colDef(
      name = "Full DB Name", 
      header = withTooltip(
        "Full DB Name",
        "Name of the database (DB)"
      )
      ),
    databaseName = reactable::colDef(
      name = "DB Name", 
      header = withTooltip(
        "DB Name",
        "Abbreviation for the database (DB)"
      )
    ),
    cdmHolder = reactable::colDef(
      name = "DB Holder", 
      header = withTooltip(
        "DB Holder",
        "Holder of the database (DB)"
      )
    ),
    sourceDescription = reactable::colDef(
      name = "DB Description", 
      minWidth = 500,
      header = withTooltip(
        "DB Description",
        "Description of the database (DB)"
      )
    ),
    sourceDocumentationReference = reactable::colDef(
      name = "DB Description Link", 
      header = withTooltip(
        "DB Description Link",
        "HTML link to the database (DB) description"
      ),
      html = TRUE, 
      cell = function(value, index){
        if(value != 'None'){
          shiny::tagList(
            shiny::a("RHEALTH Description", href = value, target = "_blank")
          )
        } else{
          'No link available'
        }
      }
    ),

    cdmEtlReference = reactable::colDef(
      name = "DB ETL Link", 
      header = withTooltip(
        "DB ETL Link",
        "HTML link to the ETL for the database (DB)"
      ),
      html = TRUE, 
      cell = function(value, index){
        if(value != 'None'){
          shiny::tagList(
            shiny::a("RHEALTH Description", href = value, target = "_blank")
          )
        } else{
          'No link available'
        }
      }
    ),
    sourceReleaseDate = reactable::colDef(
      name = "Source Data Release Date", 
      header = withTooltip(
        "Source Data Release Date",
        "Date the source data was released"
      )
    ),
    cdmReleaseDate = reactable::colDef(
      name = "CDM DB Release Date", 
      header = withTooltip(
        "CDM DB Release Date",
        "Date the CDM database (DB) was accessible"
      )
    ),
    cdmVersion = reactable::colDef(
      name = "CDM Version", 
      header = withTooltip(
        "CDM Version",
        "Version of the common data model (CDM)"
      )
    ),
    cdmVersionConceptId = reactable::colDef(
      show = FALSE
    ),
    vocabularyVersion = reactable::colDef(
      name = "Vocabulary Version", 
      header = withTooltip(
        "Vocabulary Version",
        "Version of the vocabulary used in the database (DB)"
      )
    ),
    maxObsPeriodEndDate = reactable::colDef(
      name = "Max Obs. Period End Date", 
      header = withTooltip(
        "Max Obs. Period End Date",
        "Maximum/Latest observation period date in the database (DB)"
      )
    ),
    databaseId = reactable::colDef(
        name = "DB ID", 
        header = withTooltip(
          "DB ID",
          "Unique identifier (ID) of the database (DB)"
        )
      )
  )
  
  
  return(result)
}

