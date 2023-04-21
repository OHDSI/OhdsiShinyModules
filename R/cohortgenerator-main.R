# @file cohortgenerator-main.R
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


#' The location of the cohort-generator module helper file
#'
#' @details
#' Returns the location of the cohort-generator helper file
#' 
#' @return
#' string location of the cohort-generator helper file
#'
#' @export
cohortGeneratorHelperFile <- function(){
  fileLoc <- system.file('cohort-generator-www', "cohort-generator.html", package = "OhdsiShinyModules")
  return(fileLoc)
}

#' The viewer of the main cohort generator module
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the cohort generator results viewer
#' 
#' @export
cohortGeneratorViewer <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::div(
    
    shinydashboard::box(
      status = 'info', 
      width = '100%',
      title = shiny::span( shiny::icon("user-gear"),'Cohort Generator Viewer'),
      solidHeader = TRUE,
      
      shinydashboard::box(
        collapsible = TRUE,
        collapsed = TRUE,
        title = shiny::span( shiny::icon("circle-question"), "Help & Information"),
        width = "100%",
        shiny::htmlTemplate(system.file("cohort-generator-www", "cohort-generator.html", package = utils::packageName()))
      ),
      
    
    shiny::tabsetPanel(
      id = ns("cohortGeneratorTabs"),
      type = "pills",
      
      
      shiny::tabPanel(
        title = "Cohort Counts",
        
        shinydashboard::box(
          status = 'info', 
          width = '100%',
          title = shiny::span( shiny::icon("file-arrow-down"),'Download Data'),
          solidHeader = TRUE,
          
          shiny::downloadButton(
            ns('downloadCohortCounts'),
            label = "Download",
            icon = shiny::icon("download")
          )
        ),
        
        shinydashboard::box(
          status = 'info', 
          width = '100%',
          title = shiny::span( shiny::icon("table"), 'Counts Table'),
          solidHeader = TRUE,
          
          reactable::reactableOutput(
            outputId = ns("cohortCounts")
          )  
        )
        # ,
        # shiny::downloadButton(
        #   ns('downloadCohortCounts'),
        #   label = "Download"
        # )
      ),
      
      shiny::tabPanel(
        title = "Cohort Generation",
        
        shinydashboard::box(
          status = 'info', 
          width = '100%',
          title = shiny::span( shiny::icon("file-arrow-down"),'Download Data'),
          solidHeader = TRUE,
          
          shiny::downloadButton(
            ns('downloadCohortGeneration'),
            label = "Download",
            icon = shiny::icon("download")
          )
        ),
        
        shinydashboard::box(
          status = 'info', 
          width = '100%',
          title = shiny::span( shiny::icon("table"), 'Generation Table'),
          solidHeader = TRUE,
          
          reactable::reactableOutput(
            outputId = ns("cohortGeneration")
            )  
          )
      ),
      
      shiny::tabPanel(
        title = "Inclusion Rules & Attrition"
        ,

        shinydashboard::box(
          status = 'info', 
          width = '100%',
          title = shiny::span( shiny::icon("gear"), 'Options'),
          solidHeader = TRUE,
          
          shiny::uiOutput(ns('attritionTableSelect'))
          ),
        
        shinydashboard::box(
          status = 'info', 
          width = '100%',
          title = shiny::span( shiny::icon("file-arrow-down"),'Download Data'),
          solidHeader = TRUE,
          
          shiny::downloadButton(
            ns('downloadAttritionTable'),
            label = "Download",
            icon = shiny::icon("download")
          )
        ),
        
        shinydashboard::box(
          status = 'info', 
          width = '100%',
          title = shiny::span( shiny::icon("table"), 'Attrition Table'),
          solidHeader = TRUE,
          
          reactable::reactableOutput(ns('attritionTable'))
        ),
        
        shinydashboard::box(
          status = 'info', 
          width = '100%',
          title = shiny::span( shiny::icon("chart-area"), 'Attrition Plot'),
          solidHeader = TRUE,
          
          plotly::plotlyOutput(ns('attritionPlot'))
        )
      )
    )
   )
  )
}




#' The module server for the main cohort generator module
#'
#' @param id the unique reference id for the module
#' @param connectionHandler a connection to the database with the results
#' @param resultDatabaseSettings a named list containing the cohort generator results database details (schema, table prefix)
#'
#' @return
#' the cohort generator results viewer main module server
#' 
#' @export

cohortGeneratorServer <- function(
  id, 
  connectionHandler, 
  resultDatabaseSettings
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      withTooltip <- function(value, tooltip, ...) {
        shiny::div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
                   tippy::tippy(value, tooltip, ...))
      }
      
      format_yesorno <- function(value) {
        # Render as an X mark or check mark
        if (value == "COMPLETE") "\u2714\ufe0f Yes" #if generation complete then green check mark with "yes"
        else "\u274c No" #if not then red x with "no"
      }
      
      resultsSchema <- resultDatabaseSettings$schema
      
      output$cohortCounts <- reactable::renderReactable({
        data <- getCohortGeneratorCohortCounts(
          connectionHandler = connectionHandler, 
          resultsSchema = resultsSchema,
          tablePrefix = resultDatabaseSettings$tablePrefix,
          databaseTable = resultDatabaseSettings$databaseTable,
          databaseTablePrefix = resultDatabaseSettings$databaseTablePrefix
          ) %>%
          dplyr::select("cdmSourceName",
                        "cohortId",
                        "cohortName",
                        "cohortSubjects",
                        "cohortEntries")
        reactable::reactable(data,
                             columns = list(
                               # Render a "show details" button in the last column of the table.
                               # This button won't do anything by itself, but will trigger the custom
                               # click action on the column.
                               cdmSourceName = reactable::colDef( 
                                 header = withTooltip(
                                   "Database Name", 
                                   "The name of the database"
                                 )),
                               cohortId = reactable::colDef( 
                                 header = withTooltip(
                                   "Cohort ID", 
                                   "The unique numeric identifier of the cohort"
                                 )),
                               cohortName = reactable::colDef( 
                                 header = withTooltip(
                                   "Cohort Name", 
                                   "The name of the cohort"
                                 )),
                               cohortSubjects = reactable::colDef( 
                                 header = withTooltip(
                                   "Number of Subjects", 
                                   "The number of distinct subjects in the cohort"
                                 ),
                                 format = reactable::colFormat(separators = TRUE
                                 )),
                               cohortEntries = reactable::colDef( 
                                 header = withTooltip(
                                   "Number of Records", 
                                   "The number of records in the cohort"
                                 ),
                                 format = reactable::colFormat(separators = TRUE
                                 ))
                             ),
                             filterable = TRUE,
                             sortable = TRUE,
                             defaultColDef = reactable::colDef(
                               align = "left"
                             )
                             )
      })
      
      # download button - counts
      output$downloadCohortCounts <- shiny::downloadHandler(
        filename = function() {
          paste('cohort-count-data-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          utils::write.csv(getCohortGeneratorCohortCounts(
            connectionHandler = connectionHandler, 
            resultsSchema = resultsSchema,
            tablePrefix = resultDatabaseSettings$tablePrefix,
            databaseTable = resultDatabaseSettings$databaseTable,
            databaseTablePrefix = resultDatabaseSettings$databaseTablePrefix
          ) %>%
            dplyr::select("cdmSourceName",
                          "cohortId",
                          "cohortName",
                          "cohortSubjects",
                          "cohortEntries"), con)
        }
      )

      output$cohortGeneration <- reactable::renderReactable({
        data <- getCohortGeneratorCohortMeta(
          connectionHandler = connectionHandler, 
          resultsSchema = resultsSchema,
          tablePrefix = resultDatabaseSettings$tablePrefix
          ) %>%
          dplyr::select("cohortId",
                        "cohortName",
                        "generationStatus",
                        "startTime",
                        "endTime")
        reactable::reactable(data,
                             columns = list(
                               # Render a "show details" button in the last column of the table.
                               # This button won't do anything by itself, but will trigger the custom
                               # click action on the column.
                               cohortId = reactable::colDef( 
                                 header = withTooltip(
                                   "Cohort ID", 
                                   "The unique numeric identifier of the cohort"
                                 )),
                               cohortName = reactable::colDef( 
                                 header = withTooltip(
                                   "Cohort Name", 
                                   "The name of the cohort"
                                 )),
                               generationStatus = reactable::colDef( 
                                 header = withTooltip(
                                   "Is the Cohort Generated?", 
                                   "Indicator of if the cohort has been generated"
                                 ),
                                 cell = format_yesorno
                                 ),
                               startTime = reactable::colDef( 
                                 header = withTooltip(
                                   "Generation Start Time", 
                                   "The time and date the cohort started generating"
                                 ),
                                 format = reactable::colFormat(datetime = TRUE
                                 )),
                               endTime = reactable::colDef( 
                                 header = withTooltip(
                                   "Generation End Time", 
                                   "The time and date the cohort finished generating"
                                 ),
                                 format = reactable::colFormat(datetime = TRUE
                                 ))
                             ),
                             filterable = TRUE,
                             sortable = TRUE,
                             defaultColDef = reactable::colDef(
                               align = "left"
                             )
        )
      })
      
      # download button - generation
      output$downloadCohortGeneration <- shiny::downloadHandler(
        filename = function() {
          paste('cohort-generation-data-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          utils::write.csv(getCohortGeneratorCohortMeta(
            connectionHandler = connectionHandler, 
            resultsSchema = resultsSchema,
            tablePrefix = resultDatabaseSettings$tablePrefix
          ) %>%
            dplyr::select("cohortId",
                          "cohortName",
                          "generationStatus",
                          "startTime",
                          "endTime"), con)
        }
      )
      
      #building attrition table using inclusion rules & stats tables
      rules <- getCohortGeneratorInclusionRules(
        connectionHandler = connectionHandler, 
        resultsSchema = resultsSchema,
        tablePrefix = resultDatabaseSettings$tablePrefix
      )
      
      stats <- getCohortGeneratorInclusionStats(
        connectionHandler = connectionHandler, 
        resultsSchema = resultsSchema,
        tablePrefix = resultDatabaseSettings$tablePrefix,
        databaseTable = resultDatabaseSettings$databaseTable,
        databaseTablePrefix = resultDatabaseSettings$databaseTablePrefix
      )
      
      #this gets the full attrition table
      inputVals <- getCohortGenerationAttritionTable(
        rules, 
        stats
      )
      
      #making a "clean" version where modeId is renamed to sensible values
      cohortNames <- unique(inputVals$cohortName)
      databaseIds <- unique(inputVals$cdmSourceName)
      inputValsClean <- dplyr::ungroup(inputVals) %>%
        dplyr::mutate(modeId = case_when(
          modeId==1 ~ "Subject",
          .default = "Record"
          )
        )
      modeIds <- unique(inputValsClean$modeId)
        
      
      cohortName <- shiny::reactiveVal(cohortNames[1])
      databaseId <- shiny::reactiveVal(databaseIds[1])
      modeId <- shiny::reactiveVal(modeIds[1])
      
      #build the selector
      output$attritionTableSelect <- shiny::renderUI({
        
        shiny::tagList(
          shiny::selectInput(
            inputId = session$ns('selectedCohortName'), 
            label = 'Cohort:', 
            choices = cohortNames, 
            selected = 1,
            multiple = F, 
            selectize=FALSE
          ),
          shiny::selectInput(
            inputId = session$ns('selectedDatabaseId'), 
            label = 'Database:', 
            choices = databaseIds, 
            selected = 1,
            multiple = F, 
            selectize=FALSE
          ),
          shiny::radioButtons(
            inputId = session$ns('selectedModeId'),
            label = "Subject-level or Record-level?",
            choices = modeIds,
            selected = "Subject"
          )
        )
      })
      
      shiny::observeEvent(input$selectedCohortName,{
        cohortName(input$selectedCohortName)
      })
      shiny::observeEvent(input$selectedDatabaseId,{
        databaseId(input$selectedDatabaseId)
      })
      shiny::observeEvent(input$selectedModeId,{
        modeId(input$selectedModeId)
      })
      
      #build the reactive data
      data <- shiny::reactive({
        inputValsClean %>%
          dplyr::filter(cdmSourceName == databaseId() & 
                          cohortName == cohortName() &
                          modeId == modeId()
          )
      })
      
      output$attritionTable <- reactable::renderReactable(
        reactable::reactable(
          data =  data() %>%
            dplyr::select(c(cdmSourceName, cohortName, ruleName,
                            personCount, dropCount,
                            dropPerc, retainPerc)
                          )
          
          ,
          rownames = FALSE, 
          defaultPageSize = 5,
          showPageSizeOptions = T, 
          striped = T,
          columns = list(
            cdmSourceName = reactable::colDef( 
              filterable = TRUE,
              header = withTooltip(
                "Database Name", 
                "The name of the database"
              )),
            cohortName = reactable::colDef( 
              filterable = TRUE,
              header = withTooltip(
                "Cohort Name", 
                "The name of the cohort"
              )),
            ruleName = reactable::colDef( 
              header = withTooltip(
                "Inclusion Rule Name", 
                "The name of the inclusion rule"
              )),
            personCount = reactable::colDef( 
              format = reactable::colFormat(separators = TRUE),
              header = withTooltip(
                "Subject/Record Count", 
                "The number of subjects or records (depending on your selection) remaining after the inclusion rule was applied"
              )),
            dropCount = reactable::colDef( 
              format = reactable::colFormat(separators = TRUE),
              header = withTooltip(
                "Number Lost", 
                "The number of subjects or records (depending on your selection) removed/lost after the inclusion rule was applied"
              )),
            dropPerc = reactable::colDef( 
              format = reactable::colFormat(separators = TRUE),
              header = withTooltip(
                "Percentage Lost", 
                "The percentage of subjects or records (depending on your selection) removed/lost after the inclusion rule was applied compared to the previous rule count"
              )),
            retainPerc = reactable::colDef( 
              format = reactable::colFormat(separators = TRUE),
              header = withTooltip(
                "Number Retained", 
                "The number of subjects or records (depending on your selection) retained after the inclusion rule was applied compared to the previous rule count"
              ))
          ),
          
          filterable = TRUE,
          sortable = TRUE,
          defaultColDef = reactable::colDef(
            align = "left"
          )
        )
      )
      
      #attrition plot
      output$attritionPlot <- plotly::renderPlotly(
        getCohortAttritionPlot(
          data()
        )
      )
      
      # download button
      output$downloadAttritionTable <- shiny::downloadHandler(
        filename = function() {
          paste('cohort-attrition-data-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          utils::write.csv(data()
                           , con)
        }
      )
      
      #testing funnel plot for attrition visualization
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      # output$inclusionSummary <- reactable::renderReactable({
      #   data <- getCohortGeneratorCohortInclusionSummary(
      #     connectionHandler = connectionHandler, 
      #     resultsSchema = resultsSchema,
      #     tablePrefix = resultDatabaseSettings$tablePrefix,
      #     databaseTable = resultDatabaseSettings$databaseTable,
      #     databaseTablePrefix = resultDatabaseSettings$databaseTablePrefix
      #   ) %>%
      #     dplyr::select("cdmSourceName",
      #                   "cohortDefinitionId",
      #                   "cohortName",
      #                   "baseCount",
      #                   "finalCount",
      #                   "modeId") %>%
      #     dplyr::mutate(modeId = 
      #                     dplyr::case_when(
      #                       modeId == 1 ~ "Subjects",
      #                       .default = "Records"
      #                     )
      #                   )
      #   reactable::reactable(data,
      #                        columns = list(
      #                          # Render a "show details" button in the last column of the table.
      #                          # This button won't do anything by itself, but will trigger the custom
      #                          # click action on the column.
      #                          cdmSourceName = reactable::colDef( 
      #                            header = withTooltip(
      #                              "Database Name", 
      #                              "The name of the database"
      #                            )),
      #                          cohortDefinitionId = reactable::colDef( 
      #                            header = withTooltip(
      #                              "Cohort ID", 
      #                              "The unique numeric identifier of the cohort"
      #                            )),
      #                          cohortName = reactable::colDef( 
      #                            header = withTooltip(
      #                              "Cohort Name", 
      #                              "The name of the cohort"
      #                            )),
      #                          baseCount = reactable::colDef( 
      #                            header = withTooltip(
      #                              "Base Count", 
      #                              "The number of records before any inclusion criteria are applied (entry events)"
      #                            ),
      #                            format = reactable::colFormat(separators = TRUE
      #                            )),
      #                          finalCount = reactable::colDef( 
      #                            header = withTooltip(
      #                              "Final Count", 
      #                              "The number of records after all inclusion criteria are applied"
      #                            ),
      #                            format = reactable::colFormat(separators = TRUE
      #                            )),
      #                          modeId = reactable::colDef( 
      #                            header = withTooltip(
      #                              "Records or Subjects?", 
      #                              "An indicator of whether the counts shown are the number of subjects or the number of records"
      #                            ),
      #                            format = reactable::colFormat(separators = TRUE
      #                            ))
      #                        ),
      #                        filterable = TRUE,
      #                        sortable = TRUE,
      #                        defaultColDef = reactable::colDef(
      #                          align = "left"
      #                        )
      #   )
      # })
      
    }
  )
}
