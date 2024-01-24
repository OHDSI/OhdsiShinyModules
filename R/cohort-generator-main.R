# @file cohortgenerator-main.R
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
      title = shiny::span( shiny::icon("user-gear"),'Cohorts'),
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
            collapsible = T,
            collapsed = F,
            width = '100%',
            title = shiny::span( shiny::icon("file-arrow-down"),'Download Data'),
            #solidHeader = TRUE,
            
            shiny::downloadButton(
              ns('downloadCohortCountsFull'),
              label = "Download (Full)",
              icon = shiny::icon("download")
            ),
            
            shiny::actionButton(
              ns('downloadCohortCountsFiltered'),
              label = "Download (Filtered)",
              icon = shiny::icon("download"),
              onclick = paste0("Reactable.downloadDataCSV('", ns('cohortCounts'),
                               "', 'cohort-count-data-filtered-", Sys.Date(), ".csv')")
            )
          ),
          
          shinydashboard::box(
            width = '100%',
            title = shiny::span( shiny::icon("table"), 'Counts Table'),
            #solidHeader = TRUE,
            
            shiny::uiOutput(ns("selectColsCohortCounts")
            ),
            
            reactable::reactableOutput(
              outputId = ns("cohortCounts")
            )  
          )
        ),
        
        shiny::tabPanel(
          title = "Cohort Generation",
          
          shinydashboard::box(
            collapsible = T,
            collapsed = F,
            width = '100%',
            title = shiny::span( shiny::icon("file-arrow-down"),'Download Data'),
            #solidHeader = TRUE,
            
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
            #solidHeader = TRUE,
            
            reactable::reactableOutput(
              outputId = ns("cohortGeneration")
            )  
          )
        ),
        
        shiny::tabPanel(
          title = "Inclusion Rules & Attrition"
          ,
          
          shinydashboard::box(
            collapsible = T,
            collapsed = F,
            width = '100%',
            title = shiny::span( shiny::icon("gear"), 'Options'),
            #solidHeader = TRUE,
            
            shiny::uiOutput(ns('attritionTableSelect'))
          ),
          
          shiny::conditionalPanel(
            condition = "input.generate != 0",
            ns = ns,
            
            shiny::uiOutput(ns("inputsText")),
            
            shinydashboard::box(
              collapsible = T,
              collapsed = F,
              width = '100%',
              title = shiny::span( shiny::icon("file-arrow-down"),'Download Data'),
              #solidHeader = TRUE,
              
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
              #solidHeader = TRUE,
              
              reactable::reactableOutput(ns('attritionTable'))
            ),
            
            shinydashboard::box(
              status = 'info', 
              width = '100%',
              title = shiny::span( shiny::icon("chart-area"), 'Attrition Plot'),
              #solidHeader = TRUE,
              
              plotly::plotlyOutput(ns('attritionPlot'))
            )
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
      
      inputColsCohortCounts <- colnames(getCohortGeneratorCohortCounts(
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings
      ) %>%
        dplyr::select("cdmSourceName",
                      "cohortId",
                      "cohortName",
                      "cohortSubjects",
                      "cohortEntries")
      )
      
      names(inputColsCohortCounts) <- c("Database Name", 
                                        "Cohort ID",
                                        "Cohort Name",
                                        "Number of Subjects",
                                        "Number of Records")
      
      output$selectColsCohortCounts <- shiny::renderUI({
        
        shinyWidgets::pickerInput(
          inputId = session$ns('cohortCountsCols'), 
          label = 'Select Columns to Display: ', 
          choices = inputColsCohortCounts, 
          selected = inputColsCohortCounts,
          choicesOpt = list(style = rep_len("color: black;", 999)),
          multiple = T,
          options = shinyWidgets::pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            size = 10,
            liveSearchStyle = "contains",
            liveSearchPlaceholder = "Type here to search",
            virtualScroll = 50
          ),
          width = "50%"
        )
        
      })
      
      data <- getCohortGeneratorCohortCounts(
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings
      ) %>%
        dplyr::select("cdmSourceName",
                      "cohortId",
                      "cohortName",
                      "cohortSubjects",
                      "cohortEntries")
      
      
      rtable <- shiny::reactive({
        reactable::reactable(
          data %>%
            dplyr::select(input$cohortCountsCols),
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
          resizable = T,
          searchable = T,
          striped = T,
          defaultColDef = reactable::colDef(
            align = "left"
          )
        )
        
      })
      
      output$cohortCounts <- reactable::renderReactable({
        
        tryCatch({
          rtable()
        },
        
        error = function(e){
          shiny::showNotification(
            paste0(
              "Loading..."
            )
          ); 
          return(NULL)
        }
          
        )
    })
      
      
      # download buttons - counts
      output$downloadCohortCountsFull <- shiny::downloadHandler(
        filename = function() {
          paste('cohort-count-data-full', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          utils::write.csv(getCohortGeneratorCohortCounts(
            connectionHandler = connectionHandler, 
            resultDatabaseSettings = resultDatabaseSettings
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
          resultDatabaseSettings = resultDatabaseSettings
          ) %>%
          dplyr::select("cdmSourceName",
                        "cohortId",
                        "cohortName",
                        "generationStatus",
                        "startTime",
                        "endTime",
                        "generationDuration")
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
                                 format = reactable::colFormat(suffix = " mins"
                                 #format = reactable::colFormat(datetime = TRUE
                                 )),
                               endTime = reactable::colDef( 
                                 header = withTooltip(
                                   "Generation End Time", 
                                   "The time and date the cohort finished generating"
                                 ),
                                 format = reactable::colFormat(datetime = TRUE
                                 )),
                               generationDuration = reactable::colDef( 
                                 header = withTooltip(
                                   "Generation Duration (mins)", 
                                   "The time it took (in minutes) to generate the cohort"
                                 ),
                                 format = reactable::colFormat(digits = 2)

                                 )
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
            resultDatabaseSettings = resultDatabaseSettings
          ) %>%
            dplyr::select("cohortId",
                          "cohortName",
                          "generationStatus",
                          "startTime",
                          "endTime"), con)
        }
      )
      
      tryCatch(
        
        {
      
      #building attrition table using inclusion rules & stats tables
      rules <- getCohortGeneratorInclusionRules(
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      stats <- getCohortGeneratorInclusionStats(
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings
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
        dplyr::mutate(modeId = dplyr::case_when(
          modeId==1 ~ "Subject",
          TRUE ~ "Record"
          )
        )
      
      modeIds <- unique(inputValsClean$modeId)
      
      # cohortName <- shiny::reactiveVal(cohortNames[1])
      # databaseId <- shiny::reactiveVal(databaseIds[1])
      # modeId <- shiny::reactiveVal(modeIds[1])
      
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
          ),
          shiny::actionButton(
            inputId = session$ns('generate'),
            label = 'Generate Report'
          )
        )
      })
      
      reactiveData <- shiny::reactiveVal(NULL)
      selectedInputs <- shiny::reactiveVal()
      output$inputsText <- shiny::renderUI(selectedInputs())
      
      # shiny::observeEvent(input$selectedCohortName,{
      #   cohortName(input$selectedCohortName)
      # })
      # shiny::observeEvent(input$selectedDatabaseId,{
      #   databaseId(input$selectedDatabaseId)
      # })
      # shiny::observeEvent(input$selectedModeId,{
      #   modeId(input$selectedModeId)
      # })
      
      #build the reactive data
      
      shiny::observeEvent(
        eventExpr = input$generate,
    {
      
      # if(length(input$selectedCohortName) == 0 | is.null(input$selectedDatabaseId |
      #                                                    is.null(input$selectedModeId))){
      #   print('Null ids value')
      #   return(invisible(NULL))
      # }
      
      selectedInputs(
        shinydashboard::box(
          status = 'warning', 
          width = "100%",
          title = 'Selected:',
          collapsible = T,
          collapsed = F,
          shiny::div(
            shiny::fluidRow(
              shiny::column(
                width = 8,
                shiny::tags$b("Cohort:"),
                 #unique(inputVals$cohortName[inputVals$cohortName %in% input$selectedCohortName])
                input$selectedCohortName
              ),
              shiny::column(
                width = 4,
                shiny::tags$b("Database:"),
                #unique(inputVals$cdmSourceName[inputVals$cdmSourceName == input$selectedDatabaseId])
                input$selectedDatabaseId
              ),
              shiny::column(
                width = 4,
                shiny::tags$b("Level:"),
                #unique(inputValsClean$modeId)[inputValsClean$modeId == input$selectedModeId]
                input$selectedModeId
              )
            )
          )
        )
      )

      
      data <- inputValsClean %>%
          dplyr::filter(.data$cdmSourceName %in% input$selectedDatabaseId & 
                          .data$cohortName %in% input$selectedCohortName &
                          .data$modeId %in% input$selectedModeId
          )
      
      reactiveData <- shiny::reactive(data)
      
      if(!is.null(data)){
      
      output$attritionTable <- reactable::renderReactable(
        reactable::reactable(
          data =  reactiveData() %>%
            dplyr::select(c("cdmSourceName", "cohortName", "ruleName",
                            "personCount", "dropCount",
                            "dropPerc", "retainPerc")
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
          data
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
      }
      
      else{
        shiny::showNotification('data NULL')
      }
      
    }
  )
      
    },
      
      error = function(e){
        shiny::showNotification(
          paste0(
            "No cohort inclusion result data present."
          )
        ); 
        return(NULL)
      }

  )
      
      # end of server
      
    }
  )
}




getCohortGeneratorCohortCounts <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  sql <- "SELECT cc.cohort_id, cc.cohort_entries, cc.cohort_subjects,
  dt.cdm_source_name, cd.cohort_name 
  FROM @schema.@cg_table_prefixCOHORT_COUNT cc
  join @schema.@database_table_prefix@database_table dt
  on cc.database_id = dt.database_id
  join @schema.@cg_table_prefixCOHORT_DEFINITION cd
  on cd.cohort_definition_id = cc.cohort_id
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
      database_table = resultDatabaseSettings$databaseTable,
      database_table_prefix = resultDatabaseSettings$databaseTablePrefix
    )
  )
}

getCohortGeneratorCohortMeta <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  sql <- "SELECT cg.cohort_id, cg.cohort_name,
  cg.generation_status, cg.start_time, cg.end_time, dt.cdm_source_name
  from @schema.@cg_table_prefixCOHORT_GENERATION cg
  join @schema.@database_table_prefix@database_table dt
  on cg.database_id = dt.database_id
  ;"
  
  df <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
    database_table = resultDatabaseSettings$databaseTable,
    database_table_prefix = resultDatabaseSettings$databaseTablePrefix
  )
  
  df2 <- df %>%
    dplyr::mutate(
      generationDuration = dplyr::case_when(
        generationStatus == "COMPLETE"
        ~ tryCatch(
          {
            difftime(
              as.POSIXct(as.numeric(.data$endTime), origin = "1970-01-01"),
              as.POSIXct(as.numeric(.data$startTime), origin = "1970-01-01"),
              units="mins"
            )
          },
          error = function(e){return(NA)}
        ),
        T ~ NA
      )
    )
  
  return(df2)
}

getCohortGeneratorCohortInclusionSummary <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  sql <- "SELECT css.cohort_definition_id, css.base_count, css.final_count, css.mode_id,
  dt.cdm_source_name, cd.cohort_name 
  FROM @schema.@cg_table_prefixCOHORT_SUMMARY_STATS css
  join @schema.@database_table_prefix@database_table dt
  on css.database_id = dt.database_id
  join @schema.@cg_table_prefixCOHORT_DEFINITION cd
  on cd.cohort_definition_id = css.cohort_definition_id
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      schema =resultDatabaseSettings$schema,
      cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
      database_table = resultDatabaseSettings$databaseTable,
      database_table_prefix = resultDatabaseSettings$databaseTablePrefix
    )
  )
}



getCohortGeneratorInclusionRules <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  sql <- "SELECT ci.cohort_definition_id, ci.rule_sequence, ci.name as rule_name,
  cd.cohort_name FROM @schema.@cg_table_prefixCOHORT_INCLUSION ci
  join @schema.@cg_table_prefixCOHORT_DEFINITION cd
  on cd.cohort_definition_id = ci.cohort_definition_id
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      cg_table_prefix = resultDatabaseSettings$cgTablePrefix
    )
  )
}

getCohortGeneratorInclusionStats <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  sql <- "SELECT cir.database_id, cir.cohort_definition_id, cir.inclusion_rule_mask, cir.person_count, cir.mode_id,
  dt.cdm_source_name FROM @schema.@cg_table_prefixCOHORT_INC_RESULT cir
  join @schema.@database_table_prefix@database_table dt
  on cir.database_id = dt.database_id
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
      database_table = resultDatabaseSettings$databaseTable,
      database_table_prefix = resultDatabaseSettings$databaseTablePrefix
    )
  )
}

getCohortGenerationAttritionTable <- function(
    rules,
    stats
){
  
  
  
  uniqueCohortIDs <- unique(rules$cohortDefinitionId)
  
  attritionTable <- data.frame()
  
  for(cohortId in uniqueCohortIDs){
    
    cohortRules <- rules %>% 
      dplyr::filter(.data$cohortDefinitionId==cohortId) %>%
      dplyr::select("ruleSequence", "ruleName", "cohortName") %>%
      dplyr::arrange("ruleSequence")
    
    testMask = 0
    
    for(i in 1:nrow(cohortRules)){
      
      rule = cohortRules[i,]
      
      testMask = testMask + 2^(rule$ruleSequence)
      
      attritionRows <- stats %>%
        dplyr::filter((.data$cohortDefinitionId == !!cohortId) &
                        (bitwAnd(.data$inclusionRuleMask, !!testMask) == !!testMask)
        ) %>% 
        dplyr::select(-c("databaseId")) %>%
        dplyr::group_by(.data$cdmSourceName, .data$cohortDefinitionId, .data$modeId) %>%
        dplyr::summarise(personCount = sum(.data$personCount),
        )
      
      startingCounts <- stats %>%
        dplyr::select(-c("databaseId")) %>%
        dplyr::group_by(.data$cdmSourceName, .data$cohortDefinitionId, .data$modeId) %>%
        dplyr::summarise(personCount = sum(.data$personCount),
        ) %>%
        dplyr::mutate(ruleSequence = -1,
                      ruleName = "Before any inclusion criteria",
        )
      
      attritionRowsFull <- cbind(attritionRows, rule)
      
      startingCountsFull <- cbind(startingCounts, rule %>% dplyr::select("cohortName")) %>%
        dplyr::filter(.data$cohortDefinitionId %in% !!attritionRows$cohortDefinitionId)
      
      attritionTable <- rbind(attritionTable, attritionRowsFull, startingCountsFull)
      
    }
    
  }
  
  attritionTableDistinct <- dplyr::distinct(attritionTable)
  
  #adding drop counts
  attritionTableFinal <- attritionTableDistinct %>%
    dplyr::group_by(
      .data$cdmSourceName, 
      .data$cohortDefinitionId, 
      .data$modeId) %>%
    dplyr::mutate(
      dropCount = dplyr::case_when(
        is.na(dplyr::lag(.data$personCount, order_by = .data$ruleSequence)) ~ 0,
        TRUE ~ dplyr::lag(.data$personCount, order_by = .data$ruleSequence) - .data$personCount
      ),
      dropPerc = dplyr::case_when(
        is.na(dplyr::lag(.data$personCount, order_by = .data$ruleSequence)) ~ "0.00%",
        TRUE ~  paste(
          round(
            (.data$dropCount/(dplyr::lag(.data$personCount, order_by = .data$ruleSequence)) * 100), 
            digits = 2
          ),
          "%",
          sep="")
      ),
      retainPerc = dplyr::case_when(
        is.na(dplyr::lag(.data$personCount, order_by = .data$ruleSequence)) ~ "100.00%",
        TRUE ~ paste(
          round(
            (.data$personCount/(dplyr::lag(.data$personCount, order_by = .data$ruleSequence)) * 100), 
            digits = 2
          ),
          "%",
          sep="")
        
      )
    )
  #newdata <- mtcars[order(mpg, -cyl),]
  return(attritionTableFinal[order(attritionTableFinal$ruleSequence),])
  
}

# test <- inputValsClean %>%
#   dplyr::filter(cohortDefinitionId == 11057 & cdmSourceName == "Optum EHR" & 
#            modeId == "Subject")

getCohortAttritionPlot <- function(data) {
  
  #colorPal <- colorRampPalette(c("darkgreen", "green", "yellow", "orange", "red"))
  
  fig <- plotly::plot_ly() 
  fig %>%
    plotly::add_trace(
      type = "funnel",
      y = data$ruleName,
      x = data$personCount,
      texttemplate = "N: %{value:,d}<br>Number Lost: %{text:,d}",
      marker = list(color = RColorBrewer::brewer.pal(length(unique(data$ruleName)),
                                                     "Greens"
      )
      ),
      connector = list(fillcolor = "#e9e9bf"),
      text = data$dropCount,
      hoverinfo = "percent initial+percent previous" ,
      hovertemplate='% of Previous: %{percentPrevious:.2%}<br> % of Initial: %{percentInitial:.2%}</b><extra></extra>'
    ) %>%
    plotly::layout(title = "Cohort Attrition by Inclusion Rules",
                   yaxis = list(categoryarray = c(order(data$personCount, decreasing = T)))
    )
  
}

