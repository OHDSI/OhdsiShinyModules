# @file cohortgenerator-main.R
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


#' The location of the cohort-generator module helper file
#'
#' @details
#' Returns the location of the cohort-generator helper file
#' @family CohortGenerator
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
#' @family CohortGenerator
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

      
      shiny::tabsetPanel(
        id = ns("cohortGeneratorTabs"),
        type = "pills",
        
        
        shiny::tabPanel(
          title = "Cohort Counts",
          
          shinydashboard::box(
            collapsible = T,
            collapsed = F,
            width = '100%',
          
            resultTableViewer(
              ns("cohortCounts"),
              downloadedFileName = "cohortCountsTable-"
            )
          )
        ),
        
        shiny::tabPanel(
          title = "Cohort Generation",
          
          shinydashboard::box(
            collapsible = T,
            collapsed = F,
            width = '100%',
            
          resultTableViewer(
            ns("cohortGeneration"),
            downloadedFileName = "cohortGenerationTable-"
          )
          ),
 
          ),
        
        shiny::tabPanel(
          title = "Cohort Definition",
          
          shinydashboard::box(
            collapsible = T,
            collapsed = F,
            width = '100%',
            title = shiny::span( shiny::icon("gear"), 'Options'),
            shiny::uiOutput(ns('cohortDefinitionCohortSelect'))
          ),
          
          shiny::conditionalPanel(
            condition = "input.generate_cohort_def != 0",
            ns = ns,
            
            shiny::uiOutput(ns("inputsCohortDefText")),
            
            shiny::tabsetPanel( 
              id = ns('cohortDefPanel'),
              
              shiny::tabPanel(
                title = "Friendly Definition",
                shiny::uiOutput(ns('outputCohortDefText'))
              ),
              
              shiny::tabPanel(
                title = "JSON",
                shiny::uiOutput(ns('outputCohortDefJson'))
              ),
              
              shiny::tabPanel(
                title = "SQL",
                shiny::uiOutput(ns('outputCohortDefSql'))
              ),
              
              shiny::tabPanel(
                title = "Inclusion Rules & Attrition", 
                
                shiny::uiOutput(ns('attritionRuleSelect')),
                
                #shiny::conditionalPanel(
                #  condition = "input.generate_attrition != 0",
                #  ns = ns,
                  
                shiny::uiOutput(ns("attritionInputsText")),
                
                shiny::uiOutput(ns("attritionOutputTable")),
                
                shiny::uiOutput(ns("attritionOutputPlot"))
                  
                #) # end attrition select condition
                
              ) # end inclusion tab
              
              
            ) # end cohort def sub tabs
 
            
          ) # end conditional on generate
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
#' @family CohortGenerator
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
      
      
      cohortCountsColDefs = list(
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
      )
      
      resultTableServer(
        id = "cohortCounts",
        df = data,
        colDefsInput = cohortCountsColDefs,
        downloadedFileName = "cohortCountsTable-"
      )
      
      
      
     # cohort generation table
      
      inputColsCohortGeneration <- colnames(getCohortGeneratorCohortMeta(
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
      )
      
      names(inputColsCohortGeneration) <- c("Database Name", 
                                        "Cohort ID",
                                        "Cohort Name",
                                        "Is the Cohort Generated?",
                                        "Generation Start Time",
                                        "Generation End Time",
                                        "Generation Duration (mins)")
      
      output$selectColsCohortGeneration <- shiny::renderUI({
        
        shinyWidgets::pickerInput(
          inputId = session$ns('cohortGenerationCols'), 
          label = 'Select Columns to Display: ', 
          choices = inputColsCohortGeneration, 
          selected = inputColsCohortGeneration,
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
      
      dataGen <- getCohortGeneratorCohortMeta(
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
      
      
      cohortGenerationColDefs <- list(
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
      )
      
      resultTableServer(
        id = "cohortGeneration",
        df = dataGen,
        colDefsInput = cohortGenerationColDefs,
        downloadedFileName = "cohortGenerationTable-"
      )
      
      
      
      # NEW: Cohort definition
      
      cohortDefData <- getCohortGeneratorCohortDefinition(
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      cohortDefInputs <- 1:nrow(cohortDefData)
      names(cohortDefInputs) <- cohortDefData$cohortName
      
      output$cohortDefinitionCohortSelect <- shiny::renderUI(
        shiny::tagList(
        shiny::selectInput(
          inputId = session$ns('selectedCohortDefRow'), 
          label = 'Cohort:', 
          choices = cohortDefInputs, 
          selected = 1,
          multiple = FALSE, 
          selectize = FALSE
        ),
        shiny::actionButton(
          inputId = session$ns('generate_cohort_def'),
          label = 'Generate'
        )
      )
      )
      
    # reactive vars for all the cohort def parts  
    selectedCohortDefInputs <- shiny::reactiveVal()
    selectedJson <- shiny::reactiveVal()
    selectedSql <- shiny::reactiveVal()
    selectedJsonText <- shiny::reactiveVal()
    
    attritionData <- shiny::reactiveVal(NULL)
    
    selectedAttritionInputs <- shiny::reactiveVal()
    selectedAttritionTable <- shiny::reactiveVal()
    selectedAttritionPlot <- shiny::reactiveVal()
    
    
    # outputs for all the cohort def parts
    output$inputsCohortDefText <- shiny::renderUI(selectedCohortDefInputs())
    output$outputCohortDefJson <- shiny::renderUI(selectedJson())
    output$outputCohortDefSql <- shiny::renderUI(selectedSql())
    output$outputCohortDefText <- shiny::renderUI(selectedJsonText())
    
    output$attritionInputsText <- shiny::renderUI(selectedAttritionInputs())
    output$attritionOutputTable  <- shiny::renderUI(selectedAttritionTable())
    output$attritionOutputPlot  <- shiny::renderUI(selectedAttritionPlot())
    
    
    shiny::observeEvent(
      eventExpr = input$generate_cohort_def,
      {
        
        # make the output for the attrition empty 
        # evertime a new cohort is delected
        selectedAttritionInputs(NULL)
        selectedAttritionTable(NULL)
        selectedAttritionPlot(NULL)
        
        # set to tab to cohort friendly
        shiny::updateTabsetPanel(
          session = session,
          inputId = 'cohortDefPanel',
          selected = "Friendly Definition"
        )
        
        json <- cohortDefData$parentJson[as.double(input$selectedCohortDefRow)]
        subset <- cohortDefData$subsetJson[as.double(input$selectedCohortDefRow)]
        isSubset <- cohortDefData$cohortDefinitionId[as.double(input$selectedCohortDefRow)] != cohortDefData$subsetParent[as.double(input$selectedCohortDefRow)]
          
        noAttritionText <- ifelse(isSubset, 'Cannot display for cohorts with subset logic', 'No attrition results to display')
        
        selectedCohortDefInputs(
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
                  shiny::tags$b("Cohort Name:"),
                  cohortDefData$cohortName[as.double(input$selectedCohortDefRow)]
                )
              )
            )
          )
        )
        

        # get the json 
        if(isSubset){        
          selectedJson(
          shinydashboard::box(
            status = 'primary', 
            solidHeader = TRUE,
            width = "100%",
            title = 'JSON Code',
            collapsible = T,
            collapsed = F,
            
            shiny::tabsetPanel(
              
              shiny::tabPanel(
                title = 'Parent',
                shiny::renderPrint({
                  cat(json, sep = "\n")
                })
              ),
              
              shiny::tabPanel(
                title = 'Subset',
                shiny::renderPrint({
                  cat(subset, sep = "\n")
                })
              )
              
            )
          ))
            
            selectedJsonText(
              shinydashboard::box(
                status = 'primary', 
                solidHeader = TRUE,
                width = "100%",
                title = 'Cohort Definition',
                collapsible = T,
                collapsed = F,
                
                shiny::tabsetPanel(
                  
                  shiny::tabPanel(
                    title = 'Parent',
                shiny::HTML(
                  markdown::renderMarkdown(text = CirceR::cohortPrintFriendly(json))
                )
                  ),
                
                shiny::tabPanel(
                  title = 'Subset',
                  shiny::HTML(
                    markdown::renderMarkdown(text = extractSubsetText(subset))
                    )
                )
                
                )
                
              )
            )
            
            selectedSql(
              shinydashboard::box(
                status = 'primary', 
                solidHeader = TRUE,
                width = "100%",
                title = 'SQL Code',
                collapsible = T,
                collapsed = F,
                
                shiny::tabsetPanel(
                  
                  shiny::tabPanel(
                    title = 'Parent',
                    shiny::renderPrint({
                      cat(CirceR::buildCohortQuery(
                        expression = json, 
                        options = CirceR::createGenerateOptions()
                      ), sep = "\n")
                    })
                  ),
                  
                  shiny::tabPanel(
                    title = 'Subset',
                    shiny::renderPrint({
                      cat(
                        cohortDefData$sqlCommand[as.double(input$selectedCohortDefRow)], 
                        sep = "\n")
                    })
                  )
                  
                )
              ))
            
        } else{
          selectedJson(
            shinydashboard::box(
              status = 'primary', 
              solidHeader = TRUE,
              width = "100%",
              title = 'JSON Code',
              collapsible = T,
              collapsed = F,
              
                  shiny::renderPrint({
                    cat(json, sep = "\n")
                  })
                
            ))
          
          selectedJsonText(
            shinydashboard::box(
              status = 'primary', 
              solidHeader = TRUE,
              width = "100%",
              title = 'Cohort Definition',
              collapsible = T,
              collapsed = F,
              
              shiny::HTML(
                markdown::renderMarkdown(text = CirceR::cohortPrintFriendly(json))
              )
              
            )
          )
          
          selectedSql(
            shinydashboard::box(
              status = 'primary', 
              solidHeader = TRUE,
              width = "100%",
              title = 'SQL Code',
              collapsible = T,
              collapsed = F,
              
              shiny::renderPrint({
                cat(CirceR::buildCohortQuery(
                  expression = json, 
                  options = CirceR::createGenerateOptions()
                ), sep = "\n")
              })
              
            ))
          
        }
        
        # add attrition stuff
        #building attrition table using inclusion rules & stats tables
        rules <- getCohortGeneratorInclusionRules(
          connectionHandler = connectionHandler, 
          resultDatabaseSettings = resultDatabaseSettings,
          cohortDefinitionId = cohortDefData$cohortDefinitionId[as.double(input$selectedCohortDefRow)]
        )

        stats <- getCohortGeneratorInclusionStats(
          connectionHandler = connectionHandler, 
          resultDatabaseSettings = resultDatabaseSettings,
          cohortDefinitionId = cohortDefData$cohortDefinitionId[as.double(input$selectedCohortDefRow)]
        )

        if(!nrow(rules) == 0 & !nrow(stats) == 0){
        #this gets the full attrition table
        inputVals <- getCohortGenerationAttritionTable(
          rules, 
          stats
        )
        
        attritionData(dplyr::ungroup(inputVals) %>%
          dplyr::mutate(modeId = dplyr::case_when(
            modeId==1 ~ "Subject",
            TRUE ~ "Record"
          )
          ))
        
        #build the selector
        output$attritionRuleSelect <- shiny::renderUI({
          
          shiny::tagList(
            shiny::selectInput(
              inputId = session$ns('selectedDatabaseId'), 
              label = 'Database:', 
              choices = unique(attritionData()$cdmSourceName), 
              selected = 1,
              multiple = F, 
              selectize=FALSE
            ),
            shiny::radioButtons(
              inputId = session$ns('selectedModeId'),
              label = "Subject-level or Record-level?",
              choices = unique(attritionData()$modeId),
              selected = "Subject"
            ),
            shiny::actionButton(
              inputId = session$ns('generate_attrition'),
              label = 'Generate Report'
            )
          )
        })  
        } else {
          
          output$attritionRuleSelect <- shiny::renderUI({
             shiny::renderText(noAttritionText)
            })
        }
        
      }) # end observe event
      
      # inclusion rules and attrition
      
      tryCatch(
        
        {
      
      #build the reactive data
      shiny::observeEvent(
        eventExpr = input$generate_attrition,
    {
      
      selectedAttritionInputs(
        shinydashboard::box(
          status = 'warning', 
          width = "100%",
          title = 'Selected:',
          collapsible = T,
          collapsed = F,
          shiny::div(
            shiny::fluidRow(
              shiny::column(
                width = 4,
                shiny::tags$b("Database:"),
                input$selectedDatabaseId
              ),
              shiny::column(
                width = 4,
                shiny::tags$b("Level:"),
                input$selectedModeId
              )
            )
          )
        )
      )
      
      selectedAttritionTable(
        shinydashboard::box(
          status = 'info', 
          width = '100%',
          title = shiny::span(shiny::icon("table"), 'Attrition Table'),
          
          resultTableViewer(session$ns('attritionTable'),
                            downloadedFileName = "cohortAttritionTable-")
        )
      )
      
      selectedAttritionPlot(
        shinydashboard::box(
          status = 'info', 
          width = '100%',
          title = shiny::span( shiny::icon("chart-area"), 'Attrition Plot'),
          
          plotly::plotlyOutput(session$ns('attritionPlot'))
        )
      )
      

      selectedAttritionData <- attritionData() %>%
        dplyr::filter(.data$cdmSourceName %in% input$selectedDatabaseId & 
                        .data$modeId %in% input$selectedModeId
        )
      
      if(!is.null(selectedAttritionData)){ # or nrow > 0 ?
        
        resultTableServer(
            id = 'attritionTable',
            df =  selectedAttritionData %>%
              dplyr::select(c("cdmSourceName", "cohortName", "ruleName",
                              "personCount", "dropCount",
                              "dropPerc", "retainPerc")
              )
            
            ,
            colDefsInput = list(
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
                  "Percentage Retained", 
                  "The percentage of subjects or records (depending on your selection) retained after the inclusion rule was applied compared to the previous rule count"
                ))
            )
          )
        
        #attrition plot
        output$attritionPlot <- plotly::renderPlotly(
          getCohortAttritionPlot(
            selectedAttritionData
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
      } else{
        shiny::showNotification('data NULL')
      }
      
    }) # end observe
      
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


extractSubsetText <- function(subsetJson){
  
  # add code to extract the names from the json and display as bullet points
  
  json <- ParallelLogger::convertJsonToSettings(as.character(subsetJson))
  getOperatorNames <- lapply(json$subsetOperators, function(x){x$name})
  getOperatorNames <- paste0('- ',unlist(getOperatorNames), collapse = ' \n')

  return(getOperatorNames)
}


getCohortGeneratorCohortDefinition <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  
  sql <- "SELECT cd.*, csd.json as subset_json
  FROM @schema.@cg_table_prefixCOHORT_DEFINITION cd
  left join 
  @schema.@cg_table_prefixcohort_subset_definition csd
  on cd.subset_definition_id = csd.subset_definition_id
  ;"
  
  result <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix
  )
  
  parents <- result %>%
    dplyr::filter(.data$cohortDefinitionId == .data$subsetParent) %>%
    dplyr::select("json","cohortDefinitionId") %>%
    dplyr::rename(
      parentJson = "json",
      parentCohortDefinitionId = 'cohortDefinitionId'
    )
  
  result <- merge(
    x = result,
    y = parents, 
    by.x = 'subsetParent', 
    by.y = 'parentCohortDefinitionId'
  )
    
  return(result)
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
    resultDatabaseSettings,
    cohortDefinitionId = NULL
) {
  
  sql <- "SELECT ci.cohort_definition_id, ci.rule_sequence, ci.name as rule_name,
  cd.cohort_name FROM @schema.@cg_table_prefixCOHORT_INCLUSION ci
  join @schema.@cg_table_prefixCOHORT_DEFINITION cd
  on cd.cohort_definition_id = ci.cohort_definition_id
  {@use_cohort_def_id}?{ where cd.cohort_definition_id = @cohort_definition_id}
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
      use_cohort_def_id = !is.null(cohortDefinitionId),
      cohort_definition_id = cohortDefinitionId
    )
  )
}

getCohortGeneratorInclusionStats <- function(
    connectionHandler, 
    resultDatabaseSettings,
    cohortDefinitionId = NULL
) {
  
  sql <- "SELECT cir.database_id, cir.cohort_definition_id, cir.inclusion_rule_mask, cir.person_count, cir.mode_id,
  dt.cdm_source_name FROM @schema.@cg_table_prefixCOHORT_INC_RESULT cir
  join @schema.@database_table_prefix@database_table dt
  on cir.database_id = dt.database_id
  {@use_cohort_def_id}?{ where cir.cohort_definition_id = @cohort_definition_id}
  ;"
  return(
    connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      cg_table_prefix = resultDatabaseSettings$cgTablePrefix,
      database_table = resultDatabaseSettings$databaseTable,
      database_table_prefix = resultDatabaseSettings$databaseTablePrefix,
      use_cohort_def_id = !is.null(cohortDefinitionId),
      cohort_definition_id = cohortDefinitionId
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
      dplyr::arrange(.data$ruleSequence)
    
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

