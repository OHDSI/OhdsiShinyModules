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
  
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          "
          .cg-top-tabs .nav-pills {
            display: flex;
            flex-wrap: wrap;
            gap: 8px;
            margin-bottom: 14px;
          }
          .cg-top-tabs .nav-pills > li {
            float: none;
          }
          .cg-top-tabs .nav-pills > li > a {
            border-radius: 999px;
            border: 1px solid #d4e0ed;
            background: #ffffff;
            color: #334155;
            font-weight: 600;
            padding: 9px 14px;
            line-height: 1.3;
            transition: all 0.2s ease;
          }
          .cg-top-tabs .nav-pills > li > a:hover {
            border-color: #93c5fd;
            background: #eff6ff;
            color: #1d4ed8;
          }
          .cg-top-tabs .nav-pills > li.active > a,
          .cg-top-tabs .nav-pills > li.active > a:hover,
          .cg-top-tabs .nav-pills > li.active > a:focus {
            background: #2563eb;
            border-color: #2563eb;
            color: #ffffff;
            box-shadow: 0 2px 8px rgba(37, 99, 235, 0.25);
          }
          "
        )
      )
    ),
    shinydashboard::box(
      status = 'info',
      width = '100%',
      title = shiny::span(shiny::icon("user-gear"), 'Cohorts'),
      solidHeader = TRUE,
      shiny::div(
        class = "cg-top-tabs",
        shiny::tabsetPanel(
          id = ns("cohortGeneratorTabs"),
          type = "pills",
          shiny::tabPanel(
            title = shiny::tagList(shiny::icon("table"), "Cohort Counts"),
            shinydashboard::box(
              collapsible = T,
              collapsed = F,
              width = '100%',
              title = shiny::span(shiny::icon("chart-bar"), 'Cohort Sizes Across Databases'),
              shiny::uiOutput(ns("cohortCountsContent")),
              shiny::uiOutput(ns("cohortCountsControls")),
              shiny::uiOutput(ns("cohortCountsSearch")),
              shiny::uiOutput(ns("cohortCountsTable"))
            )
          ),
          shiny::tabPanel(
            title = shiny::tagList(shiny::icon("cogs"), "Generation Status"),
            shinydashboard::box(
              collapsible = T,
              collapsed = F,
              width = '100%',
              resultTableViewer(
                ns("cohortGeneration")
              )
            )
          ),
          shiny::tabPanel(
            title = shiny::tagList(shiny::icon("file-text"), "Definition and Attrition"),
            shinydashboard::box(
              collapsible = T,
              collapsed = F,
              width = '100%',
              title = shiny::span(shiny::icon("gear"), 'Options'),
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
                  shiny::uiOutput(ns("attritionInputsText")),
                  shiny::uiOutput(ns("attritionOutputTable")),
                  shiny::uiOutput(ns("attritionOutputPlot"))
                )
              )
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
      
      # Helpers
      #---------
      formatYesorno <- function(value) {
        # Render as an X mark or check mark
        if (value == "COMPLETE") "\u2714\ufe0f Yes" #if generation complete then green check mark with "yes"
        else "\u274c No" #if not then red x with "no"
      }
      
      resultsSchema <- resultDatabaseSettings$schema
      #---------
      
      # COHORT COUNTS
      #---------
      data <- getCohortGeneratorCohortCounts(
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings
      ) %>%
        dplyr::select("databaseName",
                      "cohortId",
                      "cohortName",
                      "cohortSubjects",
                      "cohortEntries")
      
      cohortCountsColDefs = list(
        databaseName = reactable::colDef(
          name = "Database Name",
          header = withTooltip(
            "Database Name",
            "The name of the database"
          )),
        cohortId = reactable::colDef(
          name = "Cohort ID",
          header = withTooltip(
            "Cohort ID",
            "The unique numeric identifier of the cohort"
          )),
        cohortName = reactable::colDef(
          name = "Cohort Name",
          header = withTooltip(
            "Cohort Name",
            "The name of the cohort"
          )),
        cohortSubjects = reactable::colDef(
          name = "Number of Subjects",
          header = withTooltip(
            "Number of Subjects",
            "The number of distinct subjects in the cohort"
          ),
          format = reactable::colFormat(separators = TRUE
          )),
        cohortEntries = reactable::colDef(
          name = "Number of Records",
          header = withTooltip(
            "Number of Records",
            "The number of records in the cohort"
          ),
          format = reactable::colFormat(separators = TRUE
          ))
      )
      
      # Reactive value to track which count view is active
      activeCountView <- shiny::reactiveVal('subjects')
      
      # Pivot data for subjects view
      dataSubjectsPivot <- reactive({
        data %>%
          dplyr::select(cohortId, cohortName, databaseName, cohortSubjects) %>%
          tidyr::pivot_wider(
            names_from = databaseName,
            values_from = cohortSubjects,
            values_fill = 0
          ) %>%
          dplyr::arrange(cohortId)
      })
      
      # Pivot data for records view
      dataRecordsPivot <- reactive({
        data %>%
          dplyr::select(cohortId, cohortName, databaseName, cohortEntries) %>%
          tidyr::pivot_wider(
            names_from = databaseName,
            values_from = cohortEntries,
            values_fill = 0
          ) %>%
          dplyr::arrange(cohortId)
      })
      
      # Handle toggle buttons
      shiny::observeEvent(input$ccSubjects, {
        activeCountView('subjects')
      })
      
      shiny::observeEvent(input$ccRecords, {
        activeCountView('records')
      })
      
      # Create dynamic column definitions based on pivoted data (same columns for both views)
      cohortCountsColDefsPivoted <- reactive({
        # Use subjects data to determine columns (same columns in both views)
        pivotedData <- dataSubjectsPivot()
        
        # Get database column names (all except cohortId and cohortName)
        dbCols <- setdiff(colnames(pivotedData), c('cohortId', 'cohortName'))
        
        # Create base column defs
        colDefs <- list(
          cohortId = reactable::colDef(
            name = "ID",
            width = 60,
            header = withTooltip("ID", "Unique cohort identifier")
          ),
          cohortName = reactable::colDef(
            name = "Cohort Name",
            minWidth = 200,
            header = withTooltip("Cohort Name", "Name of the cohort")
          )
        )
        
        # Add database columns
        for (db in dbCols) {
          colDefs[[db]] <- reactable::colDef(
            name = db,
            align = "center",
            format = reactable::colFormat(separators = TRUE),
            header = withTooltip(db, paste0("Count in ", db))
          )
        }
        
        return(colDefs)
      })
      
      # Render the style (static, only once)
      output$cohortCountsContent <- shiny::renderUI({
        shiny::tags$style(
          '
          .cc-controls {
            display: flex;
            align-items: center;
            gap: 16px;
            margin-bottom: 20px;
            padding: 16px;
            background: #f8fbff;
            border-radius: 8px;
            border: 1px solid #dbe5f1;
          }
          .cc-controls-label {
            font-weight: 600;
            color: #132238;
            margin: 0;
          }
          .cc-toggle-group {
            display: flex;
            gap: 4px;
            background: #ffffff;
            border: 1px solid #d4e0ed;
            border-radius: 6px;
            padding: 2px;
          }
          .cc-toggle-btn {
            padding: 6px 12px;
            border: none;
            background: transparent;
            color: #475569;
            font-size: 13px;
            font-weight: 500;
            cursor: pointer;
            border-radius: 4px;
            transition: all 0.15s ease;
          }
          .cc-toggle-btn.active {
            background: #2563eb;
            color: #ffffff;
          }
          .cc-toggle-btn:hover:not(.active) {
            background: #e5ecf6;
          }
          .cc-search-box {
            margin-bottom: 16px;
          }
          .cc-search-input {
            width: 100%;
            max-width: 300px;
            padding: 8px 12px;
            border: 1px solid #d4e0ed;
            border-radius: 6px;
            font-size: 13px;
            box-sizing: border-box;
          }
          .cc-search-input:focus {
            outline: none;
            border-color: #2563eb;
            box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.1);
          }
          '
        )
      })
      
      # Render the toggle buttons (updates when view changes)
      output$cohortCountsControls <- shiny::renderUI({
        subjClass <- paste('cc-toggle-btn', if(activeCountView() == 'subjects') 'active' else '')
        recClass <- paste('cc-toggle-btn', if(activeCountView() == 'records') 'active' else '')
        
        shiny::div(
          class = 'cc-controls',
          shiny::p(class = 'cc-controls-label', 'View:'),
          shiny::div(
            class = 'cc-toggle-group',
            shiny::actionButton(
              session$ns('ccSubjects'),
              'Subject Counts',
              class = subjClass
            ),
            shiny::actionButton(
              session$ns('ccRecords'),
              'Record Counts',
              class = recClass
            )
          )
        )
      })
      
      # Render the search box (non-reactive to view changes)
      output$cohortCountsSearch <- shiny::renderUI({
        shiny::div(
          class = 'cc-search-box',
          shiny::tags$input(
            type = 'text',
            id = session$ns('ccSearch'),
            class = 'cc-search-input',
            placeholder = 'Search by cohort name or ID...'
          )
        )
      })
      
      # Render the table output (non-reactive to view changes)
      output$cohortCountsTable <- shiny::renderUI({
        shinydashboard::box(
          status = 'primary',
          width = '100%',
          collapsible = F,
          reactable::reactableOutput(session$ns('cohortCountsPivoted'))
        )
      })
      
      # Filtered data based on search input
      filteredCohortCounts <- reactive({
        pivotedData <- if(activeCountView() == 'subjects') dataSubjectsPivot() else dataRecordsPivot()
        searchTerm <- tolower(input$ccSearch %||% '')
        
        if (searchTerm == '') {
          return(pivotedData)
        }
        
        # Filter by cohort ID or name
        mask <- grepl(searchTerm, tolower(pivotedData$cohortId), fixed = TRUE) |
                grepl(searchTerm, tolower(pivotedData$cohortName), fixed = TRUE)
        
        return(pivotedData[mask, , drop = FALSE])
      })
      
      output$cohortCountsPivoted <- reactable::renderReactable({
        reactable::reactable(
          filteredCohortCounts(),
          columns = cohortCountsColDefsPivoted(),
          striped = TRUE,
          highlight = TRUE,
          sortable = TRUE,
          filterable = FALSE,
          resizable = TRUE,
          defaultPageSize = 10,
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(5, 10, 25, 50),
          defaultColDef = reactable::colDef(align = "center"),
          compact = TRUE,
          theme = reactable::reactableTheme(
            borderColor = "#e5ecf6",
            headerStyle = list(
              backgroundColor = "#f8fbff",
              borderColor = "#d4e0ed",
              fontWeight = "600",
              color = "#132238"
            ),
            rowStyle = list(
              borderBottom = "1px solid #f1f5f9"
            ),
            stripedColor = "#fafbff",
            highlightColor = "#e5ecf6"
          )
        )
      })
      
      # Observer to update table data when toggling views or searching
      shiny::observe({
        reactable::updateReactable(
          outputId = session$ns('cohortCountsPivoted'),
          data = filteredCohortCounts()
        )
      }) %>%
        shiny::bindEvent(activeCountView(), input$ccSearch, ignoreInit = TRUE)
      
      # cohort count table server (keeping for compatibility if needed elsewhere)
      resultTableServer(
        id = "cohortCounts",
        df = data,
        colDefsInput = cohortCountsColDefs,
        downloadedFileName = "cohortCountsTable-",
        elementId = session$ns("cohortCountsTable")
      )
      #---------
      
      # COHORT GENERATION
      #---------
     # cohort generation table
      dataGen <- getCohortGeneratorCohortMeta(
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings
      ) %>%
        dplyr::select("databaseName",
                      "cohortId",
                      "cohortName",
                      "generationStatus",
                      "startTime",
                      "endTime",
                      "generationDuration")
      
      
      cohortGenerationColDefs <- list(
        databaseName = reactable::colDef( 
          name = "Database Name", 
          header = withTooltip(
            "Database Name", 
            "The name of the database"
          )),
        cohortId = reactable::colDef( 
          name = "Cohort ID", 
          header = withTooltip(
            "Cohort ID", 
            "The unique numeric identifier of the cohort"
          )),
        cohortName = reactable::colDef( 
          name = "Cohort Name", 
          header = withTooltip(
            "Cohort Name", 
            "The name of the cohort"
          )),
        generationStatus = reactable::colDef( 
          name = "Is the Cohort Generated?", 
          header = withTooltip(
            "Is the Cohort Generated?", 
            "Indicator of if the cohort has been generated"
          ),
          cell = formatYesorno
        ),
        startTime = reactable::colDef( 
          name = "Generation Start Time",
          header = withTooltip(
            "Generation Start Time", 
            "The time and date the cohort started generating"
          ),
          format = reactable::colFormat(suffix = " mins"
                                        #format = reactable::colFormat(datetime = TRUE
          )),
        endTime = reactable::colDef( 
          name = "Generation End Time", 
          header = withTooltip(
            "Generation End Time", 
            "The time and date the cohort finished generating"
          ),
          format = reactable::colFormat(datetime = TRUE
          )),
        generationDuration = reactable::colDef( 
          name = "Generation Duration (mins)", 
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
        downloadedFileName = "cohortGenerationTable-",
        elementId = session$ns('cohort-gen-main')
      )
      #---------
      
      
      # NEW: Cohort definition with attrition 
      #---------
      cohortDefData <- getCohortGeneratorCohortDefinition(
        connectionHandler = connectionHandler, 
        resultDatabaseSettings = resultDatabaseSettings
      )
      
      cohortDefInputs <- 1:nrow(cohortDefData)
      names(cohortDefInputs) <- cohortDefData$cohortName
      
      output$cohortDefinitionCohortSelect <- shiny::renderUI(
        shiny::tagList(
          shinyWidgets::pickerInput(
            inputId = session$ns('selectedCohortDefRow'),
            label = 'Cohort: ',
            choices = cohortDefInputs,
            selected = cohortDefInputs[1],
            multiple = FALSE,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              dropupAuto = F,
              #size = 10,
              liveSearchStyle = "contains",
              liveSearchPlaceholder = "Type here to search",
              virtualScroll = 500
            )
          ),
        shiny::actionButton(
          inputId = session$ns('generate_cohort_def'),
          label = 'Generate'
        )
      )
      )
      
    # reactive vars for all the cohort def parts  
    selectedCohortDefInputs <- shiny::reactiveVal(NULL)
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
        subset <- cohortDefData$subsetDefinitionJson[as.double(input$selectedCohortDefRow)]
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
              choices = unique(attritionData()$databaseName), 
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
          
          resultTableViewer(session$ns('attritionTable'))
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
        dplyr::filter(.data$databaseName %in% input$selectedDatabaseId & 
                        .data$modeId %in% input$selectedModeId
        )
      
      if(!is.null(selectedAttritionData)){ # or nrow > 0 ?
        
        resultTableServer(
            id = 'attritionTable',
            elementId = session$ns('cohort-gen-attrition'),
            df =  selectedAttritionData %>%
              dplyr::select(c("databaseName", "cohortName", "ruleName",
                              "personCount", "dropCount",
                              "dropPerc", "retainPerc")
              )
            ,
            colDefsInput = list(
              databaseName = reactable::colDef( 
                name = "Database Name", 
                filterable = TRUE,
                header = withTooltip(
                  "Database Name", 
                  "The name of the database"
                )),
              cohortName = reactable::colDef( 
                name = "Cohort Name", 
                filterable = TRUE,
                header = withTooltip(
                  "Cohort Name", 
                  "The name of the cohort"
                )),
              ruleName = reactable::colDef( 
                name = "Inclusion Rule Name", 
                header = withTooltip(
                  "Inclusion Rule Name", 
                  "The name of the inclusion rule"
                )),
              personCount = reactable::colDef( 
                name = "Subject/Record Count", 
                format = reactable::colFormat(separators = TRUE),
                header = withTooltip(
                  "Subject/Record Count", 
                  "The number of subjects or records (depending on your selection) remaining after the inclusion rule was applied"
                )),
              dropCount = reactable::colDef( 
                name = "Number Lost", 
                format = reactable::colFormat(separators = TRUE),
                header = withTooltip(
                  "Number Lost", 
                  "The number of subjects or records (depending on your selection) removed/lost after the inclusion rule was applied"
                )),
              dropPerc = reactable::colDef( 
                name = "Percentage Lost", 
                format = reactable::colFormat(separators = TRUE),
                header = withTooltip(
                  "Percentage Lost", 
                  "The percentage of subjects or records (depending on your selection) removed/lost after the inclusion rule was applied compared to the previous rule count"
                )),
              retainPerc = reactable::colDef( 
                name = "Percentage Retained",
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
    resultDatabaseSettings,
    cohortDefinitionId = NULL
) {
  
  result <- OhdsiReportGenerator::getCohortDefinitions(
    connectionHandler = connectionHandler,
    schema = resultDatabaseSettings$schema,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    targetIds = cohortDefinitionId
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
    resultDatabaseSettings,
    cohortDefinitionId = NULL
) {
  
  result <- OhdsiReportGenerator::getCohortCounts(
    connectionHandler = connectionHandler,
    schema = resultDatabaseSettings$schema,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    databaseTable = resultDatabaseSettings$databaseTable,
    cohortIds = cohortDefinitionId
  )
  
  return(result)
}

# is this used?
getCohortGeneratorCohortMeta <- function(
    connectionHandler, 
    resultDatabaseSettings,
    cohortDefinitionId = NULL
) {
  
  result <- OhdsiReportGenerator::getCohortMeta(
    connectionHandler = connectionHandler,
    schema = resultDatabaseSettings$schema,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    databaseTable = resultDatabaseSettings$databaseTable,
    cohortIds = cohortDefinitionId
  )
  
  result <- result %>%
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
  
  return(result)
}

# is this used?
getCohortGeneratorCohortInclusionSummary <- function(
    connectionHandler, 
    resultDatabaseSettings,
    cohortDefinitionId = NULL
) {
  
  result <- OhdsiReportGenerator::getCohortInclusionSummary(
    connectionHandler = connectionHandler,
    schema = resultDatabaseSettings$schema,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    databaseTable = resultDatabaseSettings$databaseTable,
    cohortIds = cohortDefinitionId
  )

  return(result)
}



getCohortGeneratorInclusionRules <- function(
    connectionHandler, 
    resultDatabaseSettings,
    cohortDefinitionId = NULL
) {
  
  result <- OhdsiReportGenerator::getCohortInclusionRules(
    connectionHandler = connectionHandler,
    schema = resultDatabaseSettings$schema,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    cohortIds = cohortDefinitionId
  )
  
  return(result)
}

getCohortGeneratorInclusionStats <- function(
    connectionHandler, 
    resultDatabaseSettings,
    cohortDefinitionId = NULL
) {
  
  
  result <- OhdsiReportGenerator::getCohortInclusionStats(
    connectionHandler = connectionHandler,
    schema = resultDatabaseSettings$schema,
    cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
    databaseTable = resultDatabaseSettings$databaseTable,
    cohortIds = cohortDefinitionId
  )
  
  return(result)
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
        dplyr::group_by(.data$databaseName, .data$cohortDefinitionId, .data$modeId) %>%
        dplyr::summarise(personCount = sum(.data$personCount),
        )
      
      startingCounts <- stats %>%
        dplyr::select(-c("databaseId")) %>%
        dplyr::group_by(.data$databaseName, .data$cohortDefinitionId, .data$modeId) %>%
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
  
  # change to unique as dplyr::distinct gave weird error
  attritionTableDistinct <- unique(attritionTable)

  
  #adding drop counts
  attritionTableFinal <- attritionTableDistinct %>%
    dplyr::group_by(
      .data$databaseName, 
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

