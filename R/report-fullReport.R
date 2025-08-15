fullReportViewer <- function(
    id = 'fullReport'
){
  
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    title = "Full Report", 
    solidHeader = TRUE, 
    width = 12,
    
    tableSelectionViewer(id = ns('target-select')),
    
    shiny::conditionalPanel(
      condition = "output.showIndications != 0", 
      ns = ns,
      tableSelectionViewer(id = ns('indications-select'))
    ),
    
    shiny::conditionalPanel(
      condition = "output.showOutcomes != 0", 
      ns = ns,
      tableSelectionViewer(id = ns('outcome-select'))
    ),
    
    shiny::conditionalPanel(
      condition = "output.showGenerate != 0", 
      ns = ns,
    shiny::uiOutput(ns("generateSelection"))
    )
    
  )
}

fullReportServer <- function(
    id = 'fullReport',
    connectionHandler = NULL,
    resultDatabaseSettings = NULL,
    server = Sys.getenv("RESULTS_SERVER"), 
    username = Sys.getenv("RESULTS_USER"), 
    password = Sys.getenv("RESULTS_PASSWORD"), 
    dbms = Sys.getenv("RESULTS_DBMS")
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # get input options
      selectedTargetRowId <- shiny::reactiveVal(0)
      selectedIndicationRowIds <- shiny::reactiveVal(0)
      selectedOutcomeRowIds <- shiny::reactiveVal(0)
      selections <- shiny::reactiveVal(NULL)
      showDownload <- shiny::reactiveVal(FALSE)
      
      
      output$showIndications <- shiny::reactive(0)
      shiny::outputOptions(output, "showIndications", suspendWhenHidden = FALSE)
      output$showOutcomes <- shiny::reactive(0)
      shiny::outputOptions(output, "showOutcomes", suspendWhenHidden = FALSE)
      output$showGenerate <- shiny::reactive(0)
      shiny::outputOptions(output, "showGenerate", suspendWhenHidden = FALSE)
      
      targets <- OhdsiReportGenerator::getTargetTable(
        connectionHandler = connectionHandler,
        schema = resultDatabaseSettings$schema
      )
      fullReportTargets <- processFullReportTargets(targets)
      
      indications <- shiny::reactiveVal(NULL)
      outcomes <- shiny::reactiveVal(NULL)
      
      
      ## Target Select
      #============================
      tableSelectionServer(
        id = 'target-select',
        table = shiny::reactive(fullReportTargets), # must be reactive
        selectedRowId = selectedTargetRowId, # must be reactive
        helpText = 'Click the button to select your targets',
        selectMultiple = FALSE,
        inputColumns = NULL,
        #displayColumns = inputColumns,
        elementId = session$ns('target-select-element'),
        selectButtonText = 'Select Target/s',
        tableReset = shiny::reactive(0)
        )
      
      
      tableSelectionServer(
        id = 'indications-select',
        table = indications, # must be reactive
        selectedRowId = selectedIndicationRowIds, # must be reactive
        helpText = 'Click the button to select your outcome',
        selectMultiple = TRUE,
        inputColumns = NULL,
        #displayColumns = inputColumns,
        elementId = session$ns('indication-select-element'),
        selectButtonText = 'Select Indications/s',
        tableReset = shiny::reactive(0)
      )
      
      tableSelectionServer(
        id = 'outcome-select',
        table = outcomes, # must be reactive
        selectedRowId = selectedOutcomeRowIds, # must be reactive
        helpText = 'Click the button to select your outcome',
        selectMultiple = TRUE,
        inputColumns = NULL,
        #displayColumns = inputColumns,
        elementId = session$ns('outcome-select-element'),
        selectButtonText = 'Select Outcome/s',
        tableReset = shiny::reactive(0)
      )
      
      shiny::observeEvent(
        selectedTargetRowId(),
        {
          
          output$showIndications <- shiny::reactive(0)
          output$showOutcomes <- shiny::reactive(0)
          output$showGenerate <- shiny::reactive(0)
          
          selectedIndicationRowIds(0)
          selectedOutcomeRowIds(0)
          
          if(selectedTargetRowId() != 0){
            output$showIndications <- shiny::reactive(1)
          
            indicationIds <- fullReportTargets$indicationIds[selectedTargetRowId()]
            indicationIds <- strsplit(indicationIds, split = ',')[[1]]
            
            indicationTemp <- targets %>%
              dplyr::filter(
                .data$cohortId %in% indicationIds
              ) %>%
              dplyr::select(
                "cohortName",
                "cohortId"
              )
            
            indications(
              rbind(
                data.frame(
                  cohortName = c('Any', 'None'),
                  cohortId = c(-1, -1)
                ),
                indicationTemp
              )
            )
          
          }
          
        }
      )
      
      
      shiny::observeEvent(
        selectedIndicationRowIds(),
        {
          
          selectedOutcomeRowIds(0)
          
          output$showOutcomes <- shiny::reactive(0)
          output$showGenerate <- shiny::reactive(0)
          
          if(selectedIndicationRowIds()[1] != 0){
            
            output$showOutcomes  <- shiny::reactive(1)
            
            parentId <- fullReportTargets$cohortId[selectedTargetRowId()]
            indicationVal <- indications()[selectedIndicationRowIds(),]
            
            if('Any' %in% indicationVal$cohortName){
              targetIds <- unique(targets$cohortId[targets$subsetParent %in% parentId])
            } else{
              targetIds <- unique(targets$cohortId[(targets$subsetCohortId %in% indicationVal$cohortId) & 
                                                               targets$subsetParent == parentId ])
              if('None' %in% indicationVal$cohortName){
                targetIds <- unique(c(targetIds, parentId))
              }
            }
            
            # TODO remove - used for testing
            print(targetIds)
            
            # get outcome options
            outcomeVal <- OhdsiReportGenerator::getOutcomeTable(
              connectionHandler = connectionHandler,
              schema = resultDatabaseSettings$schema, 
              targetId = targetIds
            ) %>% 
              dplyr::select("cohortName", "cohortId", "cohortIncidence", "riskFactors", "cohortMethod", "selfControlledCaseSeries", "prediction" )
            
            outcomes(outcomeVal)
            
          }
          
        }
      )
      
      
      shiny::observeEvent(
        selectedOutcomeRowIds(),
        {
          
          output$showGenerate <- shiny::reactive(0)
          
          if(selectedOutcomeRowIds()[1] != 0){
            
            output$showGenerate <- shiny::reactive(1)
            
            # add code to create the settings
            tempSelections <- getFullReportInputs(
              parentId = fullReportTargets$cohortId[selectedTargetRowId()],
              indicationVal = indications()[selectedIndicationRowIds(),],
              targets = targets,
              outcomes = outcomes()[selectedOutcomeRowIds(),],
              session = session
            )
            selections(tempSelections)
            
          }
          
        }
      )
      
        
    
      
      # GENERATE 
      output$generateSelection <- shiny::renderUI({
        
        shiny::div(
          
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::p('First generate the protocol and then download')
            )
          ),
          
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::p('Selected input review: '),
              
              reactable::reactable( # replace this with selection
                data = selections(),
                
                columns = list(
                  friendlyName = reactable::colDef(
                    html = TRUE,
                    cell = function(value, index) {
                      shiny::textInput(
                        inputId = session$ns(paste0("names_", index)),
                        label = NULL, 
                        value = value, 
                        placeholder = value,
                        width = '100%'
                      )
                    }
                  )
                )
                  
              )
               
            )
          ),
          
          shiny::fluidRow(
            shiny::column(
              width = 5,
              shiny::actionButton(
                inputId = session$ns("generate"), 
                label = "Generate", 
                shiny::icon('circle-plus')
              )
            ),
            shiny::column(
              width = 5,
              shiny::uiOutput(session$ns('downloadButton'))
            )
          )
          
        ) # end div
      })
      
      
      output$downloadButton <- shiny::renderUI(
        expr = if(showDownload()) {
          shiny::downloadButton(
            outputId = session$ns("download"), 
            label = "Download"
          )
        } else {
          NULL
        })
      
      # Downloadable presentation ----
      shiny::observeEvent(
        eventExpr = input$generate, 
        handlerExpr = {
          
          print(names(input))
          #getCohortNames <- unlist(lapply(1:nrow(selections()), function(ind) input[[session$ns(paste0('names_',ind))]]))
          #print(getCohortNames)
          
          shiny::withProgress(
            message = 'Cleaning files', value = 0, {
              # remove file is exists
              if(file.exists(file.path(tempdir(), 'full_report.html'))){
                file.remove(file.path(tempdir(), 'full_report.html'))
                showDownload(FALSE)
              };
              
              shiny::incProgress(0.2, detail = "Generating report")
              
              indicationTemp <- indications()[selectedIndicationRowIds(),]
              if('Any' %in% indicationTemp$cohortName){
                indicationIds <- 'All'
              } else{
                
                if('None' %in% indicationTemp$cohortName){
                  indicationTemp$cohortId[indicationTemp$cohortName %in% 'None'] <- ""
                }
                indicationIds <- unique(indicationTemp$cohortId)
              }
              
              
              print(fullReportTargets$cohortId[selectedTargetRowId()])
              print(outcomes()$cohortId[selectedOutcomeRowIds()])
              print(indicationIds)
              
              OhdsiReportGenerator::generateFullReport(
                server = server, 
                username = username, 
                password = password, 
                dbms = dbms,
                resultsSchema = resultDatabaseSettings$schema, 
                targetId = fullReportTargets$cohortId[selectedTargetRowId()], 
                outcomeIds = outcomes()$cohortId[selectedOutcomeRowIds()], 
                comparatorIds = NULL, 
                indicationIds = indicationIds,
                cohortNames = selections()$cohortName,
                cohortIds = selections()$cohortId,
                webAPI = NULL,
                authMethod = NULL,
                webApiUsername = NULL,
                webApiPassword = NULL,
                outputLocation = tempdir(), 
                outputName = 'full_report.html', 
                pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
              )
              showDownload(TRUE)
              shiny::incProgress(1, detail = "Done")
            })
        })
      
      output$download <- shiny::downloadHandler(
        filename = function() {
          paste("full_report-", Sys.Date(), ".html", sep="")
        }, 
        content = function(file){
          if(file.exists(file.path(tempdir(), 'full_report.html'))){
            file.copy(
              from = file.path(tempdir(), 'full_report.html'), 
              to = file
            )
          }
        }
      )
      
      
      
    }
  )
}



processFullReportTargets <- function(targets){

  result <- targets %>%
    dplyr::group_by(.data$subsetParent,.data$parentName) %>%
    dplyr::summarise(
      indicationIds = paste0(unique(.data$subsetCohortId), collapse = ','),
      cohortIncidence = max(.data$cohortIncidence, na.rm = TRUE),
      characterization = max(.data$riskFactors, na.rm = TRUE),
      cohortMethod = max(.data$cohortMethod, na.rm = TRUE),
      selfControlledCaseSeries = max(.data$selfControlledCaseSeries, na.rm = TRUE)
    ) %>%
    dplyr::rename(
      cohortName = "parentName",
      cohortId = "subsetParent"
    ) %>%
    dplyr::filter(
      (.data$cohortIncidence + .data$characterization + .data$cohortMethod + .data$selfControlledCaseSeries) > 0
    )
  
  return(result)
}


getFullReportInputs <- function(
  parentId,
  indicationVal,
  targets,
  outcomes,
  session
  ){
  
  if('Any' %in% indicationVal$cohortName){
    targetIds <- unique(targets$cohortId[targets$subsetParent %in% parentId])
  } else{
    targetIds <- unique(targets$cohortId[(targets$subsetCohortId %in% indicationVal$cohortId) & 
                                           targets$subsetParent == parentId ])
    if('None' %in% indicationVal$cohortName){
      targetIds <- unique(c(targetIds, parentId))
    }
  }
  
  # get indication ids
  indicationIds <- c()
  indicationNames <- c()
  if(sum(!indicationVal$cohortName %in% c('Any', 'None')) > 0){
    indicationIds <- indicationVal$cohortId[!indicationVal$cohortName %in% c('Any', 'None')]
    indicationNames <- unlist(lapply(indicationIds, function(x) targets$cohortName[targets$cohortId == x] ))
  }
  
  if(sum(indicationVal$cohortName %in% c('None')) > 0){
    indicationIds <- c(indicationIds,0)
    indicationNames <- c(indicationNames, 'No indication restriction')
  }
  
  if(sum(indicationVal$cohortName %in% c('Any')) > 0){
    indicationIds <- c(0)
    indicationNames <- 'All subsets will be included'
  }

  result <- data.frame(
    inputType = c('Target', rep('Indication', length(indicationIds)),
                  rep('Outcome', nrow(outcomes))),
    cohortId = c(parentId, indicationIds, outcomes$cohortId),
    cohortName = c(targets$cohortName[targets$cohortId == parentId]
                   ,indicationNames, outcomes$cohortName),
    friendlyName = c(targets$cohortName[targets$cohortId == parentId]
                   ,indicationNames, outcomes$cohortName)
  )
  
  return(result)
  
}

