# @file characterization-DechallengeRechallenge.R
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


characterizationDechallengeRechallengeViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    
    shiny::helpText('View how often the outcome occurs just before the target stops (a positive dechallenge) and how often the outcome restarts shortly after the target restarts (positive rechallenge)'),
    
    shinydashboard::box(
      collapsible = TRUE,
      title = "Options",
      width = "100%",
      shiny::uiOutput(ns("inputs"))
    ),
    
    shiny::conditionalPanel(
      condition = 'output.showDechalRechal != 0', 
      ns = ns,

      shiny::uiOutput(ns('warning')),
      
      shinydashboard::box(
        status = 'info', 
        width = '100%',
        solidHeader = TRUE,
        resultTableViewer(ns('tableResults'))
      )
    )
  )
}

characterizationDechallengeRechallengeServer <- function(
  id, 
  connectionHandler,
  resultDatabaseSettings,
  reactiveCharacterizationTargetTable,
  reactiveOutcomeTable
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # moving the selections within module rather than shared across
      reactiveOutcomeRowId <- shiny::reactiveVal(NULL)
      reactiveCharacterizationTargetRowId <- shiny::reactiveVal(NULL)
      
      # restrict to populations with cohort comp data
      moduleCharacterizationTargetTable <- shiny::reactive({
        if(!is.null(reactiveCharacterizationTargetTable())){
          reactiveCharacterizationTargetTable() %>%
            dplyr::filter(.data$dechalRechal == 1)
        } else{
          NULL
        }
      })
      
      
      reactiveTargetRow <- shiny::reactive({
        rowId <- reactiveCharacterizationTargetRowId()
        targetTable <-  moduleCharacterizationTargetTable()
        
        if (is.null(rowId) || length(rowId) == 0 || is.null(targetTable) || nrow(targetTable) == 0) {
          return(data.frame())
        }
        
        targetTable[rowId, , drop = FALSE]
      })
      
      tableSelectionServer(
        id = 'char-pop-select-dcrc',
        table =  moduleCharacterizationTargetTable, 
        selectedRowId = reactiveCharacterizationTargetRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('table-selector-dcrc'),
        inputColumns = characterizationTargetsColumns(),
        displayColumns = characterizationTargetsColumns(), 
        selectButtonText = 'Select Population'
      )
      
      
      output$showDechalRechal <- shiny::reactive(0)
      shiny::outputOptions(output, "showDechalRechal", suspendWhenHidden = FALSE)
      allData <- shiny::reactiveVal(NULL)

      outcomeTableForSelect <- shiny::reactive({
        out <- reactiveOutcomeTable()

        if (is.null(out) || nrow(out) == 0) {
          return(data.frame())
        }

        if (!("cohortId" %in% colnames(out)) && ("cohortDefinitionId" %in% colnames(out))) {
          out$cohortId <- out$cohortDefinitionId
        }

        out
      })
      
      # if target or outcome changes hide results
      shiny::observeEvent(reactiveTargetRow(), {
        output$showDechalRechal <- shiny::reactive(0)
      })
      shiny::observeEvent(reactiveOutcomeRowId(), {
        output$showDechalRechal <- shiny::reactive(0)
      })
      
      
      # INPUTS
      output$inputs <- shiny::renderUI({ # need to make reactive?
        hasTarget <- !is.null(reactiveTargetRow()) && nrow(reactiveTargetRow()) > 0
        hasOutcome <- !is.null(reactiveOutcomeRowId()) &&
          length(reactiveOutcomeRowId()) > 0 &&
          all(reactiveOutcomeRowId() > 0)
        canGenerate <- hasTarget && hasOutcome
        
        shiny::div(
        
          tableSelectionViewer(id = session$ns('char-pop-select-dcrc')),

          if (hasTarget) {
            tableSelectionViewer(id = session$ns('outcome-table-select-dechal'))
          },

          shiny::tags$button(
            id = session$ns('generate'),
            type = 'button',
            class = if (canGenerate) 'btn btn-primary action-button' else 'btn btn-default action-button',
            disabled = if (!canGenerate) 'disabled' else NULL,
            'Generate'
          ),

          if (!canGenerate) {
            shiny::helpText('Select both a population and an outcome to enable Generate.')
          }
        )
      })
      
      # server for outcome seleciton table
      tableSelectionServer(
        id = 'outcome-table-select-dechal',
        table = shiny::reactive(outcomeTableForSelect() %>%
                                  dplyr::select("cohortName", "cohortId") %>%
                                  dplyr::relocate("cohortId", .after = "cohortName")
        ), 
        selectedRowId = reactiveOutcomeRowId,
        selectMultiple = FALSE, 
        elementId = session$ns('table-outcome-selector'),
        inputColumns = characterizationOutcomeDisplayColumns(),
        displayColumns = characterizationOutcomeDisplayColumns(), 
        selectButtonText = 'Select Outcome'
      )
      

      # wait for generate to extract data
      shiny::observeEvent(input$generate, {
        targetRow <- reactiveTargetRow()
        outcomeTable <- outcomeTableForSelect()
        outcomeRowId <- reactiveOutcomeRowId()

        hasTarget <- !is.null(targetRow) && nrow(targetRow) > 0
        hasOutcome <- !is.null(outcomeRowId) && length(outcomeRowId) > 0 &&
          all(outcomeRowId > 0) && !is.null(outcomeTable) &&
          nrow(outcomeTable) >= max(outcomeRowId)

        if (!hasTarget || !hasOutcome) {
          output$showDechalRechal <- shiny::reactive(0)
          allData(NULL)
          shiny::showNotification('Select both a population and an outcome before generating.')
          return(NULL)
        }
        
        reactiveOutcomeRow <- outcomeTableForSelect()[reactiveOutcomeRowId(), , drop = FALSE]
        
        if(is.null(reactiveTargetRow()) | is.null(reactiveOutcomeRow)){
          output$showDechalRechal <- shiny::reactive(0)
          allData(NULL)
          return(NULL)
        } else{
          
          if(nrow(reactiveTargetRow()) > 0 & nrow(reactiveOutcomeRow) > 0 ){
            
            output$showDechalRechal <- shiny::reactive(1)
            
            allData(
              getDechalRechalInputsData(
                characterizationTargetId = reactiveTargetRow()$characterizationTargetId,
                outcomeId = reactiveOutcomeRow$cohortId,
                connectionHandler = connectionHandler,
                resultDatabaseSettings
              )
            )
            
            #========
            
          } else{
            shiny::showNotification('Must have target and outcome set')
          }
          
          
        }
        })
      
      # make details reactive and move this outside observe event
      tableOutputs <- resultTableServer(
        id = "tableResults", 
        df = allData,
        details = shiny::reactive(data.frame(
          target = reactiveTargetRow()$cohortName,
          outcome = outcomeTableForSelect()[reactiveOutcomeRowId(),]$cohortName,
          Analysis = 'Exposed Cases Summary - Dechallenge-Rechallenge'
        ))(),
        downloadedFileName = 'dechallege-rechallenge',
        colDefsInput = characteriationDechalRechalColDefs(),
        addActions = c('fails'), 
        elementId = session$ns('dechal-rechal-main')
      )
      

      failData <- shiny::reactiveVal(NULL)
      shiny::observeEvent(tableOutputs$actionCount(), {
        if(!is.null(tableOutputs$actionType())){
          if(tableOutputs$actionType() == 'fails'){
            result <- getDechalRechalFailData(
              characterizationTargetId = reactiveTargetRow()$characterizationTargetId,
              outcomeId = outcomeTableForSelect()[reactiveOutcomeRowId(),]$cohortId,
              databaseId = allData()$databaseId[tableOutputs$actionIndex()$index], # update?
              dechallengeStopInterval = allData()$dechallengeStopInterval[tableOutputs$actionIndex()$index],
              dechallengeEvaluationWindow = allData()$dechallengeEvaluationWindow[tableOutputs$actionIndex()$index],
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings
            )
            failData(result)
            # module to show failed plots
            
            if(nrow(result) > 0){
            shiny::showModal(
              shiny::modalDialog(
                title = paste0("Failed Plots: "),
                size = "l",
                shiny::plotOutput(session$ns('dechalplot')),
                easyClose = TRUE,
                footer = NULL
              )
            )
            } else{
              shiny::showNotification("No fails to display")
            }
          }
        }
      })
          
          
      # do the plots reactively
        output$dechalplot <- shiny::renderPlot(
          plotDechalRechal(
            dechalRechalData = failData()
          )
        )
        
        
      
      return(invisible(NULL))
      
    }
  )
}

# pulls all data for a target and outcome
getDechalRechalInputsData <- function(
    characterizationTargetId,
  outcomeId,
  connectionHandler,
  resultDatabaseSettings
){
  
  if(is.null(characterizationTargetId)){
    return(NULL)
  }
  
  shiny::withProgress(message = 'Extracting DECHALLENGE_RECHALLENGE data', value = 0, {
  
    
    data <- OhdsiReportGenerator::getDechallengeRechallenge(
      connectionHandler = connectionHandler, 
      schema = resultDatabaseSettings$schema, 
      cTablePrefix = resultDatabaseSettings$cTablePrefix, 
      cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
      databaseTable = resultDatabaseSettings$databaseTable, 
      characterizationTargetIds = characterizationTargetId, 
      outcomeIds = outcomeId
        )
  
  shiny::incProgress(3/3, detail = paste("Finished"))
  
  })
  
  return(data)
}


getDechalRechalFailData <- function(
    characterizationTargetId,
  outcomeId,
  databaseId,
  dechallengeStopInterval,
  dechallengeEvaluationWindow,
  connectionHandler,
  resultDatabaseSettings
){

  if(is.null(characterizationTargetId)){
    return(NULL)
  }

  shiny::withProgress(message = 'Extracting FAILLED DECHALLENGE_RECHALLENGE data', value = 0, {
    
    shiny::incProgress(1/3, detail = paste("Fetching data"))
    
    
    data <- OhdsiReportGenerator::getDechallengeRechallengeFails(
      connectionHandler = connectionHandler, 
      schema = resultDatabaseSettings$schema,
      cTablePrefix = resultDatabaseSettings$cTablePrefix,
      characterizationTargetId = characterizationTargetId, 
      outcomeId = outcomeId,
      databaseId = databaseId,
      dechallengeStopInterval = dechallengeStopInterval,
      dechallengeEvaluationWindow = dechallengeEvaluationWindow
    )
    
    shiny::incProgress(3/3, detail = paste("Finished"))
    
  })
  
  return(data)
  
}

plotDechalRechal <- function(
  dechalRechalData,
  i = 1
){
  
  if(is.null(dechalRechalData)){
    return(NULL)
  }
  
  shiny::withProgress(message = 'Plotting DECHALLENGE_RECHALLENGE', value = 0, {
    
    
    #order the data so that cases are in order of exposure/outcome offsets
    dechalRechalData <- dechalRechalData %>% 
      dplyr::arrange(
        .data$dechallengeExposureStartDateOffset, 
        .data$dechallengeOutcomeStartDateOffset, 
        .data$rechallengeExposureStartDateOffset, 
        .data$rechallengeOutcomeStartDateOffset
        )
    
    #give temp ID for purposes of allowing plotting in order of sort
    ##cases <- data.frame(subjectId = unique(dechalRechalData$subjectId))
    cases <- data.frame(personKey = unique(dechalRechalData$personKey))
    cases <- tibble::rowid_to_column(cases, "PID")
    dechalRechalData <- dechalRechalData %>% dplyr::inner_join(cases)
    
    
      i50 <- min(i + 49,length(cases$personKey))
      caseSubset <- cases[i:i50,2]
      
      #grab the cases to plot      
      rdcsSubset <- dechalRechalData %>% 
        dplyr::filter(
          .data$personKey %in% caseSubset
          )
      
      #small datasets to fit ggplot
      dechallengeExposure <- rdcsSubset %>%
        dplyr::select(
          c(
          "PID", 
          "characterizationTargetId", 
          "outcomeCohortDefinitionId", 
          "personKey", 
          "dechallengeExposureNumber",
          "dechallengeExposureStartDateOffset", 
          "dechallengeExposureEndDateOffset"
          )
          ) %>%
        dplyr::mutate(
          eventId = .data$personKey*1000 + .data$dechallengeExposureNumber
          ) %>%
        dplyr::rename(
          eventNumber = "dechallengeExposureNumber", 
          eventStart = "dechallengeExposureStartDateOffset", 
          eventEnd = "dechallengeExposureEndDateOffset") %>%
        dplyr::distinct() %>%
        tidyr::pivot_longer(
          cols = c("eventStart", "eventEnd"),
          names_to = "eventDateType",
          values_to = "offset"
        )
      
      dechallengeStarts <- dechallengeExposure %>% 
        dplyr::filter(.data$eventDateType == "eventStart")
      
      dechallengeOutcome <- rdcsSubset %>%
        dplyr::select(
          c(
          "PID", 
          "characterizationTargetId", 
          "outcomeCohortDefinitionId", 
          "personKey", 
          "dechallengeOutcomeNumber", 
          "dechallengeOutcomeStartDateOffset"
          )
          ) %>%
        dplyr::mutate(
          eventId = .data$personKey*1000 + .data$dechallengeOutcomeNumber
          ) %>%
        dplyr::rename(
          eventNumber = "dechallengeOutcomeNumber", 
          offset = "dechallengeOutcomeStartDateOffset"
          ) %>%
        dplyr::distinct()
      
      
      rechallengeExposure <- rdcsSubset %>%
        dplyr::select(
          c(
          "PID", 
          "characterizationTargetId", 
          "outcomeCohortDefinitionId", 
          "personKey", 
          "rechallengeExposureNumber", 
          "rechallengeExposureStartDateOffset", 
          "rechallengeExposureEndDateOffset"
          )
          ) %>%
        dplyr::mutate(
          eventId = .data$personKey*1000 + .data$rechallengeExposureNumber
          ) %>%
        dplyr::rename(
          eventNumber = "rechallengeExposureNumber", 
          eventStart = "rechallengeExposureStartDateOffset", 
          eventEnd = "rechallengeExposureEndDateOffset"
          ) %>%
        dplyr::distinct() %>%
        tidyr::pivot_longer(
          cols = c("eventStart", "eventEnd"),
          names_to = "eventDateType",
          values_to = "offset"
        )
      
      rechallengeStarts <- rechallengeExposure %>% 
        dplyr::filter(
          .data$eventDateType == "eventStart"
          )
      
      
      rechallengeOutcome <- rdcsSubset %>%
        dplyr::select(
          c(
          "PID", 
          "characterizationTargetId", 
          "outcomeCohortDefinitionId", 
          "personKey", 
          "rechallengeOutcomeNumber", 
          "rechallengeOutcomeStartDateOffset"
          )
          ) %>%
        dplyr::mutate(
          eventId = .data$personKey*1000 + .data$rechallengeOutcomeNumber
          ) %>%
        dplyr::rename(
          eventNumber = "rechallengeOutcomeNumber", 
          offset = "rechallengeOutcomeStartDateOffset") %>%
        dplyr::distinct()
      
      shiny::incProgress(1/2, detail = paste("Formatted data, now plotting"))
      
      labelSize <- 5
      # ggplot lays out dechallenge/rechallenge exposure eras and points for each outcome
      plot <- ggplot2::ggplot(
        data = dechallengeExposure, 
        ggplot2::aes(
          x = .data$offset, 
          y = .data$PID, 
          label = .data$eventNumber
          )
        ) +
        #ggplot2::geom_text(size = labelSize) +
        ggplot2::geom_line(
          data = dechallengeExposure, 
          ggplot2::aes(group = .data$eventId), 
          size = 2, 
          color = "blue"
          ) +
        ggplot2::geom_line(
          data = rechallengeExposure, 
          ggplot2::aes(group = .data$eventId), 
          size = 2, 
          color = "navyblue"
          ) +
        ggplot2::geom_point(
          data = dechallengeOutcome, 
          color = "darkorange", 
          size = 2, 
          shape = 8
          ) +
        ggplot2::geom_point(
          data = rechallengeOutcome, 
          color = "orangered", 
          size = 2, 
          shape = 8
          ) +
        ggplot2::geom_text(
          data = dechallengeStarts, 
          hjust = 1, 
          vjust = 0, 
          color = "blue", 
          size = labelSize
          ) +
        ggplot2::geom_text(
          data = rechallengeStarts, 
          hjust = 1, 
          vjust = 0, 
          color = "navyblue", 
          size = labelSize
          ) +
        ggplot2::geom_text(
          data = dechallengeOutcome, 
          color = "darkorange", 
          hjust = -.5, 
          vjust = -.5, 
          size = labelSize
          ) +
        ggplot2::geom_text(
          data = rechallengeOutcome, 
          color = "orangered", 
          hjust = -.5, 
          vjust = -.5, 
          size = labelSize
          ) +
        ggplot2::scale_y_reverse() +
        ggplot2::theme_bw() + 
        ggplot2::theme(
          panel.border = ggplot2::element_blank(), 
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(), 
          axis.line = ggplot2::element_line(colour = "black"),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          text = ggplot2::element_text(size = 20) # testing
          ) +
        ggplot2::xlab("Time from first exposure") + 
        ggplot2::ylab("Each horizontal line is one person")
  
  shiny::incProgress(2/2, detail = paste("Finished"))
  
  })
  
  
    return(plot)
}


characteriationDechalRechalColDefs <- function(){
  result <- list(
    databaseName = reactable::colDef(
      name = "Database",
      header = withTooltip("Database",
                           "Name of the database"),
      filterable = TRUE
    ),
    databaseId = reactable::colDef(
      show = FALSE
    ),
    characterizationTargetId = reactable::colDef(
      show = FALSE
    ),
    targetId = reactable::colDef(
      show = FALSE
    ),
    targetName = reactable::colDef(show = FALSE),
    limitToFirstInNDays = reactable::colDef(show = FALSE),    
    minPriorObservation= reactable::colDef(show = FALSE),
    nestingCohortId = reactable::colDef(show = FALSE),
    nestingName = reactable::colDef(show = FALSE),
    minAge = reactable::colDef(show = FALSE),
    maxAge = reactable::colDef(show = FALSE),
    studyStart= reactable::colDef(show = FALSE),
    studyEnd = reactable::colDef(show = FALSE),
    genderConceptIds= reactable::colDef(show = FALSE),
    outcomeId = reactable::colDef(
      show = FALSE
    ),
    outcomeName = reactable::colDef(
      show = FALSE
    ),
    dechallengeStopInterval = reactable::colDef(
      name = "Dechallenge Stop Interval",
      header = withTooltip("Dechallenge Stop Interval",
                           "An integer specifying the how much time to add to the cohort_end when determining whether the event starts during cohort and ends after"),
      filterable = TRUE
    ),
    dechallengeEvaluationWindow = reactable::colDef(
      name = "Dechallenge Evaluation Window",
      header = withTooltip("Dechallenge Evaluation Window",
                           "A period of time evaluated for outcome recurrence after discontinuation of exposure, among patients with challenge outcomes"),
      filterable = TRUE
    ), 
    numExposureEras = reactable::colDef(
      name = "# of Exposure Eras",
      header = withTooltip("# of Exposure Eras",
                           "Distinct number of exposure events (i.e. drug eras) in a given target cohort"),
      cell = function(value) {
        # Add < if cencored
        if (value < 0 ) paste("<", abs(value)) else abs(value)
      }
    ),
    numPersonsExposed = reactable::colDef(
      name = "# of Exposed Persons",
      header = withTooltip("# of Exposed Persons",
                           "Distinct nuber of people exposed in target cohort. A person must have at least 1 day exposure to be included"),
      cell = function(value) {
        # Add < if cencored
        if (value < 0 ) paste("<", abs(value)) else abs(value)
      }
    ),
    numCases = reactable::colDef(
      name = "# of Cases",
      header = withTooltip("# of Cases",
                           "Distinct number of persons in outcome cohort. A person must have at least 1 day of observation time to be included"),
      cell = function(value) {
        # Add < if cencored
        if (value < 0 ) paste("<", abs(value)) else abs(value)
      }
    ),
    dechallengeAttempt = reactable::colDef(
      name = "# of Dechallenge Attempts",
      header = withTooltip("# of Dechallenge Attempts",
                           "Distinct count of people with observable time after discontinuation of the exposure era during which the challenge outcome occurred"),
      cell = function(value) {
        # Add < if cencored
        if (value < 0 ) paste("<", abs(value)) else abs(value)
      }
    ),
    dechallengeFail = reactable::colDef(
      name = "# of Dechallenge Fails",
      header = withTooltip("# of Dechallenge Fails",
                           "Among people with challenge outcomes, the distinct number of people with outcomes during dechallengeEvaluationWindow"),
      cell = function(value) {
        # Add < if cencored
        if (value < 0 ) paste("<", abs(value)) else abs(value)
      }
    ),
    dechallengeSuccess = reactable::colDef(
      name = "# of Dechallenge Successes",
      header = withTooltip("# of Dechallenge Successes",
                           "Among people with challenge outcomes, the distinct number of people without outcomes during the dechallengeEvaluationWindow"),
      cell = function(value) {
        # Add < if cencored
        if (value < 0 ) paste("<", abs(value)) else abs(value)
      }
    ),
    rechallengeAttempt = reactable::colDef(
      name = "# of Rechallenge Attempts",
      header = withTooltip("# of Rechallenge Attempts",
                           "Number of people with a new exposure era after the occurrence of an outcome during a prior exposure era"),
      cell = function(value) {
        # Add < if cencored
        if (value < 0 ) paste("<", abs(value)) else abs(value)
      }
    ),
    rechallengeFail = reactable::colDef(
      name = "# of Rechallenge Fails",
      header = withTooltip("# of Rechallenge Fails",
                           "Number of people with a new exposure era during which an outcome occurred, after the occurrence of an outcome during a prior exposure era"),
      cell = function(value) {
        # Add < if cencored
        if (value < 0 ) paste("<", abs(value)) else abs(value)
      }
    ),
    rechallengeSuccess = reactable::colDef(
      name = "# of Rechallenge Successes",
      header = withTooltip("# of Rechallenge Successes",
                           "Number of people with a new exposure era during which an outcome did not occur, after the occurrence of an outcome during a prior exposure era"),
      cell = function(value) {
        # Add < if cencored
        if (value < 0 ) paste("<", abs(value)) else abs(value)
      }
    ),
    pctDechallengeAttempt = reactable::colDef(
      name = "% of Dechallenge Attempts",
      header = withTooltip("% of Dechallenge Attempts",
                           "Percent of people with observable time after discontinuation of the exposure era during which the challenge outcome occurred"),
      format = reactable::colFormat(digits = 2, percent = TRUE)
    ),
    pctDechallengeSuccess = reactable::colDef(
      name = "% of Dechallenge Success",
      header = withTooltip("% of Dechallenge Success",
                           "Among people with challenge outcomes, the percent of people with outcomes during dechallengeEvaluationWindow"),
      format = reactable::colFormat(digits = 2, percent = TRUE)
    ),
    pctDechallengeFail = reactable::colDef(
      name = "% of Dechallenge Fail",
      header = withTooltip("% of Dechallenge Fail",
                           "Among people with challenge outcomes, the percent of people without outcomes during the dechallengeEvaluationWindow"),
      format = reactable::colFormat(digits = 2, percent = TRUE)
    ),
    pctRechallengeAttempt = reactable::colDef(
      name = "% of Rechallenge Attempts",
      header = withTooltip("% of Rechallenge Attempts",
                           "Percent of people with a new exposure era after the occurrence of an outcome during a prior exposure era"),
      format = reactable::colFormat(digits = 2, percent = TRUE)
    ),
    pctRechallengeSuccess = reactable::colDef(
      name = "% of Rechallenge Success",
      header = withTooltip("% of Rechallenge Success",
                           "Percent of people with a new exposure era during which an outcome occurred, after the occurrence of an outcome during a prior exposure era"),
      format = reactable::colFormat(digits = 2, percent = TRUE)
    ),
    pctRechallengeFail = reactable::colDef(
      name = "% of Rechallenge Fail",
      header = withTooltip("% of Rechallenge Fail",
                           "Percent of people with a new exposure era during which an outcome did not occur, after the occurrence of an outcome during a prior exposure era"),
      format = reactable::colFormat(digits = 2, percent = TRUE)
    )
  )
  return(result)
}
