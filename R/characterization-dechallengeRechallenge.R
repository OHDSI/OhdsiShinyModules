# @file characterization-DechallengeRechallenge.R
#
# Copyright 2024 Observational Health Data Sciences and Informatics
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


#' The module viewer for exploring Dechallenge Rechallenge results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the description Dechallenge Rechallenge module
#'
#' @export
characterizationDechallengeRechallengeViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    
    # helper component module
    infoHelperViewer(
      id = "helper",
      helpLocation= system.file("characterization-www", "help-dechallengeRechallenge.html", package = utils::packageName())
    ),
    
    # input component module
    inputSelectionViewer(id = ns('input-selection')),
    
    shiny::conditionalPanel(
      condition = 'input.generate != 0',
      ns = shiny::NS(ns("input-selection")),
      
      resultTableViewer(ns('tableResults'))
        
    
    )
  )
}


#' The module server for exploring Dechallenge Rechallenge results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler the connection to the prediction result database
#' @param resultDatabaseSettings a list containing the characterization result schema, dbms, tablePrefix, databaseTable and cgTablePrefix
#' 
#' @return
#' The server to the Dechallenge Rechallenge module
#'
#' @export
characterizationDechallengeRechallengeServer <- function(
  id, 
  connectionHandler,
  resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      

      # get the possible target ids
      bothIds <- dechalRechalGetIds(
        connectionHandler,
        resultDatabaseSettings
      )
      
      # input selection component
      inputSelected <- inputSelectionServer(
        id = "input-selection", 
        inputSettingList = list(
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 6,
            varName = 'targetId',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Target: ',
              choices = bothIds$targetIds,
              #choicesOpt = list(style = rep_len("color: black;", 999)),
              selected = bothIds$targetIds[1],
              multiple = F,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          ),
          
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 6,
            varName = 'outcomeId',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Outcome: ',
              choices = bothIds$outcomeIds,
              #choicesOpt = list(style = rep_len("color: black;", 999)),
              selected = bothIds$outcomeIds[1],
              multiple = F,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          )
        )
      )
      
      # fetch data when targetId changes
      allData <-shiny::reactive({
        getDechalRechalInputsData(
          targetId = inputSelected()$targetId,
          outcomeId = inputSelected()$outcomeId,
          connectionHandler = connectionHandler,
          resultDatabaseSettings
        )
      })

          #databases(allData$databaseId)
          #dechallengeStopInterval(allData$dechallengeStopInterval)
          #dechallengeEvaluationWindow(allData$dechallengeEvaluationWindow)
          
      tableOutputs <- resultTableServer(
        id = "tableResults", 
        df = allData,
        colDefsInput = list(  
          targetCohortDefinitionId = reactable::colDef(show = F),
          databaseId = reactable::colDef(show = F),
          outcomeCohortDefinitionId = reactable::colDef(show = F),
          
          databaseName = reactable::colDef(name = 'Database'),
          
          pctDechallengeAttempt = reactable::colDef(
            format = reactable::colFormat(digits = 2, percent = T)
          ),
          pctDechallengeSuccess = reactable::colDef(
            format = reactable::colFormat(digits = 2, percent = T)
          ),
          pctDechallengeFail = reactable::colDef(
            format = reactable::colFormat(digits = 2, percent = T)
          ),
          pctRechallengeAttempt = reactable::colDef(
            format = reactable::colFormat(digits = 2, percent = T)
          ),
          pctRechallengeSuccess = reactable::colDef(
            format = reactable::colFormat(digits = 2, percent = T)
          ),
          pctRechallengeFail = reactable::colDef(
            format = reactable::colFormat(digits = 2, percent = T)
          )
          ),
        addActions = c('fails')
      )
      
      failData <- shiny::reactiveVal(NULL)
      shiny::observeEvent(tableOutputs$actionCount(), {
        if(!is.null(tableOutputs$actionType())){
          if(tableOutputs$actionType() == 'fails'){
            result <- getDechalRechalFailData(
              targetId = inputSelected()$targetId,
              outcomeId = inputSelected()$outcomeId,
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
              showNotification("No fails to display")
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

dechalRechalGetIds <- function(
    connectionHandler,
    resultDatabaseSettings
){
  
  shiny::withProgress(message = 'Getting dechal Rechal T and O ids', value = 0, {
  
    
    sql <- "SELECT DISTINCT 
     t.COHORT_NAME as target, dr.TARGET_COHORT_DEFINITION_ID, 
     o.COHORT_NAME as outcome, dr.OUTCOME_COHORT_DEFINITION_ID 
  FROM @schema.@c_table_prefixDECHALLENGE_RECHALLENGE dr
 inner join @schema.@cg_table_prefixCOHORT_DEFINITION t
          on dr.TARGET_COHORT_DEFINITION_ID = t.COHORT_DEFINITION_ID
   inner join @schema.@cg_table_prefixCOHORT_DEFINITION o
          on dr.OUTCOME_COHORT_DEFINITION_ID = o.COHORT_DEFINITION_ID
  ;"
    
  shiny::incProgress(1/4, detail = paste("Fetching ids"))
  
  bothIds <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    cg_table_prefix = resultDatabaseSettings$cgTablePrefix
  )
  
  shiny::incProgress(3/4, detail = paste("Processing ids"))
  
  targetUnique <- bothIds %>% 
    dplyr::select(c("targetCohortDefinitionId", "target")) %>%
    dplyr::distinct()
  
  targetIds <- targetUnique$targetCohortDefinitionId
  names(targetIds) <- targetUnique$target
  
  outcomeUnique <- bothIds %>% 
    dplyr::select(c("outcomeCohortDefinitionId", "outcome")) %>%
    dplyr::distinct()
  
  outcomeIds <- outcomeUnique$outcomeCohortDefinitionId
  names(outcomeIds) <- outcomeUnique$outcome
  
  shiny::incProgress(4/4, detail = paste("Finished"))
  
  })
  
  return(
    list(
      targetIds = targetIds, 
      outcomeIds = outcomeIds
      )
  )
}

# pulls all data for a target and outcome
getDechalRechalInputsData <- function(
  targetId,
  outcomeId,
  connectionHandler,
  resultDatabaseSettings
){
  
  if(is.null(targetId)){
    return(NULL)
  }
  
  shiny::withProgress(message = 'Extracting DECHALLENGE_RECHALLENGE data', value = 0, {
  
  sql <- "SELECT distinct d.CDM_SOURCE_ABBREVIATION as database_name, dr.*
          FROM @schema.@c_table_prefixDECHALLENGE_RECHALLENGE dr 
          inner join @schema.@database_table d
          on dr.database_id = d.database_id
          where dr.TARGET_COHORT_DEFINITION_ID = @target_id
          and dr.OUTCOME_COHORT_DEFINITION_ID = @outcome_id;"

  
  shiny::incProgress(1/3, detail = paste("Fetching data"))
  
  data <- connectionHandler$queryDb(
    sql = sql, 
    schema = resultDatabaseSettings$schema,
    c_table_prefix = resultDatabaseSettings$cTablePrefix,
    target_id = targetId,
    outcome_id = outcomeId,
    database_table = resultDatabaseSettings$databaseTable
  )
  
  shiny::incProgress(3/3, detail = paste("Finished"))
  
  })
  
  return(data)
}


getDechalRechalFailData <- function(
  targetId,
  outcomeId,
  databaseId,
  dechallengeStopInterval,
  dechallengeEvaluationWindow,
  connectionHandler,
  resultDatabaseSettings
){

  if(is.null(targetId)){
    return(NULL)
  }

  shiny::withProgress(message = 'Extracting FAILLED DECHALLENGE_RECHALLENGE data', value = 0, {
    
    sql <- "SELECT * FROM @schema.@c_table_prefixRECHALLENGE_FAIL_CASE_SERIES 
          where TARGET_COHORT_DEFINITION_ID = @target_id
          and OUTCOME_COHORT_DEFINITION_ID = @outcome_id
          and DATABASE_ID = '@database_id'
          and DECHALLENGE_STOP_INTERVAL = @dechallenge_stop_interval	
          and DECHALLENGE_EVALUATION_WINDOW = @dechallenge_evaluation_window;"

    shiny::incProgress(1/3, detail = paste("Fetching data"))
    
    sql2 <- SqlRender::render(sql, 
                              schema = resultDatabaseSettings$schema,
                      c_table_prefix = resultDatabaseSettings$cTablePrefix,
                      target_id = targetId,
                      outcome_id = outcomeId,
                      database_id = databaseId,
                      dechallenge_stop_interval = dechallengeStopInterval,
                      dechallenge_evaluation_window = dechallengeEvaluationWindow
                      )
    
    data <- connectionHandler$queryDb(
      sql = sql, 
      schema = resultDatabaseSettings$schema,
      c_table_prefix = resultDatabaseSettings$cTablePrefix,
      target_id = targetId,
      outcome_id = outcomeId,
      database_id = databaseId,
      dechallenge_stop_interval = dechallengeStopInterval,
      dechallenge_evaluation_window = dechallengeEvaluationWindow
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
          "targetCohortDefinitionId", 
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
          "targetCohortDefinitionId", 
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
          "targetCohortDefinitionId", 
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
          "targetCohortDefinitionId", 
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
