# @file cohort-method-resultSummary
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


estimationCmResultsViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tabsetPanel(
    type = 'hidden',
    id = ns('resultPanel'),
    
    shiny::tabPanel(
      title = "Table",
      resultTableViewer(ns("resultSummaryTable"))
    ),
    
    shiny::tabPanel(
      title = "Results",
      shiny::actionButton(
        inputId = ns('goBackCmResults'), 
        label = "Back To Result Summary",
        shiny::icon("arrow-left"), 
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      estimationCmFullResultViewer(ns("cmFullResults"))
    )
    
  )
  
 
}


estimationCmResultsServer <- function(
    id,
    connectionHandler,
    resultDatabaseSettings,
    targetIds,
    outcomeId
) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      shiny::observeEvent(
        eventExpr = input$goBackCmResults,
        {
          shiny::updateTabsetPanel(session, "resultPanel", selected = "Table") 
        })
      
      # extract results from CM tables
      cmData <- shiny::reactive({
        res <- OhdsiReportGenerator::getCMEstimation(
          connectionHandler = connectionHandler, 
          schema = resultDatabaseSettings$schema, 
          cmTablePrefix = resultDatabaseSettings$cmTablePrefix, 
          cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
          databaseTable = resultDatabaseSettings$databaseTable, 
          targetIds = targetIds(), 
          outcomeIds = outcomeId()
            ) 
        if(nrow(res) > 0){
         res <- res %>%
          dplyr::mutate(
            outcomes = .data$targetOutcomes  + .data$comparatorOutcomes
          )
        } 
        return(res)
      })
        
      # extract results from ES tables if tables exist
      esData <- shiny::reactive({
        tryCatch(
          {
            OhdsiReportGenerator::getCmMetaEstimation(
              connectionHandler = connectionHandler, 
              schema = resultDatabaseSettings$schema, 
              cmTablePrefix = resultDatabaseSettings$cmTablePrefix, 
              cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
              targetIds = targetIds(), 
              outcomeIds = outcomeId()
            )
          }, error = function(e){print(e);print('CM ES error');return(NULL)}
        )
      })
        
      data <- shiny::reactive({
        dplyr::bind_rows(cmData(), esData())
      })
      
      resultTableOutputs <- resultTableServer(
        id = "resultSummaryTable",
        df = data,
        colDefsInput = estimationGetCmResultSummaryTableColDef(), 
        addActions = c('results'), # TODO wont work for esData
        elementId = session$ns('resultSummaryTable')
      )
      
      selectedRow <- shiny::reactiveVal(value = NULL)
      shiny::observeEvent(resultTableOutputs$actionCount(), {
        if(resultTableOutputs$actionType() == 'results'){ # add an and here to only work for cmData
          selectedRow(data()[resultTableOutputs$actionIndex()$index,])
          shiny::updateTabsetPanel(session, "resultPanel", selected = "Results")
        }
      })
      
      estimationCmFullResultServer(
        id = "cmFullResults", 
        connectionHandler = connectionHandler,
        resultDatabaseSettings = resultDatabaseSettings,
        selectedRow = selectedRow,
        actionCount = resultTableOutputs$actionCount
      )
      
      
      return(data)

    }
  )
}


estimationGetCmResultSummaryTableColDef <- function(){
  result <- list(
    
    analysisId = reactable::colDef(show = FALSE),
    description = reactable::colDef(
      name = "Analysis",
      header = withTooltip(
        "Analysis",
        "The analysis description"
      ), 
      minWidth = 140
    ),
    databaseId = reactable::colDef(show = FALSE),
    
    databaseName = reactable::colDef(
      name = "Database",
      header = withTooltip(
        "Database",
        "The database name"
      )
    ),
    
    targetId = reactable::colDef(
      name = "Target ID",
      header = withTooltip(
      "Target ID",
      "The ID of the target cohort of interest"
      )
    ),
    
    targetName = reactable::colDef(
      name = "Target",
      header = withTooltip(
        "Target",
        "The target cohort of interest"
      ),
      minWidth = 300
    ),
    
    comparatorId = reactable::colDef(
      name = "Comparator ID",
      header = withTooltip(
      "Comparator ID",
      "The ID of the comparator cohort of interest"
      )
    ),
    
    comparatorName = reactable::colDef(
      name= "Comparator",
      header = withTooltip(
        "Comparator",
        "The comparator cohort of interest"
      ),
      minWidth = 300
    ),
    
    outcomeId = reactable::colDef(
      name = "Outcome ID",
      header = withTooltip(
      "Outcome ID",
      "The ID of the outcome of interest"
      )
    ),
    
    outcomeName = reactable::colDef(
      name = "Outcome",
      header = withTooltip(
        "Outcome",
        "The outcome of interest"
      ),
      minWidth = 300
    ),
    
    rr = reactable::colDef(
      name = "RR",
      header = withTooltip(
        "RR",
        "The estimated relative risk (e.g. the hazard ratio)"
      ), 
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    
    ci95Lb = reactable::colDef(
      name = "Lower 95% CI",
      header = withTooltip(
        "Lower 95% CI",
        "The lower bound of the 95% confidence internval of the uncalibrated relative risk"
      ), 
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    
    ci95Ub = reactable::colDef(
      name = "Upper 95% CI",
      header = withTooltip(
        "Upper 95% CI",
        "The upper bound of the 95% confidence internval of the uncalibrated relative risk"
      ), 
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    
    p = reactable::colDef(
      name = "p-val",
      header = withTooltip(
        "p-val",
        "The two-sided p-value of the uncalibrated relative risk"
      ), 
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    
    calibratedRr = reactable::colDef(
      name = "Calibrated RR",
      header = withTooltip(
        "Calibrated RR",
        "The calibrated relative risk"
      ), 
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    
    calibratedCi95Lb = reactable::colDef(
      name = "Calibrated Lower 95% CI",
      header = withTooltip(
        "Calibrated Lower 95% CI",
        "The lower bound of the 95% confidence internval of the calibrated relative risk"
      ), 
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    
    calibratedCi95Ub = reactable::colDef(
      name = "Calibrated Upper 95% CI",
      header = withTooltip(
        "Calibrated Upper 95% CI",
        "The upper bound of the 95% confidence internval of the calibrated relative risk"
      ), 
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    
    calibratedP = reactable::colDef(
      name = "Calibrated p-val",
      header = withTooltip(
        "Calibrated p-val",
        "The two-sided p-value of the calibrated relative risk"
      ), 
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    
    calibratedOneSidedP = reactable::colDef(
      name = "Calibrated one-sided p-val",
      header = withTooltip(
        "Calibrated one-sided p-val",
        "The one-sided p-value of the calibrated relative risk"
      ), 
      format = reactable::colFormat(digits = 4),
      na = "-"
    ),
    nDatabases = reactable::colDef(
      name = "No. Databases",
      header = withTooltip(
        "No. Databases",
        "The number of databases used in the meta-analysis"
      ), 
      format = reactable::colFormat(digits = 0),
      na = "-"
    ),
    
    targetEstimator = reactable::colDef(show = FALSE),
    logRr = reactable::colDef(show = FALSE),
    seLogRr = reactable::colDef(show = FALSE),
    targetSubjects = reactable::colDef(
      name = 'Target Count',
      show = TRUE
      ),
    comparatorSubjects  = reactable::colDef(
      name = 'Comparator Count',
      show = TRUE
    ),
    targetDays = reactable::colDef(show = FALSE),
    comparatorDays  = reactable::colDef(show = FALSE),
    targetOutcomes = reactable::colDef(
      show = FALSE
    ),
    comparatorOutcomes  = reactable::colDef(
      show = FALSE
    ),
    outcomes = reactable::colDef(
      name = 'Outcome Count',
      show = TRUE
    ),
    calibratedLogRr = reactable::colDef(show = FALSE),
    calibratedSeLogRr = reactable::colDef(show = FALSE),
    calibratedSeLogRr = reactable::colDef(show = FALSE),
    unblind = reactable::colDef(show = FALSE)
  )
  
  return(result)
}

# IS THIS FUNCTION STILL NEEDED?
# Function to format results
# used by both cm and sccs
computeTraditionalP <- function(
    logRr, 
    seLogRr, 
    twoSided = TRUE, 
    upper = TRUE
) 
{
  z <- logRr/seLogRr
  
  pUpperBound <- 1 - stats::pnorm(z)
  pLowerBound <- stats::pnorm(z)
  
  if (twoSided) {
    return(2 * pmin(pUpperBound, pLowerBound))
  }
  else if (upper) {
    return(pUpperBound)
  }
  else {
    return(pLowerBound)
  }
}
