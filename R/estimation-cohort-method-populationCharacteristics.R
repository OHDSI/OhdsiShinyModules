# @file cohort-method-populationCharacteristics
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


#' The module viewer for rendering the PLE population characteristics
#'
#' @param id the unique reference id for the module
#' @family Estimation
#' @return
#' The user interface to the cohort method population characteristics objects
#' 
#' @export
cohortMethodPopulationCharacteristicsViewer <- function(id) {
  
  ns <- shiny::NS(id)
  shiny::div(
    shiny::uiOutput(outputId = ns("table1Caption")),
    resultTableViewer(id = ns("table1Table"))
  )
}


#' The module server for rendering the population characteristics
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param connectionHandler the connection to the PLE results database
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#' @family Estimation
#' @return
#' the PLE population characteristics content server
#' 
#' @export
cohortMethodPopulationCharacteristicsServer <- function(
    id, 
    selectedRow, 
    connectionHandler, 
    resultDatabaseSettings
    ) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      output$table1Caption <- shiny::renderUI({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          text <- "<strong>Table 2.</strong> Select characteristics before and after propensity score adjustment, showing the (weighted)
      percentage of subjects  with the characteristics in the target (<em>%s</em>) and comparator (<em>%s</em>) group, as
      well as the standardized difference of the means."
          return(shiny::HTML(sprintf(text, row$targetName, row$comparatorName)))
        }
      })
      
      data <- shiny::reactive(
        {
          getCohortMethodPopChar(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetId = selectedRow()$targetId,
            comparatorId = selectedRow()$comparatorId,
            outcomeId = selectedRow()$outcomeId,
            databaseId = selectedRow()$databaseId,
            analysisId = selectedRow()$analysisId
          )
        }
      )
      
      resultTableServer(
        id = 'table1Table',
        elementId = session$ns('table1Table'),
        df = data, 
        groupBy = 'label',
        colDefsInput = list(
          databaseId = reactable::colDef(show = FALSE),
          databaseName = reactable::colDef(show = FALSE),
          targetName = reactable::colDef(show = FALSE),
          targetId = reactable::colDef(show = FALSE),
          outcomeName = reactable::colDef(show = FALSE),
          outcomeId = reactable::colDef(show = FALSE),
          comparatorName = reactable::colDef(show = FALSE),
          comparatorId = reactable::colDef(show = FALSE),
          stdDiffBefore = reactable::colDef(show = FALSE),
          stdDiffAfter = reactable::colDef(show = FALSE),
          meanBefore = reactable::colDef(show = FALSE),
          meanAfter = reactable::colDef(show = FALSE),
          targetStdDiff = reactable::colDef(show = FALSE),
          comparatorStdDiff = reactable::colDef(show = FALSE),
          targetComparatorStdDiff = reactable::colDef(show = FALSE),
          covariateId = reactable::colDef(show = FALSE),
          covariateName = reactable::colDef(
            name  = 'Covariate', 
            sortable = FALSE
            ), 
          analysisId = reactable::colDef(show = FALSE),
          analysisDescription = reactable::colDef(show = FALSE),
          beforePsAdjustmentMeanTreated = reactable::colDef(
            name  = 'Before PS adjustment treated mean',
            format = reactable::colFormat(
              digits = 1, percent = TRUE
            ),
            sortable = FALSE,
            cell = function(value) {
              if (value < 0) paste0("< ",abs(value*100) ,"%") else paste0(value*100, '%')
            }
            ),
          beforePsAdjustmentMeanComparator = reactable::colDef(
            name  = 'Before PS adjustment comparator mean',
            format = reactable::colFormat(
              digits = 1, percent = TRUE
            ),
            sortable = FALSE,
            cell = function(value) {
              if (value < 0) paste0("< ",abs(value*100) ,"%") else paste0(value*100, '%')
            }
          ),
          absBeforePsAdjustmentStdDiff = reactable::colDef(
            name  = 'Before PS adjustment standardized mean difference',
            format = reactable::colFormat(
              digits = 2
            ),
            sortable = FALSE
          ),
          afterPsAdjustmentMeanTreated = reactable::colDef(
            name  = 'After PS adjustment treated mean',
            format = reactable::colFormat(
              digits = 1, percent = TRUE
            ),
            sortable = FALSE,
            cell = function(value) {
              if (ifelse(is.na(value), 0, value) < 0) paste0("< ",abs(value*100) ,"%") else paste0(value*100, '%')
            }
          ),
          afterPsAdjustmentMeanComparator = reactable::colDef(
            name  = 'After PS adjustment comparator mean',
            format = reactable::colFormat(
              digits = 1, percent = TRUE
            ),
            sortable = FALSE,
            cell = function(value) {
              if (ifelse(is.na(value), 0, value) < 0) paste0("< ",abs(value*100) ,"%") else paste0(value*100, '%')
            }
          ),
          absAfterPsAdjustmentStdDiff = reactable::colDef(
            name  = 'After PS adjustment standardized mean difference',
            format = reactable::colFormat(
              digits = 2
            ),
            sortable = FALSE
          ),
          label = reactable::colDef(show = TRUE)
          
        )
      )
      
    }
  )
}


getCohortMethodPopChar <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetId,
    comparatorId,
    analysisId,
    databaseId = NULL,
    outcomeId = NULL
) {
  
  shiny::withProgress(message = 'Extracting population summary', value = 0, {
    
    shiny::incProgress(1/3, detail = paste("Extracting"))
    
    result <- OhdsiReportGenerator::getCmTable(
      connectionHandler = connectionHandler, 
      schema = resultDatabaseSettings$schema, 
      table = 'covariate_balance', 
      cmTablePrefix = resultDatabaseSettings$cmTablePrefix, 
      cgTablePrefix = resultDatabaseSettings$cgTablePrefix, 
      databaseTable = resultDatabaseSettings$databaseTable, 
      targetIds = targetId,
      comparatorIds = comparatorId,
      outcomeIds = outcomeId,
      analysisIds = analysisId,
      databaseIds = databaseId
    )
    
    shiny::incProgress(2/3, detail = paste("Processing"))
    
    result <- result %>% 
      dplyr::rename(
        beforePsAdjustmentMeanTreated = "targetMeanBefore",
        beforePsAdjustmentMeanComparator = "comparatorMeanBefore",
        afterPsAdjustmentMeanTreated = "targetMeanAfter",
        afterPsAdjustmentMeanComparator = "comparatorMeanAfter"
      ) %>%
      dplyr::mutate(
        absBeforePsAdjustmentStdDiff = abs(.data$stdDiffBefore),
        absAfterPsAdjustmentStdDiff = abs(.data$stdDiffAfter)
      )
    
    shiny::incProgress(3/3, detail = paste("Done - nrows: ", nrow(result)))
  })
  
  # format
  pathToCsv <- system.file("cohort-method-ref", "Table1Specs.csv", package = "OhdsiShinyModules")
  specifications <- utils::read.csv(pathToCsv, stringsAsFactors = FALSE)
  
  # get specs without covariates
  part1 <- merge(
    result,
    specifications[which(specifications$covariateIds == ''),-3],
    by = 'analysisId'
  ) 
  
  covVars <- specifications[which(specifications$covariateIds != ''),-2]
  covVars <- do.call(
    what = 'rbind', 
    args = lapply(1:nrow(covVars), function(i){
    data.frame(
      label = covVars$label[i],
      covariateId  = strsplit(
      x = covVars$covariateIds[i], 
      split = ';'
      )[[1]]
    )
  })
  )
  part2 <- merge(
    result,
    covVars,
    by = 'covariateId'
  ) 
  
  result <- rbind(part1, part2) %>% 
    dplyr::arrange(
      .data$label, 
      .data$covariateName
    )
  
  # remove text before covariateNames - TODO generalize this?
  txtRms <- c(
    'age group: ', 
    'condition_era group during day -365 through 0 days relative to index: ',
    'drug_era group during day -365 through 0 days relative to index: '
  )
  for(txtRm in txtRms){
    result$covariateName <- gsub(txtRm,'', result$covariateName)
  }
  
  return(
    result
  )
  
}

