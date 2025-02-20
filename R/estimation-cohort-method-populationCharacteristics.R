# @file cohort-method-populationCharacteristics
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
          return(shiny::HTML(sprintf(text, row$target, row$comparator)))
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
        df = data, 
        groupBy = 'label',
        colDefsInput = list(
          databaseId = reactable::colDef(show = F),
          covariateId = reactable::colDef(show = F),
          covariateName = reactable::colDef(
            name  = 'Covariate', 
            sortable = F
            ), 
          analysisId = reactable::colDef(show = F),
          beforePsAdjustmentMeanTreated = reactable::colDef(
            name  = 'Before PS adjustment treated mean',
            format = reactable::colFormat(
              digits = 1, percent = T
            ),
            sortable = F,
            cell = function(value) {
              if (value < 0) paste0("< ",abs(value*100) ,"%") else paste0(value*100, '%')
            }
            ),
          beforePsAdjustmentMeanComparator = reactable::colDef(
            name  = 'Before PS adjustment comparator mean',
            format = reactable::colFormat(
              digits = 1, percent = T
            ),
            sortable = F,
            cell = function(value) {
              if (value < 0) paste0("< ",abs(value*100) ,"%") else paste0(value*100, '%')
            }
          ),
          absBeforePsAdjustmentStdDiff = reactable::colDef(
            name  = 'Before PS adjustment standardized mean difference',
            format = reactable::colFormat(
              digits = 2
            ),
            sortable = F
          ),
          afterPsAdjustmentMeanTreated = reactable::colDef(
            name  = 'After PS adjustment treated mean',
            format = reactable::colFormat(
              digits = 1, percent = T
            ),
            sortable = F,
            cell = function(value) {
              if (value < 0) paste0("< ",abs(value*100) ,"%") else paste0(value*100, '%')
            }
          ),
          afterPsAdjustmentMeanComparator = reactable::colDef(
            name  = 'After PS adjustment comparator mean',
            format = reactable::colFormat(
              digits = 1, percent = T
            ),
            sortable = F,
            cell = function(value) {
              if (value < 0) paste0("< ",abs(value*100) ,"%") else paste0(value*100, '%')
            }
          ),
          absAfterPsAdjustmentStdDiff = reactable::colDef(
            name  = 'After PS adjustment standardized mean difference',
            format = reactable::colFormat(
              digits = 2
            ),
            sortable = F
          ),
          label = reactable::colDef(show = T)
          
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
    
    shiny::incProgress(1/6, detail = paste("Writing sql with outcome"))
    sql <- "
      SELECT
        cmcb.database_id,
        cmcb.covariate_id,
        cmc.covariate_name, 
        cmc.covariate_analysis_id analysis_id,
        cmcb.target_mean_before before_ps_adjustment_mean_treated,
        cmcb.comparator_mean_before before_ps_adjustment_mean_comparator,
        abs(cmcb.std_diff_before) abs_before_ps_adjustment_std_diff,
        cmcb.target_mean_after after_ps_adjustment_mean_treated,
        cmcb.comparator_mean_after after_ps_adjustment_mean_comparator,
        abs(cmcb.std_diff_after) abs_after_ps_adjustment_std_diff
      FROM
        (select * from  @schema.@cm_table_prefixcovariate_balance 
        WHERE target_id = @target_id
        AND comparator_id = @comparator_id
        AND outcome_id = @outcome_id
        AND analysis_id = @analysis_id
        AND database_id = '@database_id'
        ) as cmcb
        INNER JOIN @schema.@cm_table_prefixcovariate cmc 
        
        ON cmcb.covariate_id = cmc.covariate_id 
        AND cmcb.analysis_id = cmc.analysis_id 
        AND cmcb.database_id = cmc.database_id -- database_id optional
          
    "
    
    shiny::incProgress(1/3, detail = paste("Extracting"))
    result <- connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
      target_id = targetId,
      comparator_id = comparatorId,
      outcome_id = outcomeId,
      analysis_id = analysisId,
      database_id = databaseId
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

