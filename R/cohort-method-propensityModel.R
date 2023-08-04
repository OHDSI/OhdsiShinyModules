# @file cohort-method-propensityModel
#
# Copyright 2022 Observational Health Data Sciences and Informatics
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


#' The module viewer for rendering the PLE propensity score model covariates/coefficients
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the cohort method propensity score model covariates/coefficients
#' 
#' @export
cohortMethodPropensityModelViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::div(shiny::strong("Table 3."),"Fitted propensity model, listing all coviates with non-zero coefficients. Positive coefficients indicate predictive of the target exposure."),
    DT::dataTableOutput(outputId = ns("propensityModelTable"))
  )
}


#' The module server for rendering the propensity score model
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param connectionHandler the connection to the PLE results database
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#'
#' @return
#' the PLE propensity score model
#' 
#' @export
cohortMethodPropensityModelServer <- function(
    id, 
    selectedRow, 
    connectionHandler, 
    resultDatabaseSettings
    ) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$propensityModelTable <- DT::renderDataTable({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          model <- getCohortMethodPropensityModel(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetId = row$targetId,
            comparatorId = row$comparatorId,
            databaseId = row$databaseId,
            analysisId = row$analysisId
          )
          
          table <- prepareCohortMethodPropensityModelTable(model)
          options = list(columnDefs = list(list(className = 'dt-right',  targets = 0)),
                         pageLength = 15,
                         searching = FALSE,
                         lengthChange = TRUE,
                         ordering = TRUE,
                         paging = TRUE)
          selection = list(mode = "single", target = "row")
          table <- DT::datatable(table,
                                 options = options,
                                 selection = selection,
                                 rownames = FALSE,
                                 escape = FALSE,
                                 class = "stripe nowrap compact")
          return(table)
        }
      })
      
    }
  )
}


getCohortMethodPropensityModel <- function(
    connectionHandler, 
    resultDatabaseSettings,
    targetId, 
    comparatorId, 
    analysisId, 
    databaseId
) {
  sqlTmp <- "
  SELECT
    cmpm.coefficient,
    cmc.covariate_id,
    cmc.covariate_name
  FROM
    @schema.@cm_table_prefixcovariate cmc
    JOIN @schema.@cm_table_prefixpropensity_model cmpm 
    ON cmc.covariate_id = cmpm.covariate_id 
    AND cmc.database_id = cmpm.database_id
  WHERE
    cmpm.target_id = @target_id
    AND cmpm.comparator_id = @comparator_id
    AND cmpm.analysis_id = @analysis_id
    AND cmpm.database_id = '@database_id'
  "
  
  sql <- "
    SELECT
    cmc.covariate_id,
    cmc.covariate_name,
    cmpm.coefficient
  FROM
    (
      SELECT
        covariate_id,
        covariate_name
      FROM
        @schema.@cm_table_prefixcovariate
      WHERE
        analysis_id = @analysis_id
        AND database_id = '@database_id'
      UNION
      SELECT
      0 as covariate_id,
      'intercept' as covariate_name) cmc
    JOIN @schema.@cm_table_prefixpropensity_model cmpm 
    ON cmc.covariate_id = cmpm.covariate_id
  WHERE
    cmpm.target_id = @target_id
    AND cmpm.comparator_id = @comparator_id
    AND cmpm.analysis_id = @analysis_id
    AND cmpm.database_id = '@database_id'
  "
  
  model <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
    target_id = targetId,
    comparator_id = comparatorId,
    analysis_id = analysisId,
    database_id = databaseId
  )
  return(model)
}

prepareCohortMethodPropensityModelTable <- function(model) {
  rnd <- function(x) {
    ifelse(x > 10, sprintf("%.1f", x), sprintf("%.2f", x))
  }
  table <- model[order(-abs(model$coefficient)), c("coefficient", "covariateName")]
  table$coefficient <- sprintf("%.2f", table$coefficient)
  colnames(table) <- c("Beta", "Covariate")
  return(table)
}