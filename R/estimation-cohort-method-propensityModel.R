# @file cohort-method-propensityModel
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


#' The module viewer for rendering the PLE propensity score model covariates/coefficients
#'
#' @param id the unique reference id for the module
#' @family Estimation
#' @return
#' The user interface to the cohort method propensity score model covariates/coefficients
#' 
#' @export
cohortMethodPropensityModelViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::div(shiny::strong("Table 3."),"Fitted propensity model, listing all coviates with non-zero coefficients. Positive coefficients indicate predictive of the target exposure."),
    
    resultTableViewer(id = ns("propensityModelTable"))
  )
}


#' The module server for rendering the propensity score model
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param connectionHandler the connection to the PLE results database
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#' @family Estimation
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
      
      data <- shiny::reactive({
        getCohortMethodPropensityModel(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          targetId = selectedRow()$targetId,
          comparatorId = selectedRow()$comparatorId,
          databaseId = selectedRow()$databaseId,
          analysisId = selectedRow()$analysisId
        ) %>%
          dplyr::mutate(absBeta = abs(.data$coefficient))
      })
      
      # ColorBrewer-inspired 3-color scale
      Yellows <- function(x) grDevices::rgb(grDevices::colorRamp(c("#FFFFDD", "#FFFFB9", "#FFFF79"))
                                           (x), maxColorValue = 255)
      
      # ColorBrewer-inspired 3-color scale
      Blues <- function(x) grDevices::rgb(grDevices::colorRamp(c("aliceblue", "lightblue1", "skyblue2"))
                                           (x), maxColorValue = 255)
      
      # ColorBrewer-inspired 3-color scale
      Greens <- function(x) grDevices::rgb(grDevices::colorRamp(c("#E8FDCF", "yellowgreen"))
                                          (x), maxColorValue = 255)
      
      resultTableServer(
        id = 'propensityModelTable',
        df = data, 
        colDefsInput = list(
          covariateId = reactable::colDef(
            show = F
            ),
          coefficient = reactable::colDef(
            name = 'Beta',
            cell = function(value) { 
              if (value >= 0) paste0("+", round(value, 3)) else round(value, 3)
              },
            style = function(value) {
              color <- if (value > 0) {
                "#B0D5FE"
              } else if (value < 0) {
                "#FEBABA"
              }
              list(background = color)
            },
            format = reactable::colFormat(
              digits = 3
            )
          ), 
          absBeta = reactable::colDef(
            name = 'Beta (Absolute Value)', 
            format = reactable::colFormat(
              digits = 3
            ),
            style = function(value) {
              if (!is.numeric(value)) return()
              normalized <- (value - min(data()$absBeta)) / (max(data()$absBeta) - min(data()$absBeta))
              color <- Greens(normalized)
              list(background = color)
            }
          ), 
          covariateName = reactable::colDef(
            name = 'Covariate'
          )
        )
      )
  
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
  
  if(is.null(targetId)){
    return(NULL)
  }
  
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
  
  model <- model %>%
    dplyr::arrange(dplyr::desc(abs(.data$coefficient)))
  
  return(model)
}
