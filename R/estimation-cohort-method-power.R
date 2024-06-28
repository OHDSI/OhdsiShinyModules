# @file cohort-method-power
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


#' The module viewer for rendering the PLE power analysis
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the cohort method power calculation results
#' 
#' @export
cohortMethodPowerViewer <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::uiOutput(outputId = ns("powerTableCaption")),
    shiny::tableOutput(outputId = ns("powerTable")),
    shiny::uiOutput(outputId = ns("timeAtRiskTableCaption")),
    shiny::tableOutput(outputId = ns("timeAtRiskTable"))
  )
}


#' The module server for rendering the PLE power analysis results
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param connectionHandler the connection to the PLE results database
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#'
#' @return
#' the PLE systematic error power server
#' 
#' @export
cohortMethodPowerServer <- function(
    id, 
    selectedRow, 
    connectionHandler, 
    resultDatabaseSettings
    ) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      output$powerTableCaption <- shiny::renderUI({
        row <- selectedRow()
        if (!is.null(row$target)) {
          text <- "<strong>Table 1a.</strong> Number of subjects, follow-up time (in years), number of outcome
      events, and event incidence rate (IR) per 1,000 patient years (PY) in the target (<em>%s</em>) and
      comparator (<em>%s</em>) group after propensity score adjustment, as  well as the minimum detectable  relative risk (MDRR).
      Note that the IR does not account for any stratification."
          return(shiny::HTML(sprintf(text, row$target, row$comparator)))
        } else {
          return(NULL)
        }
      })
      
      output$powerTable <- shiny::renderTable({
        row <- selectedRow()
        if (is.null(row$target)) {
          return(NULL)
        } else {
            table <- prepareCohortMethodPowerTable(
              row, 
              connectionHandler = connectionHandler, 
              resultDatabaseSettings = resultDatabaseSettings
              )
            if (!row$unblind) {
              table$targetOutcomes  <- NA
              table$comparatorOutcomes   <- NA
              table$targetIr   <- NA
              table$comparatorIr   <- NA
            }
            colnames(table) <- c("Target subjects",
                                 "Comparator subjects",
                                 "Target years",
                                 "Comparator years",
                                 "Target events",
                                 "Comparator events",
                                 "Target IR (per 1,000 PY)",
                                 "Comparator IR (per 1,000 PY)",
                                 "MDRR")
          
          return(table)
        }
      })
      
      output$timeAtRiskTableCaption <- shiny::renderUI({
        row <- selectedRow()
        if (!is.null(row$target)) {
          text <- "<strong>Table 1b.</strong> Time (days) at risk distribution expressed as
      minimum (min), 25th percentile (P25), median, 75th percentile (P75), and maximum (max) in the target
     (<em>%s</em>) and comparator (<em>%s</em>) cohort after propensity score adjustment."
          return(shiny::HTML(sprintf(text, row$target, row$comparator)))
        } else {
          return(NULL)
        }
      })
      
      output$timeAtRiskTable <- shiny::renderTable({
        row <- selectedRow()
        if (is.null(row$target)) {
          return(NULL)
        } else {
            followUpDist <- getCmFollowUpDist(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              targetId = row$targetId,
              comparatorId = row$comparatorId,
              outcomeId = row$outcomeId,
              databaseId = row$databaseId,
              analysisId = row$analysisId
            )
          
          table <- prepareCohortMethodFollowUpDistTable(followUpDist)
          return(table)
        }
      })
    })
}


prepareCohortMethodFollowUpDistTable <- function(followUpDist) {
  targetRow <- data.frame(Database = followUpDist$databaseId,
                          Cohort = "Target",
                          Min = followUpDist$targetMinDays,
                          P10 = followUpDist$targetP10Days,
                          P25 = followUpDist$targetP25Days,
                          Median = followUpDist$targetMedianDays,
                          P75 = followUpDist$targetP75Days,
                          P90 = followUpDist$targetP90Days,
                          Max = followUpDist$targetMaxDays)
  comparatorRow <- data.frame(Database = followUpDist$databaseId,
                              Cohort = "Comparator",
                              Min = followUpDist$comparatorMinDays,
                              P10 = followUpDist$comparatorP10Days,
                              P25 = followUpDist$comparatorP25Days,
                              Median = followUpDist$comparatorMedianDays,
                              P75 = followUpDist$comparatorP75Days,
                              P90 = followUpDist$comparatorP90Days,
                              Max = followUpDist$comparatorMaxDays)
  table <- rbind(targetRow, comparatorRow)
  table$Min <- formatC(table$Min, big.mark = ",", format = "d")
  table$P10 <- formatC(table$P10, big.mark = ",", format = "d")
  table$P25 <- formatC(table$P25, big.mark = ",", format = "d")
  table$Median <- formatC(table$Median, big.mark = ",", format = "d")
  table$P75 <- formatC(table$P75, big.mark = ",", format = "d")
  table$P90 <- formatC(table$P90, big.mark = ",", format = "d")
  table$Max <- formatC(table$Max, big.mark = ",", format = "d")
  if (length(unique(followUpDist$databaseId)) == 1)
    table$Database <- NULL
  return(table)
}


prepareCohortMethodPowerTable <- function(
    mainResults, 
    connectionHandler , 
    resultDatabaseSettings
) {
  #analyses <- getCohortMethodAnalyses(
  #  connectionHandler = connectionHandler,
  #  resultDatabaseSettings = resultDatabaseSettings
  #)
  #table <- merge(mainResults, analyses)
  table <- mainResults
  alpha <- 0.05
  power <- 0.8
  z1MinAlpha <- stats::qnorm(1 - alpha/2)
  zBeta <- -stats::qnorm(1 - power)
  pA <- table$targetSubjects/(table$targetSubjects + table$comparatorSubjects)
  pB <- 1 - pA
  totalEvents <- abs(table$targetOutcomes) + abs(table$comparatorOutcomes)
  table$mdrr <- exp(sqrt((zBeta + z1MinAlpha)^2/(totalEvents * pA * pB)))
  table$targetYears <- table$targetDays/365.25
  table$comparatorYears <- table$comparatorDays/365.25
  table$targetIr <- 1000 * table$targetOutcomes/table$targetYears
  table$comparatorIr <- 1000 * table$comparatorOutcomes/table$comparatorYears
  table <- table[, c("targetSubjects",
                     "comparatorSubjects",
                     "targetYears",
                     "comparatorYears",
                     "targetOutcomes",
                     "comparatorOutcomes",
                     "targetIr",
                     "comparatorIr",
                     "mdrr"), drop = F]
  table$targetSubjects <- formatC(table$targetSubjects, big.mark = ",", format = "d")
  table$comparatorSubjects <- formatC(table$comparatorSubjects, big.mark = ",", format = "d")
  table$targetYears <- formatC(table$targetYears, big.mark = ",", format = "d")
  table$comparatorYears <- formatC(table$comparatorYears, big.mark = ",", format = "d")
  table$targetOutcomes <- formatC(table$targetOutcomes, big.mark = ",", format = "d")
  table$comparatorOutcomes <- formatC(table$comparatorOutcomes, big.mark = ",", format = "d")
  table$targetIr <- sprintf("%.2f", table$targetIr)
  table$comparatorIr <- sprintf("%.2f", table$comparatorIr)
  table$mdrr <- sprintf("%.2f", table$mdrr)
  table$targetSubjects <- gsub("^-", "<", table$targetSubjects)
  table$comparatorSubjects <- gsub("^-", "<", table$comparatorSubjects)
  table$targetOutcomes <- gsub("^-", "<", table$targetOutcomes)
  table$comparatorOutcomes <- gsub("^-", "<", table$comparatorOutcomes)
  table$targetIr <- gsub("^-", "<", table$targetIr)
  table$comparatorIr <- gsub("^-", "<", table$comparatorIr)
  idx <- (table$targetOutcomes < 0 | table$comparatorOutcomes < 0)
  table$mdrr[idx] <- paste0(">", table$mdrr[idx])
  return(table)
}


getCohortMethodAnalyses <- function(
    connectionHandler, 
    resultDatabaseSettings
) {
  sql <- "
  SELECT
    cma.*
  FROM
    @schema.@cm_table_prefixanalysis cma
  "
  return(
    connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      cm_table_prefix = resultDatabaseSettings$cmTablePrefix
    )
  )
}

getCmFollowUpDist <- function(
    connectionHandler,
    resultDatabaseSettings,
    targetId,
    comparatorId,
    outcomeId,
    databaseId = NULL,
    analysisId
) {
  
  if(is.null(targetId)){
    return(NULL)
  }
  
  sql <- "
  SELECT
    *
  FROM
    @schema.@cm_table_prefixfollow_up_dist cmfud
  WHERE
    cmfud.target_id = @target_id
    AND cmfud.comparator_id = @comparator_id
    AND cmfud.outcome_id = @outcome_id
    AND cmfud.analysis_id = @analysis_id
  "
  if(!is.null(databaseId)) {
    sql <- paste(sql, paste("AND cmfud.database_id = '@database_id'"), collapse = "\n")
  }
  return(
    connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
      target_id = targetId,
      comparator_id = comparatorId,
      outcome_id = outcomeId,
      analysis_id = analysisId,
      database_id = databaseId
    )
  )
}
