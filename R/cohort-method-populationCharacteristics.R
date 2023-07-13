# @file cohort-method-populationCharacteristics
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


#' The module viewer for rendering the PLE population characteristics
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the cohort method population characteristics objects
#' 
#' @export
cohortMethodPopulationCharacteristicsViewer <- function(id) {
  
  ns <- shiny::NS(id)
  shiny::div(
    shiny::uiOutput(outputId = ns("table1Caption")),
    DT::dataTableOutput(outputId = ns("table1Table"))
  )
}


#' The module server for rendering the population characteristics
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param inputParams  the selected study parameters of interest
#' @param connectionHandler the connection to the PLE results database
#' @param resultsSchema the schema with the PLE results
#' @param tablePrefix tablePrefix
#'
#' @return
#' the PLE population characteristics content server
#' 
#' @export
cohortMethodPopulationCharacteristicsServer <- function(
    id, 
    selectedRow, 
    inputParams, 
    connectionHandler, 
    resultsSchema, 
    tablePrefix
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
          return(shiny::HTML(sprintf(text, inputParams()$target, inputParams()$comparator)))
        }
      })
      
      output$table1Table <- DT::renderDataTable({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          balance <- getCohortMethodPopChar(
            connectionHandler = connectionHandler,
            resultsSchema = resultsSchema,
            tablePrefix = tablePrefix,
            targetId = inputParams()$target,
            comparatorId = inputParams()$comparator,
            outcomeId = inputParams()$outcome,
            databaseId = row$databaseId,
            analysisId = row$analysisId
          )
          if (nrow(balance) == 0) {
            return(NULL)
          }
          table1 <- prepareCohortMethodTable1(
            balance = balance,
            beforeLabel = paste("Before PS adjustment"),
            afterLabel = paste("After PS adjustment")
          )
          
          container <- htmltools::tags$table(
            class = 'display',
            htmltools::tags$thead(
              htmltools::tags$tr(
                htmltools::tags$th(rowspan = 3, "Characteristic"),
                htmltools::tags$th(colspan = 3, class = "dt-center", paste("Before PS adjustment")),
                htmltools::tags$th(colspan = 3, class = "dt-center", paste("After PS adjustment"))
              ),
              htmltools::tags$tr(
                lapply(table1[1, 2:ncol(table1)], htmltools::tags$th)
              ),
              htmltools::tags$tr(
                lapply(table1[2, 2:ncol(table1)], htmltools::tags$th)
              )
            )
          )
          options <- list(columnDefs = list(list(className = 'dt-right',  targets = 1:6)),
                          searching = FALSE,
                          ordering = FALSE,
                          paging = FALSE,
                          bInfo = FALSE)
          table1 <- DT::datatable(table1[3:nrow(table1), ],
                                  options = options,
                                  rownames = FALSE,
                                  escape = FALSE,
                                  container = container,
                                  class = "stripe nowrap compact")
          return(table1)
        }
      })
    }
  )
}


getCohortMethodPopChar <- function(
    connectionHandler,
    resultsSchema,
    tablePrefix,
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
        cmcb.target_mean_before before_matching_mean_treated,
        cmcb.comparator_mean_before before_matching_mean_comparator,
        abs(cmcb.std_diff_before) abs_before_matching_std_diff,
        cmcb.target_mean_after after_matching_mean_treated,
        cmcb.comparator_mean_after after_matching_mean_comparator,
        abs(cmcb.std_diff_after) abs_after_matching_std_diff
      FROM
        (select * from  @results_schema.@table_prefixcovariate_balance 
        WHERE target_id = @target_id
        AND comparator_id = @comparator_id
        AND outcome_id = @outcome_id
        AND analysis_id = @analysis_id
        AND database_id = '@database_id'
        ) as cmcb
        INNER JOIN @results_schema.@table_prefixcovariate cmc 
        
        ON cmcb.covariate_id = cmc.covariate_id 
        AND cmcb.analysis_id = cmc.analysis_id 
        AND cmcb.database_id = cmc.database_id -- database_id optional
          
    "
    
    shiny::incProgress(1/3, detail = paste("Extracting"))
    result <- connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix,
      target_id = targetId,
      comparator_id = comparatorId,
      outcome_id = outcomeId,
      analysis_id = analysisId,
      database_id = databaseId
    )
    
    shiny::incProgress(3/3, detail = paste("Done - nrows: ", nrow(result)))
  })
  
  return(
    result
  )
  
}


# CohortMethod-populationChar
prepareCohortMethodTable1 <- function(
    balance,
    beforeLabel = "Before stratification",
    afterLabel = "After stratification",
    targetLabel = "Target",
    comparatorLabel = "Comparator",
    percentDigits = 1,
    stdDiffDigits = 2,
    output = "latex",
    pathToCsv = NULL
) {
  
  
  if(is.null(pathToCsv)) {
    pathToCsv <- system.file("cohort-method-ref", "Table1Specs.csv", package = "OhdsiShinyModules")
  }
  if (output == "latex") {
    space <- " "
  } else {
    space <- "&nbsp;"
  }
  
  specifications <- utils::read.csv(pathToCsv, stringsAsFactors = FALSE)
  
  fixCase <- function(label) {
    idx <- (toupper(label) == label)
    if (any(idx)) {
      label[idx] <- paste0(substr(label[idx], 1, 1),
                           tolower(substr(label[idx], 2, nchar(label[idx]))))
    }
    return(label)
  }
  
  formatPercent <- function(x) {
    result <- format(round(100 * x, percentDigits), digits = percentDigits + 1, justify = "right")
    result <- gsub("^-", "<", result)
    result <- gsub("NA", "", result)
    result <- gsub(" ", space, result)
    return(result)
  }
  
  formatStdDiff <- function(x) {
    result <- format(round(x, stdDiffDigits), digits = stdDiffDigits + 1, justify = "right")
    result <- gsub("NA", "", result)
    result <- gsub(" ", space, result)
    return(result)
  }
  
  resultsTable <- data.frame()
  for (i in 1:nrow(specifications)) {
    if (specifications$analysisId[i] == "") {
      resultsTable <- rbind(resultsTable,
                            data.frame(Characteristic = specifications$label[i], value = ""))
    } else {
      idx <- balance$analysisId == specifications$analysisId[i]
      if (any(idx)) {
        if (specifications$covariateIds[i] != "") {
          covariateIds <- as.numeric(strsplit(specifications$covariateIds[i], ";")[[1]])
          idx <- balance$covariateId %in% covariateIds
        } else {
          covariateIds <- NULL
        }
        if (any(idx)) {
          balanceSubset <- balance[idx, ]
          if (is.null(covariateIds)) {
            balanceSubset <- balanceSubset[order(balanceSubset$covariateId), ]
          } else {
            balanceSubset <- merge(balanceSubset, data.frame(covariateId = covariateIds,
                                                             rn = 1:length(covariateIds)))
            balanceSubset <- balanceSubset[order(balanceSubset$rn, balanceSubset$covariateId), ]
          }
          balanceSubset$covariateName <- fixCase(gsub("^.*: ", "", balanceSubset$covariateName))
          if (specifications$covariateIds[i] == "" || length(covariateIds) > 1) {
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = specifications$label[i],
                                                           beforeMatchingMeanTreated = NA,
                                                           beforeMatchingMeanComparator = NA,
                                                           absBeforeMatchingStdDiff = NA,
                                                           afterMatchingMeanTreated = NA,
                                                           afterMatchingMeanComparator = NA,
                                                           absAfterMatchingStdDiff = NA,
                                                           stringsAsFactors = FALSE))
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = paste0(space,
                                                                                   space,
                                                                                   space,
                                                                                   space,
                                                                                   balanceSubset$covariateName),
                                                           beforeMatchingMeanTreated = balanceSubset$beforeMatchingMeanTreated,
                                                           beforeMatchingMeanComparator = balanceSubset$beforeMatchingMeanComparator,
                                                           absBeforeMatchingStdDiff = balanceSubset$absBeforeMatchingStdDiff,
                                                           afterMatchingMeanTreated = balanceSubset$afterMatchingMeanTreated,
                                                           afterMatchingMeanComparator = balanceSubset$afterMatchingMeanComparator,
                                                           absAfterMatchingStdDiff = balanceSubset$absAfterMatchingStdDiff,
                                                           stringsAsFactors = FALSE))
          } else {
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = specifications$label[i],
                                                           beforeMatchingMeanTreated = balanceSubset$beforeMatchingMeanTreated,
                                                           beforeMatchingMeanComparator = balanceSubset$beforeMatchingMeanComparator,
                                                           absBeforeMatchingStdDiff = balanceSubset$absBeforeMatchingStdDiff,
                                                           afterMatchingMeanTreated = balanceSubset$afterMatchingMeanTreated,
                                                           afterMatchingMeanComparator = balanceSubset$afterMatchingMeanComparator,
                                                           absAfterMatchingStdDiff = balanceSubset$absAfterMatchingStdDiff,
                                                           stringsAsFactors = FALSE))
          }
        }
      }
    }
  }
  resultsTable$beforeMatchingMeanTreated <- formatPercent(resultsTable$beforeMatchingMeanTreated)
  resultsTable$beforeMatchingMeanComparator <- formatPercent(resultsTable$beforeMatchingMeanComparator)
  resultsTable$absBeforeMatchingStdDiff <- formatStdDiff(resultsTable$absBeforeMatchingStdDiff)
  resultsTable$afterMatchingMeanTreated <- formatPercent(resultsTable$afterMatchingMeanTreated)
  resultsTable$afterMatchingMeanComparator <- formatPercent(resultsTable$afterMatchingMeanComparator)
  resultsTable$absAfterMatchingStdDiff <- formatStdDiff(resultsTable$absAfterMatchingStdDiff)
  
  headerRow <- as.data.frame(t(rep("", ncol(resultsTable))))
  colnames(headerRow) <- colnames(resultsTable)
  headerRow$beforeMatchingMeanTreated <- targetLabel
  headerRow$beforeMatchingMeanComparator <- comparatorLabel
  headerRow$afterMatchingMeanTreated <- targetLabel
  headerRow$afterMatchingMeanComparator <- comparatorLabel
  
  subHeaderRow <- as.data.frame(t(rep("", ncol(resultsTable))))
  colnames(subHeaderRow) <- colnames(resultsTable)
  subHeaderRow$Characteristic <- "Characteristic"
  subHeaderRow$beforeMatchingMeanTreated <- "%"
  subHeaderRow$beforeMatchingMeanComparator <- "%"
  subHeaderRow$absBeforeMatchingStdDiff <- "Std. diff"
  subHeaderRow$afterMatchingMeanTreated <- "%"
  subHeaderRow$afterMatchingMeanComparator <- "%"
  subHeaderRow$absAfterMatchingStdDiff <- "Std. diff"
  
  resultsTable <- rbind(headerRow, subHeaderRow, resultsTable)
  
  colnames(resultsTable) <- rep("", ncol(resultsTable))
  colnames(resultsTable)[2] <- beforeLabel
  colnames(resultsTable)[5] <- afterLabel
  return(resultsTable)
}
