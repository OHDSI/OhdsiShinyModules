# @file estimation-covariateBalance
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


#' The module viewer for rendering the PLE covariate balance analysis
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the estimation covariate balance results
#' 
#' @export
estimationCovariateBalanceViewer <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::conditionalPanel(condition = "output.isMetaAnalysis == false",
                     ns = ns,
                     shiny::uiOutput(outputId = ns("hoverInfoBalanceScatter")),
                     
                     plotly::plotlyOutput(ns("balancePlot")),
                     shiny::uiOutput(outputId = ns("balancePlotCaption")),
                     
                     
                     shiny::textInput(ns("covariateHighlight"), "Highlight covariates containing:", ),
                     shiny::actionButton(ns("covariateHighlightButton"), "Highlight"),
                     
                     
                     reactable::reactableOutput(ns("balanceTable"))
                    
    ),
    shiny::conditionalPanel(condition = "output.isMetaAnalysis == true",
                     ns = ns,
                     shiny::plotOutput(outputId = ns("balanceSummaryPlot")),
                     shiny::uiOutput(outputId = ns("balanceSummaryPlotCaption")),
                     shiny::div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
                                shiny::downloadButton(outputId = ns("downloadBalanceSummaryPlotPng"),
                                        label = "Download plot as PNG"),
                                shiny::downloadButton(outputId = ns("downloadBalanceSummaryPlotPdf"),
                                        label = "Download plot as PDF")
                     ))
  )
}


#' The module server for rendering the covariate balance plot
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param inputParams  the selected study parameters of interest
#' @param connectionHandler the connection to the PLE results database
#' @param resultsSchema the schema with the PLE results
#' @param tablePrefix tablePrefix
#' @param metaAnalysisDbIds metaAnalysisDbIds
#'
#' @return
#' the PLE covariate balance content server
#' 
#' @export
estimationCovariateBalanceServer <- function(id, selectedRow, inputParams, connectionHandler, resultsSchema, tablePrefix, metaAnalysisDbIds = NULL) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      balance <- shiny::reactive({
        row <- selectedRow()
        balance <- tryCatch({
          getEstimationCovariateBalanceShared(
          connectionHandler = connectionHandler,
          resultsSchema = resultsSchema,
          tablePrefix = tablePrefix,
          targetId = inputParams()$target,
          comparatorId = inputParams()$comparator,
          databaseId = row$databaseId,
          analysisId = row$analysisId)},
          error = function(e){return(NULL)}
        )
        return(balance)
      })
      
      output$isMetaAnalysis <- shiny::reactive({
        return(FALSE)
        ##TODO: update once MA implemented
        row <- selectedRow()
        isMetaAnalysis <- !is.null(row) && (row$databaseId %in% metaAnalysisDbIds)
        return(isMetaAnalysis)
      })
      
      shiny::outputOptions(output, "isMetaAnalysis", suspendWhenHidden = FALSE)
      
      textSearchEstimation <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(
        input$covariateHighlightButton,{
          
          textSearchEstimation(input$covariateHighlight)
          
        }
        )
      
      balancePlot <- shiny::reactive({
        if (is.null(balance()) || nrow(balance()) == 0) {
          return(NULL)
        } else {
          plot <- plotEstimationCovariateBalanceScatterPlotNew(
            balance = balance(),
            beforeLabel = "Before propensity score adjustment",
            afterLabel = "After propensity score adjustment",
            textsearch = textSearchEstimation
          )
          return(plot)
        }
      })
      
      output$balancePlot <- plotly::renderPlotly({
        return(balancePlot())
      })
      
      output$balancePlotCaption <- shiny::renderUI({
        if (is.null(balance()) || nrow(balance()) == 0) {
          return(NULL)
        } else {
          row <- selectedRow()
          text <- "<strong>Figure 3.</strong> Covariate balance before and after propensity score adjustment. Each dot represents
      the standardizes difference of means for a single covariate before and after propensity score adjustment on the propensity
      score. Move the mouse arrow over a dot for more details."
          return(shiny::HTML(sprintf(text)))
        }
      })
      
      if(F){ # makes app slow
      output$balanceTable <- reactable::renderReactable({
        reactable::reactable(
          data = balance() %>% 
            dplyr::select("covariateName", "absBeforeMatchingStdDiff", "absAfterMatchingStdDiff"),
          columns = list(
            covariateName = reactable::colDef(
              name = "Covariate"
              ),
            absBeforeMatchingStdDiff = reactable::colDef(
              name = "Before Matching Abs Std Deff",
              filterable = FALSE,
              format = reactable::colFormat(digits = 4)
              ),
            absAfterMatchingStdDiff = reactable::colDef(
              name = "After Matching Abs Std Deff",
              filterable = FALSE,
              format = reactable::colFormat(digits = 4)
              )
          ),
          filterable = TRUE
          )
      })
      }
      
      output$hoverInfoBalanceScatter <- shiny::renderUI({
        if (is.null(balance()) || nrow(balance()) == 0) {
          return(NULL)
        } else {
          row <- selectedRow()
          hover <- input$plotHoverBalanceScatter
          point <- shiny::nearPoints(balance(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
          if (nrow(point) == 0) {
            return(NULL)
          }
          left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
          top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
          left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
          top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
          style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                          "left:",
                          left_px - 251,
                          "px; top:",
                          top_px - 150,
                          "px; width:500px;")
          beforeMatchingStdDiff <- formatC(point$beforeMatchingStdDiff, digits = 2, format = "f")
          afterMatchingStdDiff <- formatC(point$afterMatchingStdDiff, digits = 2, format = "f")
          shiny::div(
            style = "position: relative; width: 0; height: 0",
            shiny::wellPanel(
              style = style,
              shiny::p(shiny::HTML(paste0("<b> Covariate: </b>", point$covariateName, "<br/>",
                                          "<b> Std. diff before ",tolower(row$psStrategy),": </b>", beforeMatchingStdDiff, "<br/>",
                                          "<b> Std. diff after ",tolower(row$psStrategy),": </b>", afterMatchingStdDiff)))
            )
          )
        }
      })
      
      balanceSummaryPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row) || !(row$databaseId %in% metaAnalysisDbIds)) {
          return(NULL)
        } else {
          balanceSummary <- getEstimationCovariateBalanceSummary(connectionHandler = connectionHandler,
                                                                 resultsSchema = resultsSchema,
                                                                 tablePrefix = tablePrefix,  
                                                                 targetId = inputParams()$target,
                                                                 comparatorId = inputParams()$comparator,
                                                                 analysisId = row$analysisId,
                                                                 databaseId = row$analysisId,
                                                                 beforeLabel = paste("Before", row$psStrategy),
                                                                 afterLabel = paste("After", row$psStrategy))
          plot <- plotEstimationCovariateBalanceSummary(balanceSummary,
                                                        threshold = 0.1,
                                                        beforeLabel = paste("Before", row$psStrategy),
                                                        afterLabel = paste("After", row$psStrategy))
          return(plot)
        }
      })
      
      output$balanceSummaryPlot <- shiny::renderPlot({
        balanceSummaryPlot()
      }, res = 100)
      
      output$balanceSummaryPlotCaption <- shiny::renderUI({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          text <- "<strong>Figure 7.</strong> Covariate balance before and after %s. The y axis represents
      the standardized difference of mean before and after %s on the propensity
      score. The whiskers show the minimum and maximum values across covariates. The box represents the
      interquartile range, and the middle line represents the median. The dashed lines indicate a standardized
      difference of 0.1."
          return(shiny::HTML(sprintf(text, row$psStrategy, row$psStrategy)))
        }
      })
      
      output$downloadBalanceSummaryPlotPng <- shiny::downloadHandler(filename = "BalanceSummary.png",
                                                                     contentType = "image/png",
                                                                     content = function(file) {
                                                                       ggplot2::ggsave(file, plot = balanceSummaryPlot(), width = 12, height = 5.5, dpi = 400)
                                                                     })
      
      output$downloadBalanceSummaryPlotPdf <- shiny::downloadHandler(filename = "BalanceSummary.pdf",
                                                                     contentType = "application/pdf",
                                                                     content = function(file) {
                                                                       ggplot2::ggsave(file = file, plot = balanceSummaryPlot(), width = 12, height = 5.5)
                                                                     })
      
    }
  )
}

getEstimationCovariateBalanceShared <- function(
    connectionHandler,
    resultsSchema,
    tablePrefix,
    targetId,
    comparatorId,
    analysisId,
    databaseId = NULL
) {
  
  shiny::withProgress(message = 'Extracting covariate balance', value = 0, {
    
      shiny::incProgress(1/6, detail = paste("Writing sql"))
      sql <- "
      SELECT
        cmscb.database_id,
        cmscb.covariate_id,
        cmc.covariate_name,
        -- cmc.covariate_analysis_id analysis_id, #TODO: once @table_prefixanalysis_id bug fixed
        cmscb.target_mean_before before_matching_mean_treated,
        cmscb.comparator_mean_before before_matching_mean_comparator,
        abs(cmscb.std_diff_before) abs_before_matching_std_diff, --absBeforeMatchingStdDiff 
        cmscb.target_mean_after after_matching_mean_treated,
        cmscb.comparator_mean_after after_matching_mean_comparator,
        abs(cmscb.std_diff_after) abs_after_matching_std_diff
      FROM
        @results_schema.@table_prefixshared_covariate_balance cmscb 
        JOIN @results_schema.@table_prefixcovariate cmc ON cmscb.covariate_id = cmc.covariate_id AND cmscb.analysis_id = cmc.analysis_id AND cmscb.database_id = cmc.database_id -- database_id optional
       -- JOIN @results_schema.@table_prefixcovariate_analysis cmca ON cmca.analysis_id = cmc.analysis_id  -- question: shouldn't we have a covariate_analysis_id in @table_prefixcovariate table?
      WHERE
        cmscb.target_id = @target_id
        AND cmscb.comparator_id = @comparator_id
        AND cmscb.analysis_id = @analysis_id
        AND cmscb.database_id = '@database_id'
    "
    
    shiny::incProgress(1/3, detail = paste("Extracting"))
    result <- connectionHandler$queryDb(
      sql = sql,
      results_schema = resultsSchema,
      table_prefix = tablePrefix,
      target_id = targetId,
      comparator_id = comparatorId,
      analysis_id = analysisId,
      database_id = databaseId
    )
    
    shiny::incProgress(3/3, detail = paste("Done - nrows: ", nrow(result)))
  })
  
  return(
    result
  )
  
}


getEstimationCovariateBalanceSummary <- function(connectionHandler, 
                                                 resultsSchema,
                                                 tablePrefix,
                                                 databaseId,
                                                 targetId, 
                                                 comparatorId, analysisId,
                                                 beforeLabel = "Before matching",
                                                 afterLabel = "After matching") {
  
  balance <- getEstimationCovariateBalanceShared(connectionHandler = connectionHandler,
                                                 targetId = targetId,
                                                 comparatorId = comparatorId,
                                                 analysisId = analysisId,
                                                 resultsSchema,
                                                 tablePrefix,
                                                 databaseId = databaseId)
  balanceBefore <- balance %>%
    dplyr::group_by(.data$databaseId) %>%
    dplyr::summarise(covariateCount = dplyr::n(),
                     qs = stats::quantile(.data$absBeforeMatchingStdDiff, c(0, 0.25, 0.5, 0.75, 1)), prob = c("ymin", "lower", "median", "upper", "ymax")) %>%
    tidyr::spread(key = "prob", value = "qs")
  balanceBefore[, "type"] <- beforeLabel
  balanceAfter <-  balance %>%
    dplyr::group_by(.data$databaseId) %>%
    dplyr::summarise(covariateCount = dplyr::n(),
                     qs = stats::quantile(.data$afterMatchingStdDiff, c(0, 0.25, 0.5, 0.75, 1)), prob = c("ymin", "lower", "median", "upper", "ymax")) %>%
    tidyr::spread(key = "prob", value = "qs")
  balanceAfter[, "type"] <- afterLabel
  
  balanceSummary <- rbind(balanceBefore, balanceAfter) %>%
    dplyr::ungroup()
  
  return(balanceSummary)
  
}



plotEstimationCovariateBalanceScatterPlotNew <- function(
    balance,
    beforeLabel = "Before propensity score adjustment",
    afterLabel = "After propensity score adjustment",
    textsearch = NULL
){
  
  if(is.null(textsearch())){
    balance$highlight <- 'blue' 
    colors <- c("blue")
  } else if(textsearch() == ''){
    balance$highlight <- 'blue'
    colors <- c("blue")
  } else{
    balance$highlight <- 'blue'
    balance$highlight[grep(textsearch(), balance$covariateName)] <- 'yellow'
    colors <- c("blue", "goldenrod") 
  }
  
  limits <- c(min(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff),
                  na.rm = TRUE),
              max(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff),
                  na.rm = TRUE))
  
  xyline <- function(limits, color = "grey") {
    list(
      type = "line", 
      x0 = 0, 
      x1 = limits[2], 
      xref = "paper",
      y0 = 0, 
      y1 = limits[2], 
      line = list(color = color, dash = 'dash')
    )
  }
  
  plot <- plotly::plot_ly(
    data = balance, 
    x = ~absBeforeMatchingStdDiff, 
    y = ~absAfterMatchingStdDiff, 
    color = ~highlight, # added
    text = ~paste("Name: ", covariateName, '<br>Before: ', absBeforeMatchingStdDiff, '<br>After: ', absAfterMatchingStdDiff),
    colors = colors
  ) %>%
    plotly::layout(
      shapes = list(xyline(limits)),
      plot_bgcolor = "#e5ecf6",
      xaxis = list(title = beforeLabel, range = limits), 
      yaxis = list(title = afterLabel, range = limits)
    )
  
  return(plot)
}
