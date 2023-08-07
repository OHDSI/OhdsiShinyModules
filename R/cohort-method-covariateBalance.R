# @file cohort-method-covariateBalance
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
#' The user interface to the cohort method covariate balance results
#' 
#' @export
cohortMethodCovariateBalanceViewer <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::uiOutput(outputId = ns("hoverInfoBalanceScatter")),
    
    plotly::plotlyOutput(ns("balancePlot")),
    shiny::uiOutput(outputId = ns("balancePlotCaption")),
    
    shiny::downloadButton(
      ns('downloadCovariateBalance'), 
      label = "Download"
    ),
    
    shiny::textInput(ns("covariateHighlight"), "Highlight covariates containing:", ),
    shiny::actionButton(ns("covariateHighlightButton"), "Highlight")
    
  )
  
}


#' The module server for rendering the covariate balance plot
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param connectionHandler the connection to the PLE results database
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#' @param metaAnalysisDbIds metaAnalysisDbIds
#'
#' @return
#' the PLE covariate balance content server
#' 
#' @export
cohortMethodCovariateBalanceServer <- function(
    id, 
    selectedRow, 
    connectionHandler, 
    resultDatabaseSettings,
    metaAnalysisDbIds = NULL) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      balance <- shiny::reactive({
        row <- selectedRow()
        if(is.null(row$targetId)){
          return(NULL)
        }
        balance <- tryCatch({
          getCohortMethodCovariateBalanceShared(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          targetId = row$targetId,
          comparatorId = row$comparatorId,
          databaseId = row$databaseId,
          analysisId = row$analysisId)},
          error = function(e){return(NULL)}
        )
        return(balance)
      })
      

      textSearchCohortMethod <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(
        input$covariateHighlightButton,{
          
          textSearchCohortMethod(input$covariateHighlight)
          
        }
        )
      
      balancePlot <- shiny::reactive({
        if (is.null(balance()) || nrow(balance()) == 0) {
          return(NULL)
        } else {
          plot <- plotCohortMethodCovariateBalanceScatterPlotNew(
            balance = balance(),
            beforeLabel = "Before propensity score adjustment",
            afterLabel = "After propensity score adjustment",
            textsearch = textSearchCohortMethod
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
      
      ## download buttons
      output$downloadCovariateBalance <- shiny::downloadHandler(
        filename = function() {
          paste('covariate-balance-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          utils::write.csv(
            balance() %>% 
              dplyr::select("covariateName", "absBeforeMatchingStdDiff", "absAfterMatchingStdDiff")
            , con)
        }
      )
      
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
          balanceSummary <- getCohortMethodCovariateBalanceSummary(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetId = row$targetId,
            comparatorId = row$comparatorId,
            analysisId = row$analysisId,
            databaseId = row$analysisId,
            beforeLabel = paste("Before", row$psStrategy),
            afterLabel = paste("After", row$psStrategy)
            )
          plot <- plotCohortMethodCovariateBalanceSummary(
            balanceSummary,
            threshold = 0.1,
            beforeLabel = paste("Before", row$psStrategy),
            afterLabel = paste("After", row$psStrategy)
          )
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

getCohortMethodCovariateBalanceShared <- function(
    connectionHandler,
    resultDatabaseSettings,
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
        @results_schema.@cm_table_prefixshared_covariate_balance cmscb 
        JOIN @results_schema.@cm_table_prefixcovariate cmc ON cmscb.covariate_id = cmc.covariate_id AND cmscb.analysis_id = cmc.analysis_id AND cmscb.database_id = cmc.database_id -- database_id optional
       -- JOIN @results_schema.@cm_table_prefixcovariate_analysis cmca ON cmca.analysis_id = cmc.analysis_id  -- question: shouldn't we have a covariate_analysis_id in @table_prefixcovariate table?
      WHERE
        cmscb.target_id = @target_id
        AND cmscb.comparator_id = @comparator_id
        AND cmscb.analysis_id = @analysis_id
        AND cmscb.database_id = '@database_id'
    "
    
    shiny::incProgress(1/3, detail = paste("Extracting"))
    result <- connectionHandler$queryDb(
      sql = sql,
      results_schema = resultDatabaseSettings$schema,
      cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
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


getCohortMethodCovariateBalanceSummary <- function(
    connectionHandler, 
    resultDatabaseSettings,
    databaseId,
    targetId, 
    comparatorId, analysisId,
    beforeLabel = "Before matching",
    afterLabel = "After matching"
    ) {
  
  balance <- getCohortMethodCovariateBalanceShared(
    connectionHandler = connectionHandler,
    targetId = targetId,
    comparatorId = comparatorId,
    analysisId = analysisId,
    resultDatabaseSettings = resultDatabaseSettings,
    databaseId = databaseId
  )
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



plotCohortMethodCovariateBalanceScatterPlotNew <- function(
    balance,
    beforeLabel = "Before propensity score adjustment",
    afterLabel = "After propensity score adjustment",
    textsearch = shiny::reactiveVal(NULL)
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



plotCohortMethodCovariateBalanceSummary <- function(balanceSummary,
                                                    threshold = 0,
                                                    beforeLabel = "Before matching",
                                                    afterLabel = "After matching") {
  balanceSummary <- balanceSummary[rev(order(balanceSummary$databaseId)), ]
  dbs <- data.frame(databaseId = unique(balanceSummary$databaseId),
                    x = 1:length(unique(balanceSummary$databaseId)))
  vizData <- merge(balanceSummary, dbs)
  
  vizData$type <- factor(vizData$type, levels = c(beforeLabel, afterLabel))
  
  plot <- ggplot2::ggplot(vizData, ggplot2::aes(x = .data$x,
                                                ymin = .data$ymin,
                                                lower = .data$lower,
                                                middle = .data$median,
                                                upper = .data$upper,
                                                ymax = .data$ymax,
                                                group = .data$databaseId)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$ymin, ymax = .data$ymin), size = 1) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$ymax, ymax = .data$ymax), size = 1) +
    ggplot2::geom_boxplot(stat = "identity", fill = grDevices::rgb(0, 0, 0.8, alpha = 0.25), size = 1) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::scale_x_continuous(limits = c(0.5, max(vizData$x) + 1.75)) +
    ggplot2::scale_y_continuous("Standardized difference of mean") +
    ggplot2::coord_flip() +
    ggplot2::facet_grid(~type) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(color = "#AAAAAA"),
                   panel.background = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = 11),
                   axis.title.x = ggplot2::element_text(size = 11),
                   axis.ticks.x = ggplot2::element_line(color = "#AAAAAA"),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(size = 11),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines"))
  
  if (threshold != 0) {
    plot <- plot + ggplot2::geom_hline(yintercept = c(threshold, -threshold), linetype = "dotted")
  }
  after <- vizData[vizData$type == afterLabel, ]
  after$max <- pmax(abs(after$ymin), abs(after$ymax))
  text <- data.frame(y = rep(c(after$x, nrow(after) + 1.25) , 3),
                     x = rep(c(1,2,3), each = nrow(after) + 1),
                     label = c(c(as.character(after$databaseId),
                                 "Source",
                                 formatC(after$covariateCount, big.mark = ",", format = "d"),
                                 "Covariate\ncount",
                                 formatC(after$max,  digits = 2, format = "f"),
                                 paste(afterLabel, "max(absolute)", sep = "\n"))),
                     dummy = "")
  
  data_table <- ggplot2::ggplot(text, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label)) +
    ggplot2::geom_text(size = 4, hjust=0, vjust=0.5) +
    ggplot2::geom_hline(ggplot2::aes(yintercept=nrow(after) + 0.5)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "none",
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(colour="white"),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_line(colour="white"),
                   strip.background = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines")) +
    ggplot2::labs(x="",y="") +
    ggplot2::facet_grid(~dummy) +
    ggplot2::coord_cartesian(xlim=c(1,4), ylim = c(0.5, max(vizData$x) + 1.75))
  
  plot <- gridExtra::grid.arrange(data_table, plot, ncol = 2)
  return(plot)
}
