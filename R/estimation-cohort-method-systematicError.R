# @file cohort-method-systematicError
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


#' The module viewer for rendering the PLE systematic error objects
#'
#' @param id the unique reference id for the module
#' @family {Estimation}
#' @return
#' The user interface to the cohort method systematic error module
#' 
#' @export
cohortMethodSystematicErrorViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::plotOutput(outputId = ns("systematicErrorPlot")),
    shiny::div(shiny::strong("Figure 4."),"Systematic error. Effect size estimates for the negative controls (true hazard ratio = 1)
                                                                                    and positive controls (true hazard ratio > 1), before and after calibration. Estimates below the diagonal dashed
                                                                                    lines are statistically significant (alpha = 0.05) different from the true effect size. A well-calibrated
                                                                                    estimator should have the true effect size within the 95 percent confidence interval 95 percent of times. 
                                                                                    The expected absolute systematic error (EASE) statistic is also shown at the top of the figure."),
    shiny::div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
               shiny::downloadButton(outputId = ns("downloadSystematicErrorPlotPng"),
                                     label = "Download plot as PNG"),
               shiny::downloadButton(outputId = ns("downloadSystematicErrorPlotPdf"),
                                     label = "Download plot as PDF")
    )
    )

}



#' The module server for rendering the systematic error objects
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param connectionHandler  the connection handler to the result databases
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#' @family {Estimation}
#' @return
#' the PLE systematic error content server
#' 
#' @export
cohortMethodSystematicErrorServer <- function(
    id, 
    selectedRow, 
    connectionHandler, 
    resultDatabaseSettings
    ) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      systematicErrorPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          controlResults <- getCohortMethodControlResults(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetId = row$targetId,
            comparatorId = row$comparatorId,
            analysisId = row$analysisId,
            databaseId = row$databaseId
            )
          
          ease <- estimationGetEase(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetId = row$targetId,
            comparatorId = row$comparatorId,
            analysisId = row$analysisId,
            databaseId = row$databaseId
          )
          
          # remove the RR zeros that replace NAs during data upload 
          controlResults$logRr[controlResults$logRr == 0] <- NA
          controlResults$ci95Lb[controlResults$ci95Lb == 0] <- NA
          controlResults$ci95Ub[controlResults$ci95Ub == 0] <- NA
          controlResults$calibratedLogRr[controlResults$calibratedLogRr == 0] <- NA
          controlResults$calibratedCi95Lb[controlResults$calibratedCi95Lb == 0] <- NA
          controlResults$calibratedCi95Ub[controlResults$calibratedCi95Ub == 0] <- NA
          
          plot <- plotCohortMethodScatter(controlResults, ease)
          return(plot)
        }
      })
      
      output$systematicErrorPlot <- shiny::renderPlot({
        return(systematicErrorPlot())
      })
      
      picName <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          picName <- paste0("Target=", stringr::str_trunc(row$target, 35), "_", 
                            "Comparator=",stringr::str_trunc(row$comparator, 35), "_",
                            "Analysis=",row$description, "_",
                            "DB=",row$cdmSourceAbbreviation, "_",
                 Sys.Date())
        }
          
          return(picName)
          
        })
      
      output$downloadSystematicErrorPlotPng <- shiny::downloadHandler(
            
        filename = paste0("SystematicErrorPlot_", picName(), ".png"),
        contentType = "image/png",
        content = function(file) {
          ggplot2::ggsave(file, plot = systematicErrorPlot(), width = 12, height = 5.5, dpi = 400)
        }
      )
      
      output$downloadSystematicErrorPlotPdf <- shiny::downloadHandler(
        filename = paste0("SystematicErrorPlot_", picName(), ".pdf"),
        contentType = "application/pdf",
        content = function(file) {
          ggplot2::ggsave(file = file, plot = systematicErrorPlot(), width = 12, height = 5.5)
        }
      )
      
    }
  )
}


getCohortMethodControlResults <- function(
    connectionHandler, 
    resultDatabaseSettings, 
    targetId,
    comparatorId, 
    analysisId, 
    databaseId = NULL,
    includePositiveControls = TRUE, 
    emptyAsNa = TRUE
) {
  
  sql <- "
    SELECT
      cmr.*,
      cmtco.true_effect_size effect_size
    FROM
      @schema.@cm_table_prefixresult cmr
      JOIN @schema.@cm_table_prefixtarget_comparator_outcome cmtco 
      ON cmr.target_id = cmtco.target_id AND cmr.comparator_id = cmtco.comparator_id AND cmr.outcome_id = cmtco.outcome_id
    WHERE
      cmtco.outcome_of_interest != 1
      AND cmr.target_id = @target_id
      AND cmr.comparator_id = @comparator_id
      AND cmr.analysis_id = @analysis_id
  "
  
  
  if (!is.null(databaseId)) {
    # update sql
    sql <- paste(sql, paste("AND cmr.database_id = '@database_id'"), collapse = "\n")
  }
  
  if (!includePositiveControls) {
    # update sql
    sql <- paste(sql, paste("AND cmtco.true_effect_size = 1"), collapse = "\n")
  }
  
  results <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
    target_id = targetId,
    comparator_id = comparatorId,
    analysis_id = analysisId,
    database_id = databaseId
  )
  
  if (emptyAsNa) {
    results[results == ''] <- NA
  }
  
  return(results)
}


plotCohortMethodScatter <- function(controlResults, ease) {
  
  if(nrow(controlResults)==0){
    return(NULL)
  }
  
  size <- 2
  labelY <- 0.7
  d <- rbind(data.frame(yGroup = "Uncalibrated",
                        logRr = controlResults$logRr,
                        seLogRr = controlResults$seLogRr,
                        ci95Lb = controlResults$ci95Lb,
                        ci95Ub = controlResults$ci95Ub,
                        trueRr = controlResults$effectSize),
             data.frame(yGroup = "Calibrated",
                        logRr = controlResults$calibratedLogRr,
                        seLogRr = controlResults$calibratedSeLogRr,
                        ci95Lb = controlResults$calibratedCi95Lb,
                        ci95Ub = controlResults$calibratedCi95Ub,
                        trueRr = controlResults$effectSize))
  d <- d[!is.na(d$logRr), ]
  d <- d[!is.na(d$ci95Lb), ]
  d <- d[!is.na(d$ci95Ub), ]
  if (nrow(d) == 0) {
    return(NULL)
  }
  d$Group <- as.factor(d$trueRr)
  d$Significant <- d$ci95Lb > d$trueRr | d$ci95Ub < d$trueRr
  temp1 <- stats::aggregate(Significant ~ Group + yGroup, data = d, length)
  temp2 <- stats::aggregate(Significant ~ Group + yGroup, data = d, mean)
  temp1$nLabel <- paste0(formatC(temp1$Significant, big.mark = ","), " estimates")
  temp1$Significant <- NULL
  
  temp2$meanLabel <- paste0(formatC(100 * (1 - temp2$Significant), digits = 1, format = "f"),
                            "% of CIs include ",
                            temp2$Group)
  temp2$Significant <- NULL
  dd <- merge(temp1, temp2)
  dd$tes <- as.numeric(as.character(dd$Group))
  
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  themeLA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 0)
  
  d$Group <- paste("True hazard ratio =", d$Group)
  dd$Group <- paste("True hazard ratio =", dd$Group)
  alpha <- 1 - min(0.95 * (nrow(d)/nrow(dd)/50000)^0.1, 0.95)
  plot <- ggplot2::ggplot(d, ggplot2::aes(x = .data$logRr, y = .data$seLogRr), environment = environment()) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "#AAAAAA", lty = 1, size = 0.5) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(.data$tes))/stats::qnorm(0.025), slope = 1/stats::qnorm(0.025)),
                         colour = grDevices::rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5,
                         data = dd) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(.data$tes))/stats::qnorm(0.975), slope = 1/stats::qnorm(0.975)),
                         colour = grDevices::rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5,
                         data = dd) +
    ggplot2::geom_point(size = size,
                        color = grDevices::rgb(0, 0, 0, alpha = 0.05),
                        alpha = alpha,
                        shape = 16) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_label(x = log(0.15),
                        y = 0.9,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = .data$nLabel),
                        size = 5,
                        data = dd) +
    ggplot2::geom_label(x = log(0.15),
                        y = labelY,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = .data$meanLabel),
                        size = 5,
                        data = dd) +
    ggplot2::scale_x_continuous("Hazard ratio",
                                limits = log(c(0.1, 10)),
                                breaks = log(breaks),
                                labels = breaks) +
    ggplot2::scale_y_continuous("Standard Error", limits = c(0, 1)) +
    ggplot2::facet_grid(yGroup ~ Group) +
    ggplot2::ggtitle(paste0("EASE Statistic = ", ease)) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   axis.title = theme,
                   legend.key = ggplot2::element_blank(),
                   strip.text.x = theme,
                   strip.text.y = theme,
                   strip.background = ggplot2::element_blank()
                   # ,
                   # legend.position = "top",
                   # legend.text = paste0("EASE = ", ease)
                   )
  
  return(plot)
}

estimationGetEase <- function(
    connectionHandler = connectionHandler,
    resultDatabaseSettings = resultDatabaseSettings,
    targetId =  targetId,
    comparatorId = comparatorId,
    analysisId = analysisId,
    databaseId = databaseId
){
  
  sql <- "
    SELECT DISTINCT
      dmd.cdm_source_abbreviation database_name,
      cmds.analysis_id,
      cmds.target_id,
      cmds.comparator_id,
      cmds.max_sdm,
      cmds.ease
    FROM
      @schema.@cm_table_prefixdiagnostics_summary cmds
      INNER JOIN @schema.@database_table dmd ON dmd.database_id = cmds.database_id
      
      where cmds.target_id = @target_id
      and cmds.comparator_id = @comparator_id
      and cmds.analysis_id = @analysis_id
      and cmds.database_id = '@database_id'
      ;
  "
  
  result <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
    database_table = resultDatabaseSettings$databaseTable,
    target_id = targetId,
    comparator_id = comparatorId,
    analysis_id = analysisId,
    database_id = databaseId
  )
  
  ease <- round(result$ease, 4)
  
  return(
    ease
  )
  
}


