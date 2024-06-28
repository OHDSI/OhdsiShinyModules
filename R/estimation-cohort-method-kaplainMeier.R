# @file cohort-method-kaplainMeier
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


#' The module viewer for rendering the PLE Kaplan Meier curve
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The module viewer for Kaplan Meier objects
#' 
#' @export
cohortMethodKaplanMeierViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::plotOutput(outputId = ns("kaplanMeierPlot"), height = 550),
    shiny::uiOutput(outputId = ns("kaplanMeierPlotPlotCaption")),
    shiny::div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
               shiny::downloadButton(outputId = ns("downloadKaplanMeierPlotPng"),
                       label = "Download plot as PNG"),
               shiny::downloadButton(outputId = ns("downloadKaplanMeierPlotPdf"),
                       label = "Download plot as PDF"))
  )
}

#' The module server for rendering the Kaplan Meier curve
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param connectionHandler the connection to the PLE results database
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#'
#' @return
#' the PLE Kaplain Meier content server
#' 
#' @export
cohortMethodKaplanMeierServer <- function(
    id, 
    selectedRow, 
    connectionHandler, 
    resultDatabaseSettings
    ) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      

      kaplanMeierPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          km <- getCohortMethodKaplanMeier(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetId = row$targetId,
            comparatorId = row$comparatorId,
            outcomeId = row$outcomeId,
            databaseId = row$databaseId,
            analysisId = row$analysisId
          )
          
          # hack to fix data insert replacing NA with 0
          removeInd <- km$targetAtRisk == 0 & km$comparatorAtRisk == 0
          km$targetAtRisk[removeInd] <- NA
          km$comparatorAtRisk[removeInd] <- NA
          
          targetName <- row$target
          comparatorName <- row$comparator
          
          plot <- plotCohortMethodKaplanMeier(
            kaplanMeier = km,
            targetName = targetName,
            comparatorName = comparatorName
            )
          return(plot)
        }
      })
      
      output$kaplanMeierPlot <- shiny::renderPlot({
        return(kaplanMeierPlot())
      }, res = 100)
      
      output$downloadKaplanMeierPlotPng <- shiny::downloadHandler(filename = "KaplanMeier.png",
                                                                  contentType = "image/png",
                                                                  content = function(file) {
                                                                    ggplot2::ggsave(file, plot = kaplanMeierPlot(), width = 7, height = 5, dpi = 400)
                                                                  })
      
      output$downloadKaplanMeierPlotPdf <- shiny::downloadHandler(filename = "KaplanMeier.pdf",
                                                                  contentType = "application/pdf",
                                                                  content = function(file) {
                                                                    ggplot2::ggsave(file = file, plot = kaplanMeierPlot(), width = 7, height = 5)
                                                                  })
      
      output$kaplanMeierPlotPlotCaption <- shiny::renderUI({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          text <- "<strong>Figure 5.</strong> Kaplan Meier plot, showing survival as a function of time. This plot
      is adjusted using the propensity score: The target curve (<em>%s</em>) shows the actual observed survival. The
      comparator curve (<em>%s</em>) applies reweighting to approximate the counterfactual of what the target survival
      would look like had the target cohort been exposed to the comparator instead. The shaded area denotes
      the 95 percent confidence interval."
          return(shiny::HTML(sprintf(text, row$target, row$comparator)))
        }
      })
      
      
    }
  )
}



getCohortMethodKaplanMeier <- function(
    connectionHandler, 
    resultDatabaseSettings,
    targetId, 
    comparatorId, 
    outcomeId, 
    databaseId, 
    analysisId
) {
  
  sql <- "
  SELECT
    *
  FROM
    @schema.@cm_table_prefixkaplan_meier_dist cmkmd
  WHERE
    cmkmd.target_id = @target_id
    AND cmkmd.comparator_id = @comparator_id
    AND cmkmd.outcome_id = @outcome_id
    AND cmkmd.analysis_id = @analysis_id
    AND cmkmd.database_id = '@database_id';
  "
  
  return(
    connectionHandler$queryDb(
      sql = sql,
      schema = resultDatabaseSettings$schema,
      cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
      #database_table = resultDatabaseSettings$databaseTable,
      target_id = targetId,
      comparator_id = comparatorId,
      outcome_id = outcomeId,
      analysis_id = analysisId,
      database_id = databaseId
    )
  )
}


# CohortMethod-kaplainMeier
plotCohortMethodKaplanMeier <- function(
    kaplanMeier, 
    targetName, 
    comparatorName
    ) {
  
  data <- rbind(
    data.frame(
      time = kaplanMeier$time,
      s = kaplanMeier$targetSurvival,
      lower = kaplanMeier$targetSurvivalLb,
      upper = kaplanMeier$targetSurvivalUb,
      strata = ' Target' #paste0(" ", targetName, "    ")
    ),
    data.frame(
      time = kaplanMeier$time,
      s = kaplanMeier$comparatorSurvival,
      lower = kaplanMeier$comparatorSurvivalLb,
      upper = kaplanMeier$comparatorSurvivalUb,
      strata = ' Comparator'#paste0(" ", comparatorName)
    )
  )
  
  xlims <- c(-max(data$time)/40, max(data$time))
  ylims <- c(min(data$lower), 1)
  xLabel <- "Time in days"
  yLabel <- "Survival probability"
  xBreaks <- kaplanMeier$time[!is.na(kaplanMeier$targetAtRisk)]
  
  # add next time
  data <- data %>% 
    dplyr::group_by(.data$strata) %>%
    dplyr::arrange(.data$time,.data$s, .by_group = TRUE) %>%
    dplyr::mutate(
      nexttime = dplyr::lead(.data$time)
    )
  
  plot <- ggplot2::ggplot(data, ggplot2::aes(
    x = .data$time,
    xmin = .data$time,
    xmax = .data$nexttime,
    y = .data$s,
    color = .data$strata,
    fill = .data$strata,
    ymin = .data$lower,
    ymax = .data$upper)
  ) +
    ggplot2::geom_rect(
      color = grDevices::rgb(0, 0, 0, alpha = 0)
      ) +
    ggplot2::geom_step(size = 1) +
    ggplot2::scale_color_manual(
      values = c(grDevices::rgb(0.8, 0, 0, alpha = 0.8),
                 grDevices::rgb(0, 0, 0.8, alpha = 0.8))) +
    ggplot2::scale_fill_manual(
      values = c(grDevices::rgb(0.8, 0, 0, alpha = 0.3),
                 grDevices::rgb(0, 0, 0.8, alpha = 0.3))) +
    ggplot2::scale_x_continuous(
      xLabel, 
      limits = xlims, 
      breaks = xBreaks
      ) + 
    ggplot2::scale_y_continuous(
        yLabel, 
        limits = ylims
      ) + ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        legend.position = "top",
        legend.key.size = ggplot2::unit(1, "lines"),
        plot.title = ggplot2::element_text(hjust = 0.5)
      ) + 
    ggplot2::theme(
        axis.title.y = ggplot2::element_text(vjust = -10)
      )
  
  targetAtRisk <- kaplanMeier$targetAtRisk[!is.na(kaplanMeier$targetAtRisk)]
  comparatorAtRisk <- kaplanMeier$comparatorAtRisk[!is.na(kaplanMeier$comparatorAtRisk)]
  
  labels <- data.frame(
    x = c(0, xBreaks, xBreaks),
    y = as.factor(
      c("Number at risk",
        rep('Target', length(xBreaks)),
        rep('Comparator', length(xBreaks))
        )
    ),
    label = c(
      "",
      formatC(targetAtRisk, big.mark = ",", mode = "integer"),
      formatC(comparatorAtRisk, big.mark = ",", mode = "integer")
      )
    )
  labels$y <- factor(labels$y, levels = c('Comparator','Target', "Number at risk"))
  
  dataTable <- ggplot2::ggplot(
    data = labels, 
    ggplot2::aes(
      x = .data$x, 
      y = .data$y, 
      label = .data$label
    )
  ) + ggplot2::geom_text(
    size = 3.5, 
    vjust = 0.5
  ) + ggplot2::scale_x_continuous(
    xLabel,
    limits = xlims,
    breaks = xBreaks) + ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none",
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(color = "white"),
      axis.title.x = ggplot2::element_text(color = "white"),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(color = "white")
    )
  
  plots <- list(plot, dataTable)
  grobs <- widths <- list()
  for (i in 1:length(plots)) {
    grobs[[i]] <- ggplot2::ggplotGrob(plots[[i]])
    widths[[i]] <- grobs[[i]]$widths[2:5]
  }
  maxwidth <- do.call(grid::unit.pmax, widths)
  for (i in 1:length(grobs)) {
    grobs[[i]]$widths[2:5] <- as.list(maxwidth)
  }
  plot <- gridExtra::grid.arrange(grobs[[1]], grobs[[2]], heights = c(400, 100))
  
  return(plot)
}
