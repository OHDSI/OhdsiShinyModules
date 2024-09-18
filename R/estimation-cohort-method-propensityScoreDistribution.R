# @file cohort-method-propensityScoreDistribution
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


#' The module viewer for rendering the propensity score distribution
#'
#' @param id the unique reference id for the module
#' @family {Estimation}
#' @return
#' The user interface to the cohort method propensity score distribution
#' 
#' @export
cohortMethodPropensityScoreDistViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::plotOutput(outputId = ns("psDistPlot")),
    shiny::div(shiny::strong("Figure 2."),"Preference score distribution. The preference score is a transformation of the propensity score
                                                                                                         that adjusts for differences in the sizes of the two treatment groups. A higher overlap indicates subjects in the
                                                                                                         two groups were more similar in terms of their predicted probability of receiving one treatment over the other."),
    shiny::div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
               shiny::downloadButton(outputId = ns("downloadPsDistPlotPng"),
                       label = "Download plot as PNG"),
               shiny::downloadButton(outputId = ns("downloadPsDistPlotPdf"),
                       label = "Download plot as PDF"))
  )
}


#' The module server for rendering a PLE propensity score distribution
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param connectionHandler the connection to the PLE results database
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#' @param metaAnalysisDbIds metaAnalysisDbIds
#' @family {Estimation}
#' @return
#' the PLE propensity score distribution content server
#' 
#' @export
cohortMethodPropensityScoreDistServer <- function(
    id, 
    selectedRow, 
    connectionHandler, 
    resultDatabaseSettings,
    metaAnalysisDbIds = F
    ) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      psDistPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row$targetId)) {
          return(NULL)
        } else {
          ps <- getCohortMethodPs(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetId = row$targetId,
            comparatorId = row$comparatorId,
            analysisId = row$analysisId,
            databaseId = row$databaseId
          )
          
          if (nrow(ps) == 0) {
            return(NULL) #TODO: handle more gracefully
          }
          
          targetName <- row$target
            
          comparatorName <- row$comparator
          
          equipoiseStatistic <- getCohortMethodEquipoise(
            connectionHandler = connectionHandler,
            resultDatabaseSettings = resultDatabaseSettings,
            targetId = row$targetId,
            comparatorId = row$comparatorId,
            outcomeId = row$outcomeId,
            analysisId = row$analysisId,
            databaseId = row$databaseId
          ) 
          
          plot <- plotCohortMethodPs(ps, targetName, comparatorName, equipoiseStatistic)
          return(plot)
        }
      })
      
      output$psDistPlot <- shiny::renderPlot({
        return(psDistPlot())
      })
      
      output$downloadPsDistPlotPng <- shiny::downloadHandler(filename = "Ps.png",
                                                             contentType = "image/png",
                                                             content = function(file) {
                                                               ggplot2::ggsave(file, plot = psDistPlot(), width = 5, height = 3.5, dpi = 400)
                                                             })
      
      output$downloadPsDistPlotPdf <- shiny::downloadHandler(filename = "Ps.pdf",
                                                             contentType = "application/pdf",
                                                             content = function(file) {
                                                               ggplot2::ggsave(file = file, plot = psDistPlot(), width = 5, height = 3.5)
                                                             })
      
    }
  )
}

getCohortMethodEquipoise <- function(
    connectionHandler, 
    resultDatabaseSettings,
    targetId, 
    comparatorId, 
    outcomeId,
    analysisId, 
    databaseId = NULL
) {
  if(is.null(targetId)){
    return(NULL)
  }
  sql <- "
    SELECT
      database_id, target_id, comparator_id, outcome_id, analysis_id, equipoise
    FROM
      @schema.@cm_table_prefixdiagnostics_summary cmpsd
    WHERE
      cmpsd.target_id = @target_id
      AND cmpsd.comparator_id = @comparator_id
      AND cmpsd.analysis_id = @analysis_id 
      AND cmpsd.outcome_id = @outcome_id 
  "
  if(!is.null(databaseId)) {
    sql <- paste(sql, paste("AND cmpsd.database_id = '@database_id'"), collapse = "\n")
  }
  
  
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
  
  
  if (!is.null(databaseId)) {
    result$databaseId <- NULL
  }
  
  eq <- round(result$equipoise, 4)
  
  return(eq)
}

getCohortMethodPs <- function(
    connectionHandler, 
    resultDatabaseSettings,
    targetId, 
    comparatorId, 
    analysisId, 
    databaseId = NULL
) {
  if(is.null(targetId)){
    return(NULL)
  }
  sql <- "
    SELECT
      *
    FROM
      @schema.@cm_table_prefixpreference_score_dist cmpsd
    WHERE
      cmpsd.target_id = @target_id
      AND cmpsd.comparator_id = @comparator_id
      AND cmpsd.analysis_id = @analysis_id
  "
  if(!is.null(databaseId)) {
    sql <- paste(sql, paste("AND cmpsd.database_id = '@database_id'"), collapse = "\n")
  }
  
  
  ps <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
    target_id = targetId,
    comparator_id = comparatorId,
    analysis_id = analysisId,
    database_id = databaseId
  )
  
  
  if (!is.null(databaseId)) {
    ps$databaseId <- NULL
  }
  return(ps)
}

# CohortMethod-propensityScoreDist
plotCohortMethodPs <- function(ps, targetName, comparatorName, equipoiseStatistic) {
  if(is.null(ps$preferenceScore)){
    return(NULL)
  }
  if(is.null(ps$databaseId)) {
    ps <- rbind(data.frame(x = ps$preferenceScore, y = ps$targetDensity, group = targetName),
                data.frame(x = ps$preferenceScore, y = ps$comparatorDensity, group = comparatorName))
    
  } else {
    ps <- rbind(data.frame(x = ps$preferenceScore, y = ps$targetDensity, databaseId = ps$databaseId, group = targetName),
                data.frame(x = ps$preferenceScore, y = ps$comparatorDensity, databaseId = ps$databaseId, group = comparatorName))
  }
  ps$group <- factor(ps$group, levels = c(as.character(targetName), as.character(comparatorName)))
  theme <- ggplot2::element_text(colour = "#000000", size = 12, margin = ggplot2::margin(0, 0.5, 0, 0.1, "cm"))
  plot <- ggplot2::ggplot(ps,
                          ggplot2::aes(x = .data$x, y = .data$y, color = .data$group, group = .data$group, fill = .data$group)) +
    ggplot2::geom_density(stat = "identity") +
    ggplot2::scale_fill_manual(values = c(grDevices::rgb(0.8, 0, 0, alpha = 0.5),
                                          grDevices::rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_color_manual(values = c(grDevices::rgb(0.8, 0, 0, alpha = 0.5),
                                           grDevices::rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_x_continuous("Preference score", limits = c(0, 1)) +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::ggtitle(paste0("Equipoise Statistic = ", equipoiseStatistic)) +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.text = theme,
                   axis.text = theme,
                   axis.title = theme,
                   plot.title = ggplot2::element_text(vjust = -20, face = "bold")
                   ) + 
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2))
  if (!is.null(ps$databaseId)) {
    plot <- plot + ggplot2::facet_grid(databaseId~., switch = "both") +
      ggplot2::theme(legend.position = "right")
  }
  return(plot)
}

