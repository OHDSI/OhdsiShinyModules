# @file cohort-method-attrition
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

#' The module viewer for rendering the PLE attrition results
#'
#' @param id the unique reference id for the module
#'
#' @return
#' The user interface to the cohort method attrition
#' 
#' @export
cohortMethodAttritionViewer <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    shiny::plotOutput(outputId = ns("attritionPlot"), width = 600, height = 600),
    shiny::uiOutput(outputId = ns("attritionPlotCaption")),
    shiny::div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
               shiny::downloadButton(outputId = ns("downloadAttritionPlotPng"),
                       label = "Download diagram as PNG"),
               shiny::downloadButton(outputId = ns("downloadAttritionPlotPdf"),
                       label = "Download diagram as PDF"))
  )
}

#' The module server for rendering the PLE attrition results
#'
#' @param id the unique reference id for the module
#' @param selectedRow the selected row from the main results table 
#' @param connectionHandler the connection to the PLE results database
#' @param resultDatabaseSettings a list containing the result schema and prefixes
#'
#' @return
#' the PLE attrition results content server
#' 
#' @export
cohortMethodAttritionServer <- function(
    id, 
    selectedRow, 
    connectionHandler,
    resultDatabaseSettings
    ) {

  shiny::moduleServer(
    id,
    function(input, output, session) {
      
     attritionPlot <- shiny::reactive({
        attrition <- getCohortMethodAttrition(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          selectedRow = selectedRow
        )
        if(!is.null(attrition)){
          plot <- drawCohortMethodAttritionDiagram(attrition)
          return(plot)
        } else{
          return(NULL)
        } 
      })

     output$attritionPlot <- shiny::renderPlot({
       return(attritionPlot())
     })
     
     
      output$downloadAttritionPlotPng <- shiny::downloadHandler(filename = "Attrition.png",
                                                                contentType = "image/png",
                                                                content = function(file) {
                                                                  ggplot2::ggsave(file, plot = attritionPlot(), width = 6, height = 7, dpi = 400)
                                                                })
      
 
      output$downloadAttritionPlotPdf <- shiny::downloadHandler(filename = "Attrition.pdf",
                                                                contentType = "application/pdf",
                                                                content = function(file) {
                                                                  ggplot2::ggsave(file = file, plot = attritionPlot(), width = 6, height = 7)
                                                                })
      
      output$attritionPlotCaption <- shiny::renderUI({
        if (is.null(selectedRow()$target)) {
          return(NULL)
        } else {
          text <- "<strong>Figure 1.</strong> Attrition diagram, showing the Number of subjects in the target (<em>%s</em>) and
      comparator (<em>%s</em>) group after various stages in the analysis."
          return(shiny::HTML(sprintf(text, selectedRow()$target, selectedRow()$comparator)))
        }
      })
      
    }
  )
}


getCohortMethodAttrition <- function(
    connectionHandler, 
    resultDatabaseSettings, 
    selectedRow
) {
  
  if(is.null(selectedRow()$targetId)){
    return(NULL)
  }
  
  sql <- "
  SELECT cmat.*
  FROM
    @schema.@cm_table_prefixattrition cmat
  WHERE
  cmat.target_id = @target_id
  AND cmat.comparator_id = @comparator_id
  AND cmat.outcome_id = @outcome_id
  AND cmat.analysis_id = @analysis_id
  AND cmat.database_id = '@database_id';
  "
  result <- connectionHandler$queryDb(
    sql = sql,
    schema = resultDatabaseSettings$schema,
    cm_table_prefix = resultDatabaseSettings$cmTablePrefix,
    #database_table = resultDatabaseSettings$databaseTable,
    target_id = selectedRow()$targetId,
    comparator_id = selectedRow()$comparatorId,
    outcome_id = selectedRow()$outcomeId,
    analysis_id = selectedRow()$analysisId,
    database_id = selectedRow()$databaseId
  )
  targetAttrition <- result[result$exposureId == selectedRow()$targetId, ]
  comparatorAttrition <- result[result$exposureId == selectedRow()$comparatorId, ]
  colnames(targetAttrition)[colnames(targetAttrition) == "subjects"] <- "targetPersons"
  targetAttrition$exposureId <- NULL
  colnames(comparatorAttrition)[colnames(comparatorAttrition) == "subjects"] <- "comparatorPersons"
  comparatorAttrition$exposureId <- NULL
  result <- merge(targetAttrition, comparatorAttrition)
  result <- result[order(result$sequenceNumber), ]
  
  return(result)
}




drawCohortMethodAttritionDiagram <- function(
    attrition,
    targetLabel = "Target",
    comparatorLabel = "Comp"
) {
  addStep <- function(data, attrition, row) {
    label <- paste(strwrap(as.character(attrition$description[row]), width = 30), collapse = "\n")
    data$leftBoxText[length(data$leftBoxText) + 1] <- label
    data$rightBoxText[length(data$rightBoxText) + 1] <- paste(targetLabel,
                                                              ": n = ",
                                                              format(data$currentTarget - attrition$targetPersons[row], scientific = FALSE),
                                                              "\n",
                                                              comparatorLabel,
                                                              ": n = ",
                                                              format(data$currentComparator - attrition$comparatorPersons[row], scientific = FALSE),
                                                              sep = "")
    data$currentTarget <- attrition$targetPersons[row]
    data$currentComparator <- attrition$comparatorPersons[row]
    return(data)
  }
  data <- list(leftBoxText = c(paste("Exposed:\n",
                                     targetLabel,
                                     ": n = ",
                                     format(attrition$targetPersons[1], scientific = FALSE),
                                     "\n",
                                     comparatorLabel,
                                     ": n = ",
                                     format(attrition$comparatorPersons[1], scientific = FALSE),
                                     sep = "")), 
               rightBoxText = c(""), 
               currentTarget = attrition$targetPersons[1], 
               currentComparator = attrition$comparatorPersons[1]
               )
  for (i in 2:nrow(attrition)) {
    data <- addStep(data, attrition, i)
  }
  
  
  data$leftBoxText[length(data$leftBoxText) + 1] <- paste("Study Population:\n",
                                                          targetLabel,
                                                          ": n = ",
                                                          format(data$currentTarget, scientific = FALSE),
                                                          "\n",
                                                          comparatorLabel,
                                                          ": n = ",
                                                          format(data$currentComparator, scientific = FALSE),
                                                          sep = "")
  leftBoxText <- data$leftBoxText
  rightBoxText <- data$rightBoxText
  nSteps <- length(leftBoxText)
  
  boxHeight <- (1/nSteps) - 0.03
  boxWidth <- 0.45
  shadowOffset <- 0.01
  arrowLength <- 0.01
  x <- function(x) {
    return(0.25 + ((x - 1)/2))
  }
  y <- function(y) {
    return(1 - (y - 0.5) * (1/nSteps))
  }
  
  downArrow <- function(p, x1, y1, x2, y2) {
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x1, y = y1, xend = x2, yend = y2))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 + arrowLength,
                                                       yend = y2 + arrowLength))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 - arrowLength,
                                                       yend = y2 + arrowLength))
    return(p)
  }
  rightArrow <- function(p, x1, y1, x2, y2) {
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x1, y = y1, xend = x2, yend = y2))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 - arrowLength,
                                                       yend = y2 + arrowLength))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 - arrowLength,
                                                       yend = y2 - arrowLength))
    return(p)
  }
  box <- function(p, x, y) {
    p <- p + ggplot2::geom_rect(ggplot2::aes_string(xmin = x - (boxWidth/2) + shadowOffset,
                                                    ymin = y - (boxHeight/2) - shadowOffset,
                                                    xmax = x + (boxWidth/2) + shadowOffset,
                                                    ymax = y + (boxHeight/2) - shadowOffset), fill = grDevices::rgb(0,
                                                                                                                    0,
                                                                                                                    0,
                                                                                                                    alpha = 0.2))
    p <- p + ggplot2::geom_rect(ggplot2::aes_string(xmin = x - (boxWidth/2),
                                                    ymin = y - (boxHeight/2),
                                                    xmax = x + (boxWidth/2),
                                                    ymax = y + (boxHeight/2)), fill = grDevices::rgb(0.94,
                                                                                                     0.94,
                                                                                                     0.94), color = "black")
    return(p)
  }
  label <- function(p, x, y, text, hjust = 0) {
    p <- p + ggplot2::geom_text(ggplot2::aes_string(x = x, y = y, label = paste("\"", substring(text,1,100), "\"",
                                                                                sep = "")),
                                hjust = hjust,
                                size = 3.7)
    return(p)
  }
  
  p <- ggplot2::ggplot()
  for (i in 2:nSteps - 1) {
    p <- downArrow(p, x(1), y(i) - (boxHeight/2), x(1), y(i + 1) + (boxHeight/2))
    p <- label(p, x(1) + 0.02, y(i + 0.5), "Y")
  }
  for (i in 2:(nSteps - 1)) {
    p <- rightArrow(p, x(1) + boxWidth/2, y(i), x(2) - boxWidth/2, y(i))
    p <- label(p, x(1.5), y(i) - 0.02, "N", 0.5)
  }
  for (i in 1:nSteps) {
    p <- box(p, x(1), y(i))
  }
  for (i in 2:(nSteps - 1)) {
    p <- box(p, x(2), y(i))
  }
  for (i in 1:nSteps) {
    p <- label(p, x(1) - boxWidth/2 + 0.02, y(i), text = leftBoxText[i])
  }
  for (i in 2:(nSteps - 1)) {
    p <- label(p, x(2) - boxWidth/2 + 0.02, y(i), text = rightBoxText[i])
  }
  p <- p + ggplot2::theme(legend.position = "none",
                          plot.background = ggplot2::element_blank(),
                          panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank(),
                          panel.border = ggplot2::element_blank(),
                          panel.background = ggplot2::element_blank(),
                          axis.text = ggplot2::element_blank(),
                          axis.title = ggplot2::element_blank(),
                          axis.ticks = ggplot2::element_blank())
  
  return(p)
}
