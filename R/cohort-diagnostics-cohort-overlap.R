# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
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

### cohort overlap plot ##############
plotCohortOverlap <- function(data,
                              shortNameRef = NULL,
                              yAxis = "Percentages") {
  data <- data %>%
    addShortName(
      shortNameRef = shortNameRef,
      cohortIdColumn = "targetCohortId",
      shortNameColumn = "targetShortName"
    ) %>%
    addShortName(
      shortNameRef = shortNameRef,
      cohortIdColumn = "comparatorCohortId",
      shortNameColumn = "comparatorShortName"
    )

  plotData <- data %>%
    dplyr::mutate(
      absTOnlySubjects = abs(.data$tOnlySubjects),
      absCOnlySubjects = abs(.data$cOnlySubjects),
      absBothSubjects = abs(.data$bothSubjects),
      absEitherSubjects = abs(.data$eitherSubjects),
      signTOnlySubjects = dplyr::case_when(.data$tOnlySubjects < 0 ~ "<", TRUE ~ ""),
      signCOnlySubjects = dplyr::case_when(.data$cOnlySubjects < 0 ~ "<", TRUE ~ ""),
      signBothSubjects = dplyr::case_when(.data$bothSubjects < 0 ~ "<", TRUE ~ "")
    ) %>%
    dplyr::mutate(
      tOnlyString = paste0(
        .data$signTOnlySubjects,
        scales::comma(.data$absTOnlySubjects, accuracy = 1),
        " (",
        .data$signTOnlySubjects,
        scales::percent(.data$absTOnlySubjects /
                          .data$absEitherSubjects,
                        accuracy = 1
        ),
        ")"
      ),
      cOnlyString = paste0(
        .data$signCOnlySubjects,
        scales::comma(.data$absCOnlySubjects, accuracy = 1),
        " (",
        .data$signCOnlySubjects,
        scales::percent(.data$absCOnlySubjects /
                          .data$absEitherSubjects,
                        accuracy = 1
        ),
        ")"
      ),
      bothString = paste0(
        .data$signBothSubjects,
        scales::comma(.data$absBothSubjects, accuracy = 1),
        " (",
        .data$signBothSubjects,
        scales::percent(.data$absBothSubjects /
                          .data$absEitherSubjects,
                        accuracy = 1
        ),
        ")"
      )
    ) %>%
    dplyr::mutate(
      tooltip = paste0(
        "Database: ",
        .data$databaseName,
        "\n",
        "\n",
        .data$targetShortName,
        " only: ",
        .data$tOnlyString,
        "\nBoth: ",
        .data$bothString,
        "\n",
        .data$comparatorShortName,
        " only: ",
        .data$cOnlyString
      )
    ) %>%
    dplyr::select(
      "targetShortName",
      "comparatorShortName",
      "databaseId",
      "databaseName",
      "absTOnlySubjects",
      "absCOnlySubjects",
      "absBothSubjects",
      "tooltip"
    ) %>%
    tidyr::pivot_longer(
      cols = c(
        "absTOnlySubjects",
        "absCOnlySubjects",
        "absBothSubjects"
      ),
      names_to = "subjectsIn",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      subjectsIn = dplyr::recode(
        .data$subjectsIn, # is this correct?
        absTOnlySubjects = "Left cohort only",
        absBothSubjects = "Both cohorts",
        absCOnlySubjects = "Right cohort only"
      )
    )

  plotData$subjectsIn <-
    factor(
      plotData$subjectsIn,
      levels = c("Right cohort only", "Both cohorts", "Left cohort only")
    )

  if (yAxis == "Percentages") {
    position <- "fill"
  } else {
    position <- "stack"
  }

  sortTargetShortName <- plotData %>%
    dplyr::select("targetShortName") %>%
    dplyr::distinct() %>%
    dplyr::arrange(-as.integer(sub(
      pattern = "^C", "", x = .data$targetShortName
    )))

  sortComparatorShortName <- plotData %>%
    dplyr::select("comparatorShortName") %>%
    dplyr::distinct() %>%
    dplyr::arrange(as.integer(sub(
      pattern = "^C", "", x = .data$comparatorShortName
    )))

  plotData <- plotData %>%
    dplyr::arrange(
      targetShortName = factor(.data$targetShortName, levels = sortTargetShortName$targetShortName),
      .data$targetShortName
    ) %>%
    dplyr::arrange(
      comparatorShortName = factor(.data$comparatorShortName, levels = sortComparatorShortName$comparatorShortName),
      .data$comparatorShortName
    )

  plotData$targetShortName <- factor(plotData$targetShortName,
                                     levels = sortTargetShortName$targetShortName
  )

  plotData$comparatorShortName <-
    factor(plotData$comparatorShortName,
           levels = sortComparatorShortName$comparatorShortName
    )

  plot <- ggplot2::ggplot(data = plotData) +
    ggplot2::aes(
      fill = .data$subjectsIn,
      y = .data$targetShortName,
      x = .data$value,
      tooltip = .data$tooltip,
      group = .data$subjectsIn
    ) +
    ggplot2::ylab(label = "") +
    ggplot2::xlab(label = "") +
    ggplot2::scale_fill_manual("Subjects in", values = c(grDevices::rgb(0.8, 0.2, 0.2), grDevices::rgb(0.3, 0.2, 0.4), grDevices::rgb(0.4, 0.4, 0.9))) +
    ggplot2::facet_grid(.data$comparatorShortName ~ .data$databaseName) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "gray"),
      axis.ticks.y = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(2, "lines")
    ) +
    ggiraph::geom_bar_interactive(
      position = position,
      alpha = 0.6,
      stat = "identity"
    )
  if (yAxis == "Percentages") {
    plot <- plot + ggplot2::scale_x_continuous(labels = scales::percent)
  } else {
    plot <-
      plot + ggplot2::scale_x_continuous(labels = scales::comma, n.breaks = 3)
  }
  width <- length(unique(plotData$databaseId))
  height <-
    nrow(
      plotData %>%
        dplyr::select("targetShortName", "comparatorShortName") %>%
        dplyr::distinct()
    )
  plot <- ggiraph::girafe(
    ggobj = plot,
    options = list(ggiraph::opts_sizing(rescale = TRUE)),
    width_svg = max(12, 2 * width),
    height_svg = max(2, 0.5 * height)
  )
  return(plot)
}


#' Cohort Overlap View
#' @description
#' Use for customizing UI
#'
#' @param id    Namespace Id - use namespaced id ns("cohortOverlap") inside diagnosticsExplorer module
#' @export
cohortOverlapView <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::box(
      collapsible = TRUE,
      collapsed = TRUE,
      title = "Cohort Overlap (subjects)",
      width = "100%",
      shiny::htmlTemplate(system.file("cohort-diagnostics-www", "cohortOverlap.html", package = utils::packageName()))
    ),
    shinydashboard::box(
      status = "warning",
      width = "100%",
      shiny::tags$div(
        style = "max-height: 100px; overflow-y: auto",
        shiny::uiOutput(outputId = ns("selectedCohorts"))
      )
    ),
    shinydashboard::box(
      width = NULL,
      status = "primary",

      shiny::tabsetPanel(
        type = "pills",
        shiny::tabPanel(
          title = "Plot",
          shiny::radioButtons(
            inputId = ns("overlapPlotType"),
            label = "",
            choices = c("Percentages", "Counts"),
            selected = "Percentages",
            inline = TRUE
          ),
          shinycssloaders::withSpinner(ggiraph::ggiraphOutput(ns("overlapPlot"), width = "100%", height = "100%"))
        ),

        shiny::tabPanel(
          title = "Table",
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::checkboxInput(
                inputId = ns("showAsPercentage"),
                label = "Show As Percentage",
                value = TRUE
              )
            ),
            shiny::column(
              width = 3,
              shiny::checkboxInput(
                inputId = ns("showCohortIds"),
                label = "Show Cohort Ids",
                value = TRUE
              )
            )
          ),
          shinycssloaders::withSpinner(
            reactable::reactableOutput(ns("overlapTable"))
          )
        )
      )
    )
  )
}

# Returns data from cohort_relationships table of Cohort Diagnostics results data model
getResultsCohortRelationships <- function(dataSource,
                                          cohortIds = NULL,
                                          comparatorCohortIds = NULL,
                                          databaseIds = NULL,
                                          startDays = NULL,
                                          endDays = NULL) {
  data <- dataSource$connectionHandler$queryDb(
    sql = "SELECT cr.*, db.database_name
             FROM @results_database_schema.@table_name cr
             INNER JOIN @results_database_schema.@database_table db ON db.database_id = cr.database_id
             WHERE cr.cohort_id IN (@cohort_id)
             AND cr.database_id IN (@database_id)
              {@comparator_cohort_id != \"\"} ? { AND cr.comparator_cohort_id IN (@comparator_cohort_id)}
              {@start_day != \"\"} ? { AND cr.start_day IN (@start_day)}
              {@end_day != \"\"} ? { AND cr.end_day IN (@end_day)};",
    snakeCaseToCamelCase = TRUE,
    results_database_schema = dataSource$resultsDatabaseSchema,
    database_id = quoteLiterals(databaseIds),
    table_name = dataSource$prefixTable("cohort_relationships"),
    database_table = dataSource$databaseTableName,
    cohort_id = cohortIds,
    comparator_cohort_id = comparatorCohortIds,
    start_day = startDays,
    end_day = endDays
  ) %>%
    dplyr::tibble()

  return(data)
}

# Returns data for use in cohort_overlap
getResultsCohortOverlap <- function(dataSource,
                                    targetCohortIds = NULL,
                                    comparatorCohortIds = NULL,
                                    databaseIds = NULL) {
  cohortIds <- c(targetCohortIds, comparatorCohortIds) %>% unique()
  cohortCounts <-
    getResultsCohortCounts(
      dataSource = dataSource,
      cohortIds = cohortIds,
      databaseIds = databaseIds
    )

  if (!hasData(cohortCounts)) {
    return(NULL)
  }

  cohortRelationship <-
    getResultsCohortRelationships(
      dataSource = dataSource,
      cohortIds = cohortIds,
      comparatorCohortIds = comparatorCohortIds,
      databaseIds = databaseIds,
      startDays = c(-9999, 0),
      endDays = c(9999, 0)
    )

  # Fix relationship data so 0 overlap displays
  allCombinations <- dplyr::tibble(databaseId = databaseIds) %>%
    tidyr::crossing(dplyr::tibble(cohortId = cohortIds)) %>%
    tidyr::crossing(dplyr::tibble(comparatorCohortId = comparatorCohortIds)) %>%
    dplyr::filter(.data$comparatorCohortId != .data$cohortId) %>%
    tidyr::crossing(dplyr::tibble(startDay = c(-9999, 0),
                                  endDay = c(9999, 0)))

  cohortRelationship <- allCombinations %>%
    dplyr::left_join(cohortRelationship,
                     by = c("databaseId", "cohortId", "comparatorCohortId", "startDay", "endDay")) %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::where(is.numeric), ~tidyr::replace_na(., 0)))

  fullOffSet <- cohortRelationship %>%
    dplyr::filter(.data$startDay == -9999) %>%
    dplyr::filter(.data$endDay == 9999) %>%
    dplyr::filter(.data$cohortId %in% c(targetCohortIds)) %>%
    dplyr::filter(.data$comparatorCohortId %in% c(comparatorCohortIds)) %>%
    dplyr::select(
      "databaseId",
      "cohortId",
      "comparatorCohortId",
      "subjects"
    ) %>%
    dplyr::inner_join(
      cohortCounts %>%
        dplyr::select(-"cohortEntries") %>%
        dplyr::rename("targetCohortSubjects" = "cohortSubjects"),
      by = c("databaseId", "cohortId")
    ) %>%
    dplyr::mutate(tOnlySubjects = .data$targetCohortSubjects - .data$subjects) %>%
    dplyr::inner_join(
      cohortCounts %>%
        dplyr::select(-"cohortEntries") %>%
        dplyr::rename(
          "comparatorCohortSubjects" = "cohortSubjects",
          "comparatorCohortId" = "cohortId"
        ),
      by = c("databaseId", "comparatorCohortId")
    ) %>%
    dplyr::mutate(cOnlySubjects = .data$comparatorCohortSubjects - .data$subjects) %>%
    dplyr::mutate(eitherSubjects = .data$cOnlySubjects + .data$tOnlySubjects + .data$subjects) %>%
    dplyr::rename(
      "targetCohortId" = "cohortId",
      "bothSubjects" = "subjects"
    ) %>%
    dplyr::select(
      "databaseId",
      "targetCohortId",
      "comparatorCohortId",
      "bothSubjects",
      "tOnlySubjects",
      "cOnlySubjects",
      "eitherSubjects"
    )


  noOffset <- cohortRelationship %>%
    dplyr::filter(.data$comparatorCohortId %in% comparatorCohortIds) %>%
    dplyr::filter(.data$cohortId %in% targetCohortIds) %>%
    dplyr::filter(.data$startDay == 0) %>%
    dplyr::filter(.data$endDay == 0) %>%
    dplyr::select(
      "databaseId",
      "cohortId",
      "comparatorCohortId",
      "subCsBeforeTs",
      "subCWithinT",
      "subCsAfterTs",
      "subCsAfterTe",
      "subCsBeforeTs",
      "subCsBeforeTe",
      "subCsOnTs",
      "subCsOnTe"
    ) %>%
    dplyr::rename(
      "cBeforeTSubjects" = "subCsBeforeTs",
      "targetCohortId" = "cohortId",
      "cInTSubjects" = "subCWithinT",
      "cStartAfterTStart" = "subCsAfterTs",
      "cStartAfterTEnd" = "subCsAfterTe",
      "cStartBeforeTStart" = "subCsBeforeTs",
      "cStartBeforeTEnd" = "subCsBeforeTe",
      "cStartOnTStart" = "subCsOnTs",
      "cStartOnTEnd" = "subCsOnTe"
    )

  result <- fullOffSet %>%
    dplyr::left_join(noOffset,
                     by = c("databaseId", "targetCohortId", "comparatorCohortId")
    ) %>%
    dplyr::filter(.data$targetCohortId != .data$comparatorCohortId) %>%
    dplyr::select(
      "databaseId",
      # cohortId,
      "comparatorCohortId",
      "eitherSubjects",
      "tOnlySubjects",
      "cOnlySubjects",
      "bothSubjects",
      # cBeforeTSubjects,
      "targetCohortId",
      "cInTSubjects",
      "cStartAfterTStart",
      "cStartAfterTEnd",
      "cStartBeforeTStart",
      "cStartBeforeTEnd",
      "cStartOnTStart",
      "cStartOnTEnd",
    )

  databaseNames <- cohortCounts %>% dplyr::distinct(.data$databaseId, .data$databaseName)
  result <- result %>% dplyr::inner_join(databaseNames, by = "databaseId")

  return(result)
}


cohortOverlapModule <- function(id,
                                dataSource,
                                selectedCohorts,
                                selectedDatabaseIds,
                                targetCohortId,
                                cohortIds,
                                cohortTable) {
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    output$selectedCohorts <- shiny::renderUI({ selectedCohorts() })

    # Cohort Overlap ------------------------
    cohortOverlapData <- shiny::reactive({
      shiny::validate(shiny::need(length(selectedDatabaseIds()) > 0, "No data sources chosen"))
      shiny::validate(shiny::need(length(cohortIds()) > 1, "Please select at least two cohorts."))
      combisOfTargetComparator <- t(utils::combn(cohortIds(), 2)) %>%
        as.data.frame() %>%
        dplyr::tibble()
      colnames(combisOfTargetComparator) <- c("targetCohortId", "comparatorCohortId")


      data <- getResultsCohortOverlap(
        dataSource = dataSource,
        targetCohortIds = combisOfTargetComparator$targetCohortId,
        comparatorCohortIds = combisOfTargetComparator$comparatorCohortId,
        databaseIds = selectedDatabaseIds()
      )
      shiny::validate(shiny::need(
        !is.null(data),
        paste0("No cohort overlap data for this combination")
      ))
      shiny::validate(shiny::need(
        nrow(data) > 0,
        paste0("No cohort overlap data for this combination.")
      ))
      return(data)
    })

    output$overlapPlot <- ggiraph::renderggiraph(expr = {
      shiny::validate(shiny::need(
        length(cohortIds()) > 0,
        paste0("Please select Target Cohort(s)")
      ))

      data <- cohortOverlapData()
      shiny::validate(shiny::need(
        !is.null(data),
        paste0("No cohort overlap data for this combination")
      ))
      shiny::validate(shiny::need(
        nrow(data) > 0,
        paste0("No cohort overlap data for this combination.")
      ))

      shiny::validate(shiny::need(
        !all(is.na(data$eitherSubjects)),
        paste0("No cohort overlap data for this combination.")
      ))

      plot <- plotCohortOverlap(
        data = data,
        shortNameRef = cohortTable,
        yAxis = input$overlapPlotType
      )
      return(plot)
    })


    output$overlapTable <- reactable::renderReactable({
      data <- cohortOverlapData()
      shiny::validate(shiny::need(
        !is.null(data),
        paste0("No cohort overlap data for this combination")
      ))

      data <- data %>%
        dplyr::inner_join(cohortTable %>% dplyr::select("cohortId",
                                                        "targetCohortName" = "cohortName"),
                          by = c("targetCohortId" = "cohortId")) %>%
        dplyr::inner_join(cohortTable %>% dplyr::select("cohortId",
                                                        "comparatorCohortName" = "cohortName"),
                          by = c("comparatorCohortId" = "cohortId")) %>%
        dplyr::select(
          "databaseName",
          "targetCohortId",
          "targetCohortName",
          "comparatorCohortId",
          "comparatorCohortName",
          "tOnly" = "tOnlySubjects",
          "cOnly" = "cOnlySubjects",
          "both" = "bothSubjects",
          "totalSubjects" = "eitherSubjects"
        )

      if (input$showCohortIds) {
        data <- data %>% dplyr::mutate(
          targetCohortName = paste0("C", .data$targetCohortId, " - ", .data$targetCohortName),
          comparatorCohortName = paste0("C", .data$comparatorCohortId, " - ", .data$comparatorCohortName)
        )
      }

      data <- data %>% dplyr::select(-"targetCohortId", -"comparatorCohortId")

      if (input$showAsPercentage) {
        data$tOnly <- data$tOnly / data$totalSubjects
        data$cOnly <- data$cOnly / data$totalSubjects
        data$both <- data$both / data$totalSubjects
      }

      styleFunc <- function(value) {
        color <- '#fff'
        if (input$showAsPercentage) {
          if (is.numeric(value)) {
            value <- ifelse(is.na(value), 0, value)
            color <- pallete(value)
          }
        }
        list(background = color)
      }

      valueColDef <- reactable::colDef(
        cell = formatDataCellValueInDisplayTable(input$showAsPercentage),
        style = styleFunc,
        width = 80
      )
      colnames(data) <- SqlRender::camelCaseToTitleCase(colnames(data))
      reactable::reactable(
        data = data,
        columns = list(
          "T Only" = valueColDef,
          "C Only" = valueColDef,
          "Both" = valueColDef,
          "Target Cohort Name" = reactable::colDef(minWidth = 300),
          "Comparator Cohort Name" = reactable::colDef(minWidth = 300),
          "Total Subjects" = reactable::colDef(cell = formatDataCellValueInDisplayTable(FALSE))
        ),
        sortable = TRUE,
        groupBy = c("Target Cohort Name", "Comparator Cohort Name"),
        resizable = TRUE,
        filterable = TRUE,
        searchable = TRUE,
        pagination = TRUE,
        showPagination = TRUE,
        showPageInfo = TRUE,
        highlight = TRUE,
        striped = TRUE,
        compact = TRUE,
        wrap = TRUE,
        showSortIcon = TRUE,
        showSortable = TRUE,
        fullWidth = TRUE,
        bordered = TRUE,
        onClick = "select",
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 20, 50, 100, 1000),
        defaultPageSize = 20,
        theme = reactable::reactableTheme(
          rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
        )
      )
    })
  })
}
