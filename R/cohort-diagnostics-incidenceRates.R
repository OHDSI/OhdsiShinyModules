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

getIncidenceRateResult <- function(dataSource,
                                   cohortIds,
                                   databaseIds,
                                   stratifyByGender = c(TRUE, FALSE),
                                   stratifyByAgeGroup = c(TRUE, FALSE),
                                   stratifyByCalendarYear = c(TRUE, FALSE),
                                   minPersonYears = 1000,
                                   minSubjectCount = NA) {
  # Perform error checks for input variables
  errorMessage <- checkmate::makeAssertCollection()
  errorMessage <-
    checkErrorCohortIdsDatabaseIds(
      cohortIds = cohortIds,
      databaseIds = databaseIds,
      errorMessage = errorMessage
    )
  checkmate::assertLogical(
    x = stratifyByGender,
    add = errorMessage,
    min.len = 1,
    max.len = 2,
    unique = TRUE
  )
  checkmate::assertLogical(
    x = stratifyByAgeGroup,
    add = errorMessage,
    min.len = 1,
    max.len = 2,
    unique = TRUE
  )
  checkmate::assertLogical(
    x = stratifyByCalendarYear,
    add = errorMessage,
    min.len = 1,
    max.len = 2,
    unique = TRUE
  )
  checkmate::reportAssertions(collection = errorMessage)

  sql <- "SELECT ir.*, dt.database_name, cc.cohort_subjects
            FROM  @results_database_schema.@ir_table ir
            INNER JOIN @results_database_schema.@database_table dt ON ir.database_id = dt.database_id
            INNER JOIN @results_database_schema.@cc_table cc ON (
              ir.database_id = cc.database_id AND ir.cohort_id = cc.cohort_id
            )
            WHERE ir.cohort_id in (@cohort_ids)
           	  AND ir.database_id in (@database_ids)
            {@gender == TRUE} ? {AND ir.gender != ''} : {  AND ir.gender = ''}
            {@age_group == TRUE} ? {AND ir.age_group != ''} : {  AND ir.age_group = ''}
            {@calendar_year == TRUE} ? {AND ir.calendar_year != ''} : {  AND ir.calendar_year = ''}
              AND ir.person_years > @personYears;"
  data <-
    dataSource$connectionHandler$queryDb(
      sql = sql,
      results_database_schema = dataSource$resultsDatabaseSchema,
      cohort_ids = cohortIds,
      database_ids = quoteLiterals(databaseIds),
      gender = stratifyByGender,
      age_group = stratifyByAgeGroup,
      calendar_year = stratifyByCalendarYear,
      personYears = minPersonYears,
      ir_table = dataSource$prefixTable("incidence_rate"),
      cc_table = dataSource$prefixTable("cohort_count"),
      database_table = dataSource$databaseTableName,
      snakeCaseToCamelCase = TRUE
    ) %>%
      tidyr::tibble()

  data <- data %>%
    dplyr::mutate(
      gender = dplyr::na_if(.data$gender, ""),
      ageGroup = dplyr::na_if(.data$ageGroup, ""),
      calendarYear = dplyr::na_if(.data$calendarYear, "")
    ) %>%
    dplyr::mutate(calendarYear = as.integer(.data$calendarYear)) %>%
    dplyr::arrange(.data$cohortId, .data$databaseId)


  if (!is.na(minSubjectCount)) {
    data <- data %>%
      dplyr::filter(.data$cohortSubjects > !!minSubjectCount)
  }

  return(data)
}

plotIncidenceRate <- function(data,
                              cohortTable = NULL,
                              stratifyByAgeGroup = TRUE,
                              stratifyByGender = TRUE,
                              stratifyByCalendarYear = TRUE,
                              yscaleFixed = FALSE) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTibble(
    x = data,
    any.missing = TRUE,
    min.rows = 1,
    min.cols = 5,
    null.ok = FALSE,
    add = errorMessage
  )
  checkmate::assertLogical(
    x = stratifyByAgeGroup,
    any.missing = FALSE,
    min.len = 1,
    max.len = 1,
    null.ok = FALSE,
    add = errorMessage
  )
  checkmate::assertLogical(
    x = stratifyByGender,
    any.missing = FALSE,
    min.len = 1,
    max.len = 1,
    null.ok = FALSE,
    add = errorMessage
  )
  checkmate::assertLogical(
    x = stratifyByCalendarYear,
    any.missing = FALSE,
    min.len = 1,
    max.len = 1,
    null.ok = FALSE,
    add = errorMessage
  )
  checkmate::assertLogical(
    x = yscaleFixed,
    any.missing = FALSE,
    min.len = 1,
    max.len = 1,
    null.ok = FALSE,
    add = errorMessage
  )
  checkmate::assertDouble(
    x = data$incidenceRate,
    lower = 0,
    any.missing = FALSE,
    null.ok = FALSE,
    min.len = 1,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)
  checkmate::assertDouble(
    x = data$incidenceRate,
    lower = 0,
    any.missing = FALSE,
    null.ok = FALSE,
    min.len = 1,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  cohortNames <- cohortTable %>% dplyr::select("cohortId",
                                               "cohortName")
  plotData <- data %>%
    dplyr::inner_join(cohortNames, by = "cohortId",) %>%
    addShortName(cohortTable) %>%
    dplyr::mutate(incidenceRate = round(.data$incidenceRate, digits = 3))

  plotData <- plotData %>%
    dplyr::mutate(
      strataGender = !is.na(.data$gender),
      strataAgeGroup = !is.na(.data$ageGroup),
      strataCalendarYear = !is.na(.data$calendarYear)
    ) %>%
    dplyr::filter(
      .data$strataGender %in% !!stratifyByGender &
        .data$strataAgeGroup %in% !!stratifyByAgeGroup &
        .data$strataCalendarYear %in% !!stratifyByCalendarYear
    ) %>%
    dplyr::select(-dplyr::starts_with("strata"))

  aesthetics <- list(y = "incidenceRate")
  if (stratifyByCalendarYear) {
    aesthetics$x <- "calendarYear"
    xLabel <- "Calender year"
    showX <- TRUE
    if (stratifyByGender) {
      aesthetics$group <- "gender"
      aesthetics$color <- "gender"
    }
    plotType <- "line"
  } else {
    xLabel <- ""
    if (stratifyByGender) {
      aesthetics$x <- "gender"
      aesthetics$color <- "gender"
      aesthetics$fill <- "gender"
      showX <- TRUE
    } else if (stratifyByAgeGroup) {
      aesthetics$x <- "ageGroup"
      showX <- TRUE
    } else {
      aesthetics$x <- 1
      showX <- FALSE
    }
    plotType <- "bar"
  }


  sortShortName <- plotData %>%
    dplyr::select("shortName") %>%
    dplyr::distinct() %>%
    dplyr::arrange(as.integer(sub(
      pattern = "^C", "", x = .data$shortName
    )))

  plotData <- plotData %>%
    dplyr::arrange(
      shortName = factor(.data$shortName, levels = sortShortName$shortName),
      .data$shortName
    )

  plotData$shortName <- factor(plotData$shortName,
                               levels = sortShortName$shortName)

  if (stratifyByAgeGroup) {
    sortAgeGroup <- plotData %>%
      dplyr::select("ageGroup") %>%
      dplyr::distinct() %>%
      dplyr::arrange(as.integer(sub(
        pattern = "-.+$", "", x = .data$ageGroup
      )))

    plotData <- plotData %>%
      dplyr::arrange(
        ageGroup = factor(.data$ageGroup, levels = sortAgeGroup$ageGroup),
        .data$ageGroup
      )

    plotData$ageGroup <- factor(plotData$ageGroup,
                                levels = sortAgeGroup$ageGroup
    )
  }

  plotData$tooltip <- c(
    paste0(
      plotData$cohortName,
      "\n",
      plotData$databaseName,
      "\nIncidence Rate = ",
      scales::comma(plotData$incidenceRate, accuracy = 0.01),
      "/per 1k PY",
      "\nIncidence Proportion = ",
      scales::percent(plotData$cohortCount / plotData$cohortSubjects, accuracy = 0.1),
      "\nPerson years = ",
      scales::comma(plotData$personYears, accuracy = 0.01),
      "\nCohort count = ",
      scales::comma(plotData$cohortSubjects, accuracy = 1),
      "\nCount = ",
      paste0(scales::comma(plotData$cohortCount, accuracy = 1))
    )
  )

  if (stratifyByAgeGroup) {
    plotData$tooltip <-
      c(paste0(plotData$tooltip, "\nAge Group = ", plotData$ageGroup))
  }

  if (stratifyByGender) {
    plotData$tooltip <-
      c(paste0(plotData$tooltip, "\nSex = ", plotData$gender))
  }

  if (stratifyByCalendarYear) {
    plotData$tooltip <-
      c(paste0(plotData$tooltip, "\nYear = ", plotData$calendarYear))
  }

  if (stratifyByGender) {
    # Make sure colors are consistent, no matter which genders are included:
    genders <- c("Female", "Male", "No matching concept")
    # Code used to generate palette:
    colors <- RColorBrewer::brewer.pal(n = 3, name = "Dark2")
    plotData$gender <- factor(plotData$gender, levels = genders)
  } else {
    colors <- "#000000"
    plotData$gender <- "NA"
  }

  ncohorts <- plotData$shortName %>% unique() %>% length()
  ndatabases <- plotData$databaseName %>% unique() %>% length()
  nrows <- ncohorts * ndatabases

  subplots <- list()
  topAnnotations <- list()
  annotations <- list()
  ageGroupings <- plotData$ageGroup %>% unique()
  cohortIds <- plotData$cohortId %>% unique()


  makeSubPlot <- function(subsetData, colors, title = "", ytitle = "") {
    if (stratifyByCalendarYear) {
      plt <- subsetData %>%
      plotly::plot_ly() %>%
        plotly::add_lines(x = ~calendarYear,
                          color = ~gender,
                          colors = colors,
                          y0 = 0,
                          y = ~incidenceRate) %>%
        plotly::add_markers(x = ~calendarYear,
                            color = ~gender,
                            colors = colors,
                            text = ~tooltip,
                            y0 = 0,
                            y = ~incidenceRate)
    } else {
     plt <- subsetData %>%
      plotly::plot_ly() %>%
        plotly::add_bars(x = ~gender,
                         color = ~gender,
                         colors = colors,
                         text = ~tooltip,
                         y0 = 0,
                         y = ~incidenceRate)
    }
    plt <- plt %>%
      plotly::layout(title = title,
                     xaxis = list(zerolinecolor = '#ffff',
                                  zerolinewidth = 1,
                                  textangle = 70,
                                  gridcolor = 'ffff'),
                     yaxis = list(title = ytitle))

    return(plt)
  }

  databaseNames <- plotData$databaseName %>% unique()
  for (dbI in 1:length(databaseNames)) {
    dbm <- databaseNames[dbI]
    subdata <- plotData %>% dplyr::filter(.data$databaseName == dbm)
    for (cohort in cohortIds) {
      csubdata <- subdata %>% dplyr::filter(.data$cohortId == cohort)

      if (stratifyByAgeGroup) {
        for (i in 1:length(ageGroupings)) {
          agrp <- ageGroupings[i]
          cohortName <- ""
          if (i == 1) {
            cohortName <- paste0("C", cohort)
          }
          pltdt <- csubdata %>% dplyr::filter(.data$ageGroup == agrp)

          subplots[[length(subplots) + 1]] <- makeSubPlot(pltdt, colors, ytitle = cohortName)

          xTitlePos <- (length(topAnnotations) / length(ageGroupings)) + 1 / length(ageGroupings) * 0.2
          topAnnotations[[length(topAnnotations) + 1]] <- list(text = paste("Age", agrp),
                                                               x = xTitlePos,
                                                               y = 1,
                                                               xref = "paper",
                                                               yref = "paper",
                                                               xanchor = "left",
                                                               yanchor = "bottom",
                                                               showarrow = FALSE)
        }
      } else {
        subplots[[length(subplots) + 1]] <- makeSubPlot(csubdata, colors, ytitle = paste0("C", cohort))
      }
    }
  }

  for (i in 1:length(databaseNames)) {
    dbName <- rev(databaseNames)[i]
    ypos <- i * 1/length(databaseNames) - 0.05
    topAnnotations[[length(topAnnotations) + 1]] <- list(
      text = dbName,
      x = 0.998,
      showarrow = FALSE,
      y = ypos,
      textangle = 90,
      xref = "paper",
      yref = "paper",
      xanchor = "left",
      yanchor = "top"
    )
  }

  plt <- plotly::subplot(subplots, nrows = nrows, shareX = F, shareY = T, margin = c(0.02, 0.02, 0.02, 0.05)) %>%
    plotly::layout(annotations = c(annotations, topAnnotations),
                   showlegend = F,
                   plot_bgcolor = '#e5ecf6',
                   xaxis = list(
                     showTitle = FALSE,
                     zerolinecolor = '#ffff',
                     zerolinewidth = 1,
                     textangle = 70,
                     gridcolor = 'ffff'),
                   yaxis = list(
                     showTitle = FALSE,
                     zerolinecolor = '#ffff',
                     zerolinewidth = 1,
                     gridcolor = 'ffff'))

  plt
}

#' incidence Rates View
#' @description
#' Use for customizing UI
#'
#' @param id    Namespace Id - use namespaced id ns("incidenceRates") inside diagnosticsExplorer module
#' @export
incidenceRatesView <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::box(
      collapsible = TRUE,
      collapsed = TRUE,
      title = "Incidence Rates",
      width = "100%",
      shiny::htmlTemplate(system.file("cohort-diagnostics-www", "incidenceRate.html", package = utils::packageName()))
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

      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::checkboxGroupInput(
            inputId = ns("irStratification"),
            label = "Stratify by",
            choices = c("Age", "Sex", "Calendar Year"),
            selected = c("Age", "Sex", "Calendar Year"),
            inline = TRUE
          )
        ),
        shiny::column(
          width = 3,
          shiny::tags$br(),
          shiny::checkboxInput(
            inputId = ns("irYscaleFixed"),
            label = "Use same y-scale across databases"),
        ),
        shiny::column(
          width = 5,
          shiny::conditionalPanel(
            condition = "input.irYscaleFixed",
            ns = ns,
            shiny::sliderInput(
              inputId = ns("YscaleMinAndMax"),
              label = "Limit y-scale range to:",
              min = c(0),
              max = c(0),
              value = c(0, 0),
              dragRange = TRUE, width = 400,
              step = 1,
              sep = "",
            )
          )
        )
      ),
      shiny::fluidRow(
        shiny::conditionalPanel(
          condition = "input.irStratification.indexOf('Age') > -1",
          ns = ns,
          shiny::column(
            width = 6,
            shinyWidgets::pickerInput(
              inputId = ns("incidenceRateAgeFilter"),
              label = "Filter By Age",
              choices = c("All"),
              selected = c("All"),
              multiple = TRUE,
              choicesOpt = list(style = rep_len("color: black;", 999)),
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                dropupAuto = TRUE,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          )
        ),
        shiny::conditionalPanel(
          condition = "input.irStratification.indexOf('Sex') > -1",
          ns = ns,
          shiny::column(
            width = 6,
            shinyWidgets::pickerInput(
              inputId = ns("incidenceRateGenderFilter"),
              label = "Filter By Sex",
              choices = c("All"),
              selected = c("All"),
              multiple = TRUE,
              choicesOpt = list(style = rep_len("color: black;", 999)),
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                dropupAuto = TRUE,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          )
        )
      ),

      shiny::fluidRow(
        shiny::column(
          width = 3,
          shiny::numericInput(
            inputId = ns("minPersonYear"),
            label = "Minimum person years",
            value = 1000,
            min = 0
          )
        ),
        shiny::column(
          width = 3,
          shiny::numericInput(
            inputId = ns("minSubjectCount"),
            label = "Minimum subject count",
            value = NULL
          )
        ),
        shiny::column(
          width = 6,
          shiny::conditionalPanel(
            condition = "input.irStratification.indexOf('Calendar Year') > -1",
            ns = ns,
            shiny::sliderInput(
              inputId = ns("incidenceRateCalenderFilter"),
              label = "Filter By Calender Year",
              min = c(0),
              max = c(0),
              value = c(0, 0),
              dragRange = TRUE,
              pre = "Year ",
              step = 1,
              sep = ""
            )
          )
        )
      ),
      shiny::actionButton(inputId = ns("generatePlot"), label = "Generate Plots"),

    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.generatePlot > 0",
      shinydashboard::box(
        width = NULL,
        shiny::htmlOutput(outputId = ns("hoverInfoIr")),
        shinycssloaders::withSpinner(
          shiny::div(
            id = ns("irPlotContainer"),
            plotly::plotlyOutput(
              outputId = ns("incidenceRatePlot"),
              width = "100%",
              height = "400px"
            )
          )
        )
      )
    ),
    # complicated way of setting plot height based on number of rows and selection type
    # Note that this code is only used because renderUI/ uiOutput didn't seem to update with plotly
    shiny::tags$script(sprintf("
      Shiny.addCustomMessageHandler('%s', function(height) {
        let plotSpace = document.getElementById('%s');
        plotSpace.querySelector('.svg-container').style.height = height;
        plotSpace.querySelector('.js-plotly-plot').style.height = height;
      });
    ", ns("irPlotHeight"), ns("irPlotContainer")))
  )
}


# Global ranges for IR values
getIncidenceRateRanges <- function(dataSource, minPersonYears = 0) {
  sql <- "SELECT DISTINCT age_group FROM @results_database_schema.@ir_table WHERE person_years >= @person_years"

  ageGroups <- dataSource$connectionHandler$queryDb(
    sql = sql,
    results_database_schema = dataSource$resultsDatabaseSchema,
    ir_table = dataSource$prefixTable("incidence_rate"),
    person_years = minPersonYears,
    snakeCaseToCamelCase = TRUE
  ) %>%
    dplyr::mutate(ageGroup = dplyr::na_if(.data$ageGroup, ""))

  sql <- "SELECT DISTINCT calendar_year FROM @results_database_schema.@ir_table WHERE person_years >= @person_years"

  calendarYear <- dataSource$connectionHandler$queryDb(
    sql = sql,
    results_database_schema = dataSource$resultsDatabaseSchema,
    ir_table = dataSource$prefixTable("incidence_rate"),
    person_years = minPersonYears,
    snakeCaseToCamelCase = TRUE
  ) %>%
    dplyr::mutate(
      calendarYear = dplyr::na_if(.data$calendarYear, "")
    ) %>%
    dplyr::mutate(calendarYear = as.integer(.data$calendarYear))

  sql <- "SELECT DISTINCT gender FROM @results_database_schema.@ir_table WHERE person_years >= @person_years"

  gender <- dataSource$connectionHandler$queryDb(
    sql = sql,
    results_database_schema = dataSource$resultsDatabaseSchema,
    ir_table = dataSource$prefixTable("incidence_rate"),
    person_years = minPersonYears,
    snakeCaseToCamelCase = TRUE
  ) %>%
    dplyr::mutate(gender = dplyr::na_if(.data$gender, ""))


  sql <- "SELECT
    min(incidence_rate) as min_ir,
    max(incidence_rate) as max_ir
   FROM @results_database_schema.@ir_table
   WHERE person_years >= @person_years
   AND incidence_rate > 0.0
   "

  incidenceRate <- dataSource$connectionHandler$queryDb(
    sql = sql,
    results_database_schema = dataSource$resultsDatabaseSchema,
    ir_table = dataSource$prefixTable("incidence_rate"),
    person_years = minPersonYears,
    snakeCaseToCamelCase = TRUE
  )

  return(list(gender = gender,
              incidenceRate = incidenceRate,
              calendarYear = calendarYear,
              ageGroups = ageGroups))
}


incidenceRatesModule <- function(id,
                                 dataSource,
                                 selectedCohorts,
                                 selectedDatabaseIds,
                                 cohortIds,
                                 cohortTable) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    irRanges <- getIncidenceRateRanges(dataSource)
    output$selectedCohorts <- shiny::renderUI({ selectedCohorts() })

    # Incidence rate ---------------------------

    incidenceRateData <- shiny::reactive({
      shiny::validate(shiny::need(length(selectedDatabaseIds()) > 0, "No data sources chosen"))
      shiny::validate(shiny::need(length(cohortIds()) > 0, "No cohorts chosen"))
      stratifyByAge <- "Age" %in% input$irStratification
      stratifyByGender <- "Sex" %in% input$irStratification
      stratifyByCalendarYear <-
        "Calendar Year" %in% input$irStratification
      if (length(cohortIds()) > 0) {
        data <- getIncidenceRateResult(
          dataSource = dataSource,
          cohortIds = cohortIds(),
          databaseIds = selectedDatabaseIds(),
          stratifyByGender = stratifyByGender,
          stratifyByAgeGroup = stratifyByAge,
          stratifyByCalendarYear = stratifyByCalendarYear,
          minPersonYears = input$minPersonYear,
          minSubjectCount = input$minSubjectCount
        ) %>%
          dplyr::mutate(incidenceRate = dplyr::case_when(
            .data$incidenceRate < 0 ~ 0,
            TRUE ~ .data$incidenceRate
          ))
      } else {
        data <- NULL
      }
      return(data)
    })

    shiny::observe({
      ageFilter <- irRanges$ageGroups %>%
        dplyr::filter(.data$ageGroup != " ", .data$ageGroup != "NA", !is.na(.data$ageGroup)) %>%
        dplyr::distinct() %>%
        dplyr::arrange(as.integer(sub(
          pattern = "-.+$", "", x = .data$ageGroup
        )))

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "incidenceRateAgeFilter",
        selected = ageFilter$ageGroup,
        choices = ageFilter$ageGroup,
        choicesOpt = list(style = rep_len("color: black;", 999))
      )

    })

    shiny::observe({
      genderFilter <- irRanges$gender %>%
        dplyr::select("gender") %>%
        dplyr::filter(
          .data$gender != "NA",
          .data$gender != " ",
          !is.na(.data$gender),
          !is.null(.data$gender)
        ) %>%
        dplyr::distinct() %>%
        dplyr::arrange(.data$gender)

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "incidenceRateGenderFilter",
        choicesOpt = list(style = rep_len("color: black;", 999)),
        choices = genderFilter$gender,
        selected = genderFilter$gender
      )

    })

    shiny::observe({
      calenderFilter <- irRanges$calendarYear %>%
        dplyr::select("calendarYear") %>%
        dplyr::filter(
          .data$calendarYear != " ",
          .data$calendarYear != "NA",
          !is.na(.data$calendarYear)
        ) %>%
        dplyr::distinct(.data$calendarYear) %>%
        dplyr::arrange(.data$calendarYear)

      if (nrow(calenderFilter) > 0) {
        minValue <- min(calenderFilter$calendarYear)
        maxValue <- max(calenderFilter$calendarYear)
      } else {
        minValue <- 2010
        maxValue <- 2030
      }

      shiny::updateSliderInput(
        session = session,
        inputId = "incidenceRateCalenderFilter",
        min = minValue,
        max = maxValue,
        value = c(2010, maxValue)
      )
    })

    shiny::observe({
      minIncidenceRateValue <- round(min(irRanges$incidenceRate$minIr), digits = 2)
      maxIncidenceRateValue <- round(max(irRanges$incidenceRate$maxIr), digits = 2)
      shiny::updateSliderInput(
        session = session,
        inputId = "YscaleMinAndMax",
        min = 0,
        max = maxIncidenceRateValue,
        value = c(minIncidenceRateValue, maxIncidenceRateValue),
        step = round((maxIncidenceRateValue - minIncidenceRateValue) / 5, digits = 2)
      )
    })

    incidenceRateCalenderFilter <- shiny::reactive({
      calenderFilter <- incidenceRateData() %>%
        dplyr::select("calendarYear") %>%
        dplyr::filter(
          .data$calendarYear != "NA",
          !is.na(.data$calendarYear)
        ) %>%
        dplyr::distinct(.data$calendarYear) %>%
        dplyr::arrange(.data$calendarYear)

      calenderFilter <-
        calenderFilter[calenderFilter$calendarYear >= input$incidenceRateCalenderFilter[1] &
                         calenderFilter$calendarYear <= input$incidenceRateCalenderFilter[2], , drop = FALSE] %>%
          dplyr::pull("calendarYear")
      return(calenderFilter)
    })


    incidenceRateYScaleFilter <- shiny::reactive({
      incidenceRateFilter <- incidenceRateData() %>%
        dplyr::select("incidenceRate") %>%
        dplyr::filter(
          .data$incidenceRate != "NA",
          !is.na(.data$incidenceRate)
        ) %>%
        dplyr::distinct(.data$incidenceRate) %>%
        dplyr::arrange(.data$incidenceRate)
      incidenceRateFilter <-
        incidenceRateFilter[incidenceRateFilter$incidenceRate >= input$YscaleMinAndMax[1] &
                              incidenceRateFilter$incidenceRate <= input$YscaleMinAndMax[2], , drop = FALSE] %>%
          dplyr::pull("incidenceRate")
      return(incidenceRateFilter)
    })

    nplots <- shiny::reactive({
      nPlotsMade <- length(selectedDatabaseIds()) * length(cohortIds())
      if ("Age" %in% input$irStratification) {
        nPlotsMade <- nPlotsMade * length(incidenceRateCalenderFilter())
      }

      return(nPlotsMade)
    })

    shiny::observeEvent(input$generatePlot, {
      # Note that this code is only used because renderUI/ uiOutput didn't seem to update with plotly
      plotHeight <- 400
      if (nplots() < 101) {
        # Set the height/width of the plot relative to the number of cohorts and databases
        if ("Age" %in% input$irStratification) {
          plotHeight <- 150 * length(selectedDatabaseIds()) * length(cohortIds())
        } else {
          plotHeight <- 200 * length(selectedDatabaseIds()) * length(cohortIds())
        }
      }
      session$sendCustomMessage(ns("irPlotHeight"), sprintf("%spx", plotHeight))
    })

    getIrPlot <- shiny::eventReactive(input$generatePlot, {
      shiny::validate(shiny::need(length(selectedDatabaseIds()) > 0, "No data sources chosen"))
      shiny::validate(shiny::need(length(cohortIds()) > 0, "No cohorts chosen"))
      nPlotsMade <- nplots()

      stratifyByAge <- "Age" %in% input$irStratification
      stratifyByGender <- "Sex" %in% input$irStratification
      stratifyByCalendarYear <-
        "Calendar Year" %in% input$irStratification

      shiny::validate(shiny::need(nPlotsMade < 101, "Resulting number of plots will execeed 100 - adjust selection"))

      shiny::withProgress(
        message = paste(
          "Building incidence rate plot data for ",
          length(cohortIds()),
          " cohorts and ",
          length(selectedDatabaseIds()),
          " databases"
        ),
      {
        data <- incidenceRateData()

        shiny::validate(shiny::need(all(!is.null(data), nrow(data) > 0), paste0("No data for this combination")))

        if (stratifyByAge && !"All" %in% input$incidenceRateAgeFilter) {
          data <- data %>%
            dplyr::filter(.data$ageGroup %in% input$incidenceRateAgeFilter)
        }
        if (stratifyByGender &&
          !"All" %in% input$incidenceRateGenderFilter) {
          data <- data %>%
            dplyr::filter(.data$gender %in% input$incidenceRateGenderFilter)
        }
        if (stratifyByCalendarYear) {
          data <- data %>%
            dplyr::filter(.data$calendarYear %in% incidenceRateCalenderFilter())
        }
        if (input$irYscaleFixed) {
          data <- data %>%
            dplyr::filter(.data$incidenceRate %in% incidenceRateYScaleFilter())
        }
        if (all(!is.null(data), nrow(data) > 0)) {
          plot <- plotIncidenceRate(
            data = data,
            cohortTable = cohortTable,
            stratifyByAgeGroup = stratifyByAge,
            stratifyByGender = stratifyByGender,
            stratifyByCalendarYear = stratifyByCalendarYear,
            yscaleFixed = input$irYscaleFixed
          )
          return(plot)
        }
      },
        detail = "Please Wait"
      )

    })

    output$incidenceRatePlot <- plotly::renderPlotly(expr = {
      getIrPlot()
    })
  })
}
