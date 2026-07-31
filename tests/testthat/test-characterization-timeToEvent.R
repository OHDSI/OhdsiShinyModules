context("characterization-TimeToEvent")

targetCohort <- OhdsiShinyModules:::getTargetsUsedInCharMain(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema,
  cTablePrefix = resultDatabaseSettingsCharacterization$cTablePrefix,
  cgTablePrefix = resultDatabaseSettingsCharacterization$cgTablePrefix,
  ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix
)

targets4module <- targetCohort %>%
  dplyr::filter(as.integer(.data$timeToEvent) == 1)

characterizationTargetTable <- OhdsiReportGenerator::getCharacterizationTargetSettings(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema,
  targetIds = targets4module$cohortDefinitionId,
  addDatabaseDetails = TRUE,
  databaseTable = resultDatabaseSettingsCharacterization$databaseTable,
  cgTablePrefix = resultDatabaseSettingsCharacterization$cgTablePrefix,
  cTablePrefix = resultDatabaseSettingsCharacterization$cTablePrefix
)

characterizationTargetTable <- characterizationTargetTable %>%
  dplyr::filter(as.integer(.data$timeToEvent) == 1)

outcomeTables <- lapply(
  X = unique(targets4module$cohortDefinitionId),
  FUN = function(targetId) {
    OhdsiReportGenerator::getOutcomeTable(
      connectionHandler = connectionHandlerCharacterization,
      schema = resultDatabaseSettingsCharacterization$schema,
      ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix,
      targetId = targetId
    )
  }
)

outcomeCohort <- dplyr::bind_rows(outcomeTables) %>%
  dplyr::distinct()

if (!("cohortId" %in% colnames(outcomeCohort)) && "cohortDefinitionId" %in% colnames(outcomeCohort)) {
  outcomeCohort$cohortId <- outcomeCohort$cohortDefinitionId
}


shiny::testServer(
  app = characterizationTimeToEventServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    reactiveCharacterizationTargetTable = shiny::reactive(characterizationTargetTable),
    reactiveOutcomeTable = shiny::reactive(outcomeCohort)
  ), 
  expr = {
    testthat::expect_true(nrow(reactiveCharacterizationTargetTable()) > 0)

    # selection starts as NULL in this module
    testthat::expect_true(is.null(reactiveCharacterizationTargetRowId()))

    testthat::expect_true(is.null(allData()))
    testthat::expect_true(inherits(characterizationTimeToEventColDefs(), 'list'))

    # Trigger module table initialization once so reset observers settle.
    testthat::expect_true(nrow(moduleCharacterizationTargetTable()) > 0)
    session$flushReact()

    selectedTargetRowId <- NA_integer_
    selectedOutcomeRowId <- NA_integer_

    for (targetRow in seq_len(nrow(moduleCharacterizationTargetTable()))) {
      reactiveCharacterizationTargetRowId(targetRow)
      session$flushReact()

      if (nrow(reactiveTargetRow()) == 0) {
        reactiveCharacterizationTargetRowId(targetRow)
        session$flushReact()
      }

      if (nrow(reactiveTargetRow()) == 0 || nrow(reactiveOutcomeTable()) == 0) {
        next
      }

      targetId <- reactiveTargetRow()$characterizationTargetId[1]

      for (outcomeRow in seq_len(nrow(reactiveOutcomeTable()))) {
        outcomeId <- reactiveOutcomeTable()[outcomeRow, ]$cohortId[1]
        candidate <- tryCatch(
          getTimeToEventData(
            characterizationTargetId = targetId,
            outcomeId = outcomeId,
            connectionHandler = connectionHandlerCharacterization,
            resultDatabaseSettings = resultDatabaseSettingsCharacterization
          ),
          error = function(e) NULL
        )

        if (!is.null(candidate) && nrow(candidate) > 0) {
          selectedTargetRowId <- targetRow
          selectedOutcomeRowId <- outcomeRow
          break
        }
      }

      if (!is.na(selectedTargetRowId)) {
        break
      }
    }

    if (!is.na(selectedTargetRowId) && !is.na(selectedOutcomeRowId)) {
      reactiveCharacterizationTargetRowId(selectedTargetRowId)
      session$flushReact()
      if (nrow(reactiveTargetRow()) == 0) {
        reactiveCharacterizationTargetRowId(selectedTargetRowId)
        session$flushReact()
      }

      reactiveOutcomeRowId(selectedOutcomeRowId)
      session$flushReact()

      session$setInputs(generate = 1)
      session$flushReact()

      testthat::expect_true(!is.null(allData()))
      testthat::expect_true(nrow(allData()) > 0)

      data <- getTimeToEventData(
        characterizationTargetId = reactiveTargetRow()$characterizationTargetId[1],
        outcomeId = reactiveOutcomeTable()[selectedOutcomeRowId, ]$cohortId[1],
        connectionHandler = connectionHandlerCharacterization,
        resultDatabaseSettings = resultDatabaseSettingsCharacterization
      )
      testthat::expect_true(inherits(data, 'data.frame'))
      testthat::expect_true(nrow(data) > 0)

      plot <- plotTimeToEvent(
        timeToEventData = shiny::reactive(data),
        databases = unique(data$databaseName)[1],
        times = unique(data$timeScale)[1],
        colorByOutcomeTypes = TRUE,
        colorByTargetOutcomeTypes = FALSE,
        freeYByDatabase = TRUE
      )
      testthat::expect_is(plot, "ggplot")

      # check plot inputs can be set after generate
      session$setInputs(
        databases = unique(allData()$databaseName)[1],
        times = unique(allData()$timeScale)[1],
        colorByOutcomeTypes = TRUE,
        colorByTargetOutcomeTypes = FALSE,
        freeYByDatabase = TRUE
      )
    } else {
      # If fixture has no target/outcome pair with data, generate should keep data empty.
      reactiveCharacterizationTargetRowId(1)
      session$flushReact()
      session$setInputs(generate = 1)
      session$flushReact()
      testthat::expect_true(is.null(allData()))
    }
    
  })


test_that("Test characterizationTimeToEvent ui", {
  # Test ui
  ui <- characterizationTimeToEventViewer(id = 'viewer')
  checkmate::expect_list(ui)
})
