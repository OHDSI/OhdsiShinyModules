context("characterization-DechallengeRechallenge")

targetTable <- OhdsiShinyModules:::getTargetsUsedInCharMain(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema,
  cTablePrefix = resultDatabaseSettingsCharacterization$cTablePrefix,
  cgTablePrefix = resultDatabaseSettingsCharacterization$cgTablePrefix,
  ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix
)

targets4module <- targetTable %>%
  dplyr::filter(as.integer(.data$dechalRechal) == 1)

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
  dplyr::filter(as.integer(.data$dechalRechal) == 1)

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


shiny::testServer(
  app = characterizationDechallengeRechallengeServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    reactiveCharacterizationTargetTable = shiny::reactive(characterizationTargetTable),
    reactiveOutcomeTable = shiny::reactive(outcomeCohort)
  ), 
  expr = {

    testthat::expect_true(nrow(characterizationTargetTable) > 0)
    
    # allData null initially
    testthat::expect_true(is.null(allData()) )

    # target row starts as NULL in this module
    testthat::expect_true(is.null(reactiveCharacterizationTargetRowId()))

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

      if (nrow(reactiveTargetRow()) == 0) {
        next
      }

      if (nrow(outcomeTableForSelect()) == 0) {
        next
      }

      targetCharacterizationId <- reactiveTargetRow()$characterizationTargetId[1]

      for (outcomeRow in seq_len(nrow(outcomeTableForSelect()))) {
        outcomeId <- outcomeTableForSelect()[outcomeRow, ]$cohortId[1]

        candidate <- getDechalRechalInputsData(
          characterizationTargetId = targetCharacterizationId,
          outcomeId = outcomeId,
          connectionHandler = connectionHandlerCharacterization,
          resultDatabaseSettings = resultDatabaseSettingsCharacterization
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
      # Set target and outcome using a pair known to have data.
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
    } else {
      # If fixtures have no matching pair with data, generate must leave results empty.
      reactiveCharacterizationTargetRowId(1)
      session$flushReact()
      session$setInputs(generate = 1)
      session$flushReact()
      testthat::expect_true(is.null(allData()))
    }
    
    # characteriationDechalRechalColDefs is a list
    testthat::expect_true(inherits(characteriationDechalRechalColDefs(), 'list'))
    
    # failData NULL
    testthat::expect_true(is.null(failData()))
    
    # tableOutputs$actionCount()
    # failData not NULL
    
    
    if (!is.na(selectedOutcomeRowId) && nrow(reactiveTargetRow()) > 0) {
      data <- getDechalRechalInputsData(
        characterizationTargetId = reactiveTargetRow()$characterizationTargetId[1],
        outcomeId = outcomeTableForSelect()[selectedOutcomeRowId, ]$cohortId[1],
        connectionHandler = connectionHandlerCharacterization,
        resultDatabaseSettings = resultDatabaseSettingsCharacterization
      )
      testthat::expect_true(inherits(data, 'data.frame'))
      testthat::expect_true(nrow(data) > 0)
    }
    
    # add tests for functions with progress bar
    
    if (!is.null(allData()) && nrow(allData()) > 0 && !is.na(selectedOutcomeRowId) && nrow(reactiveTargetRow()) > 0) {
      fails <- getDechalRechalFailData(
        characterizationTargetId = reactiveTargetRow()$characterizationTargetId[1],
        outcomeId = outcomeTableForSelect()[selectedOutcomeRowId, ]$cohortId[1],
        databaseId = allData()$databaseId[1],
        dechallengeStopInterval = allData()$dechallengeStopInterval[1],
        dechallengeEvaluationWindow = allData()$dechallengeEvaluationWindow[1],
        connectionHandler = connectionHandlerCharacterization,
        resultDatabaseSettings = resultDatabaseSettingsCharacterization
      )
      
      testthat::expect_true(inherits(fails, 'data.frame'))
      
      if(nrow(fails) > 0){
        plot <- plotDechalRechal(
          dechalRechalData = fails,
          i = 1
        )
        testthat::expect_true(inherits(plot, 'ggplot'))
      }
    }
    
  })

test_that("Test characterizationDechallengeRechallenge ui", {
  # Test ui
  ui <- characterizationDechallengeRechallengeViewer(id = 'viewer')
  checkmate::expect_list(ui)
})


test_that("Test characteriationDechalRechalColDefs", {
  # Test ui
  colDef <- characteriationDechalRechalColDefs()
  testthat::expect_is(colDef, 'list')
})
