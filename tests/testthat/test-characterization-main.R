context("characterization-main")

shiny::testServer(
  app = characterizationServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization 
  ), 
  expr = {
    
    testthat::expect_true(inherits(connectionHandler,"ConnectionHandler"))
    
    # check initial settinfs
    testthat::expect_true(nrow(targetTable)>0)
    testthat::expect_true(is.null(reactiveOutcomeTable()))
    testthat::expect_true(resultType() == "")
    
    selectedTargetRowId <- NA_integer_
    selectedTargetRow <- NULL
    availableAnalyses <- character(0)

    analysisMap <- c(
      'Database Comparison' = 'databaseComparator',
      'Cohort Comparison' = 'cohortComparator',
      'Dechallenge Rechallenge' = 'dechalRechal',
      'Risk Factors' = 'riskFactors',
      'Time-to-event' = 'timeToEvent',
      'Case Series' = 'caseSeries',
      'Cohort Incidence' = 'cohortIncidence'
    )

    for (targetRow in seq_len(nrow(targetTable))) {
      targetRowData <- targetTable[targetRow, , drop = FALSE]
      candidateAnalyses <- names(analysisMap)[vapply(analysisMap, function(colName) {
        colName %in% colnames(targetRowData) && as.integer(targetRowData[[colName]][1]) == 1
      }, logical(1))]

      if (length(candidateAnalyses) == 0) {
        next
      }

      characterizations <- tryCatch(
        getCharacterizationTargetId(
          connectionHandler = connectionHandlerCharacterization,
          schema = resultDatabaseSettingsCharacterization$schema,
          databaseTable = resultDatabaseSettingsCharacterization$databaseTable,
          targetId = targetRowData$cohortDefinitionId[1],
          cgTablePrefix = resultDatabaseSettingsCharacterization$cgTablePrefix,
          cTablePrefix = resultDatabaseSettingsCharacterization$cTablePrefix
        ),
        error = function(e) NULL
      )

      if (is.null(characterizations) || nrow(characterizations) == 0) {
        next
      }

      selectedTargetRowId <- targetRow
      selectedTargetRow <- targetRowData
      availableAnalyses <- candidateAnalyses
      break
    }

    testthat::expect_true(!is.na(selectedTargetRowId))
    testthat::expect_true(length(availableAnalyses) > 0)

    # check selecting a target triggers characterization table + outcomes
    testthat::expect_true(!is.null(selectedTargetRow))
    reactiveTargetRowId(selectedTargetRowId)
    session$flushReact()

    if (nrow(reactiveTargetRow()) == 0) {
      reactiveTargetRowId(selectedTargetRowId)
      session$flushReact()
    }

    testthat::expect_true(nrow(reactiveTargetRow()) == 1)
    testthat::expect_true(!is.null(reactiveCharacterizationTargetTable()))
    testthat::expect_true(nrow(reactiveCharacterizationTargetTable()) > 0)
    testthat::expect_true(!is.null(reactiveOutcomeTable()))
    testthat::expect_true(nrow(reactiveOutcomeTable()) > 0)
    testthat::expect_true(!is.null(output$analysesOptions))

    # the server should auto-select one of the available analyses
    testthat::expect_true(resultType() %in% availableAnalyses)

    for (analysisName in availableAnalyses) {
      session$setInputs(resultType = analysisName)
      session$flushReact()
      testthat::expect_true(resultType() == analysisName)
      testthat::expect_true(!is.null(output$analysesResults))
    }
    
    # check the tab selector works
  })


test_that("Test characterization ui", {
  # Test ui
  ui <- characterizationViewer(id = 'viewer')
  checkmate::expect_list(ui)
})

