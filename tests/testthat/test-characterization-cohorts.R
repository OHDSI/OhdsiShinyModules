context("characterization-cohorts")

# move this to setup as will be used by all modules
targetTable <- OhdsiShinyModules:::getTargetsUsedInCharMain(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema,
  cTablePrefix = resultDatabaseSettingsCharacterization$cTablePrefix,
  cgTablePrefix = resultDatabaseSettingsCharacterization$cgTablePrefix,
  ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix
)

targetTable <- targetTable %>%
  dplyr::mutate(cohortDefinitionIdNum = suppressWarnings(as.integer(.data$cohortDefinitionId))) %>%
  dplyr::filter(!is.na(.data$cohortDefinitionIdNum))

targets4module <- targetTable %>%
  dplyr::filter(as.integer(.data$cohortComparator) == 1)

characterizationTargetTable <- OhdsiReportGenerator::getCharacterizationTargetSettings(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema,
  targetIds = targets4module$cohortDefinitionId[1],
  addDatabaseDetails = TRUE,
  databaseTable = resultDatabaseSettingsCharacterization$databaseTable,
  cgTablePrefix = resultDatabaseSettingsCharacterization$cgTablePrefix,
  cTablePrefix = resultDatabaseSettingsCharacterization$cTablePrefix
)

# keep only rows for this module
characterizationTargetTable <- characterizationTargetTable %>%
  dplyr::filter(as.integer(.data$cohortComparator) == 1)

shiny::testServer(
  app = characterizationCohortComparisonServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    targetTable = targetTable,
    reactiveCharacterizationTargetTable = shiny::reactive(characterizationTargetTable)
    ), 
  expr = {

    testthat::expect_true(nrow(reactiveCharacterizationTargetTable()) > 0)

    # target row starts as NULL in this module
    testthat::expect_true(is.null(reactiveCharacterizationTargetRowId()))

    # Trigger module table initialization once so reset observers settle.
    testthat::expect_true(nrow(moduleCharacterizationTargetTable()) > 0)
    session$flushReact()

    reactiveCharacterizationTargetRowId(1)
    session$flushReact()

    # In testServer, selection can be reset on first table change; set again once.
    if (nrow(reactiveTargetRow()) == 0) {
      reactiveCharacterizationTargetRowId(1)
      session$flushReact()
    }

    # reactiveTargetRow() should now be selected
    testthat::expect_true(nrow(reactiveTargetRow()) == 1)

    # Find a target/comparator/database triple that actually has comparison data.
    selectedTargetRowId <- NA_integer_
    selectedComparatorRowId <- NA_integer_
    selectedComparatorTargetRowId <- NA_integer_
    selectedDatabaseId <- NA_character_
    selectedDatabaseName <- NA_character_

    splitValues <- function(x) {
      if (is.null(x) || is.na(x) || !nzchar(x)) {
        return(character())
      }
      trimws(unlist(strsplit(x = x, split = ",")))
    }

    moduleTable <- moduleCharacterizationTargetTable()

    for (targetRow in seq_len(nrow(moduleTable))) {
      reactiveCharacterizationTargetRowId(targetRow)
      session$flushReact()

      if (nrow(reactiveTargetRow()) == 0) {
        next
      }

      targetTargetId <- suppressWarnings(as.integer(reactiveTargetRow()$characterizationTargetId[1]))
      if (is.na(targetTargetId)) {
        next
      }

      targetDbIds <- splitValues(moduleTable$databaseIdString[targetRow])
      targetDbNames <- splitValues(moduleTable$databaseString[targetRow])
      if (length(targetDbIds) == 0 || length(targetDbNames) == 0 || length(targetDbIds) != length(targetDbNames)) {
        next
      }

      targetDbMap <- stats::setNames(targetDbNames, targetDbIds)

      for (compRow in seq_len(nrow(moduleTable))) {
        if (compRow == targetRow) {
          next
        }

        compTargetId <- suppressWarnings(as.integer(moduleTable$characterizationTargetId[compRow]))
        if (is.na(compTargetId) || compTargetId == targetTargetId) {
          next
        }

        compDbIds <- splitValues(moduleTable$databaseIdString[compRow])
        sharedDbIds <- intersect(targetDbIds, compDbIds)
        if (length(sharedDbIds) == 0) {
          next
        }

        for (dbId in sharedDbIds) {
          candidate <- tryCatch(
            characterizatonGetCohortData(
              connectionHandler = connectionHandler,
              resultDatabaseSettings = resultDatabaseSettings,
              characterizationTargetIds = c(targetTargetId, compTargetId),
              databaseIds = dbId,
              minThreshold = 0
            ),
            error = function(e) NULL
          )

          if (is.null(candidate)) {
            next
          }

          hasRef <- !is.null(candidate$covRef) && nrow(candidate$covRef) > 0

          if (isTRUE(hasRef) && dbId %in% names(targetDbMap)) {
            selectedTargetRowId <- targetRow
            selectedComparatorRowId <- compRow
            selectedComparatorTargetRowId <- 1L
            selectedDatabaseId <- dbId
            selectedDatabaseName <- unname(targetDbMap[[dbId]])
            break
          }
        }

        if (!is.na(selectedTargetRowId)) {
          break
        }
      }

      if (!is.na(selectedTargetRowId)) {
        break
      }
    }

    testthat::expect_true(!is.na(selectedTargetRowId))
    testthat::expect_true(!is.na(selectedComparatorRowId))
    testthat::expect_true(!is.na(selectedComparatorTargetRowId))
    testthat::expect_true(!is.na(selectedDatabaseId))
    testthat::expect_true(!is.na(selectedDatabaseName))

    if (is.na(selectedTargetRowId) || is.na(selectedComparatorRowId) || is.na(selectedComparatorTargetRowId) || is.na(selectedDatabaseId) || is.na(selectedDatabaseName)) {
      return(invisible(NULL))
    }

    # Set selected rows/db that are known to have data.
    reactiveCharacterizationTargetRowId(selectedTargetRowId)
    session$flushReact()
    if (nrow(reactiveTargetRow()) == 0) {
      reactiveCharacterizationTargetRowId(selectedTargetRowId)
      session$flushReact()
    }

    comparatorCharacterizationTable(moduleTable[selectedComparatorRowId, , drop = FALSE])
    session$flushReact()
    comparatorCharacterizationTableRowId(selectedComparatorTargetRowId)
    session$flushReact()

    if (nrow(reactiveComparatorRow()) == 0) {
      comparatorCharacterizationTable(moduleTable[selectedComparatorRowId, , drop = FALSE])
      session$flushReact()
      comparatorCharacterizationTableRowId(selectedComparatorTargetRowId)
      session$flushReact()
    }

    testthat::expect_true(nrow(reactiveComparatorRow()) == 1)
    
    testthat::expect_true(length(databaseNames()) > 0)
    testthat::expect_true(length(databaseIds()) > 0)
    
    # set inputs
    session$setInputs(
      databaseName = selectedDatabaseName
    )
    
    # test generate
    session$setInputs(
      generate = TRUE
    )
    session$flushReact()
    
    testthat::expect_true(nrow(reactiveTargetRow()) == 1)
    testthat::expect_true(nrow(reactiveComparatorRow()) == 1)
    
    resultTable <- characterizatonGetCohortData(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      characterizationTargetIds = c(
        reactiveTargetRow()$characterizationTargetId,
        reactiveComparatorRow()$characterizationTargetId
      ),
      databaseIds = selectedDatabaseId,
      minThreshold = 0
    )
    testthat::expect_true(inherits(resultTable, 'list'))
    testthat::expect_true(inherits(resultTable$covariates, 'data.frame'))
    testthat::expect_true(inherits(resultTable$covRef, 'data.frame'))
    testthat::expect_true(nrow(resultTable$covRef) > 0)
    
    
    continuousTable <- characterizatonGetCohortComparisonDataContinuous(
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings,
      characterizationTargetIds = c(
        reactiveTargetRow()$characterizationTargetId,
        reactiveComparatorRow()$characterizationTargetId
      ),
      databaseIds = selectedDatabaseId
    )
    testthat::expect_true(inherits(continuousTable, 'list'))
    testthat::expect_true(inherits(continuousTable$covariates, 'data.frame'))
    
  
  })

test_that("Test characterizationTable ui", {
  # Test ui
  ui <- characterizationCohortComparisonViewer(id = 'viewer')
  checkmate::expect_list(ui)
})


test_that("Test characterizationCohortsColumns", {
  cols <- characterizationCohortsColumns()
  testthat::expect_true(inherits(cols, 'list'))
})


test_that("Test characterizationCohortsColumnsContinuous", {
  cols <- characterizationCohortsColumnsContinuous()
  testthat::expect_true(inherits(cols, 'list'))
})

