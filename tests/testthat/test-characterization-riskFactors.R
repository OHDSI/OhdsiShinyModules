context("characterization-riskFactors")

# move this to setup as will be used by all modules
targetCohort <- OhdsiShinyModules:::getTargetsUsedInCharMain(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema,
  cTablePrefix = resultDatabaseSettingsCharacterization$cTablePrefix,
  cgTablePrefix = resultDatabaseSettingsCharacterization$cgTablePrefix,
  ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix
)

targets4module <- targetCohort %>% dplyr::filter(as.integer(.data$riskFactors) == 1)

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
  dplyr::filter(as.integer(.data$riskFactors) == 1)


shiny::testServer(
  app = characterizationRiskFactorServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization ,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    reactiveCharacterizationTargetTable = shiny::reactive(characterizationTargetTable)
    ), 
  expr = {

    testthat::expect_true(nrow(reactiveCharacterizationTargetTable()) > 0)

    # target pop row is initially NULL
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

    # now reactiveOutcomesUsed should be set but the row should be NULL
    testthat::expect_true(nrow(reactiveOutcomesUsed()) > 0)
    testthat::expect_true(is.null(reactiveOutcomeCaseRowId()))

    # Find an outcome/database pair that actually has risk factor rows.
    selectedOutcomeRowId <- NA_integer_
    selectedDatabaseId <- NA_character_
    selectedDatabaseName <- NA_character_

    for (outcomeRow in seq_len(nrow(reactiveOutcomesUsed()))) {
      for (dbIndex in seq_along(databaseIds())) {
        candidate <- characterizationGetRiskFactorData(
          connectionHandler = connectionHandler,
          resultDatabaseSettings = resultDatabaseSettings,
          characterizationCaseId = reactiveOutcomesUsed()$characterizationCaseId[outcomeRow],
          databaseId = databaseIds()[dbIndex]
        )

        hasBinary <- !is.null(candidate$binary) && nrow(candidate$binary) > 0
        hasContinuous <- !is.null(candidate$continuous) && nrow(candidate$continuous) > 0

        if (isTRUE(hasBinary) && isTRUE(hasContinuous)) {
          selectedOutcomeRowId <- outcomeRow
          selectedDatabaseId <- databaseIds()[dbIndex]
          selectedDatabaseName <- databaseNames()[dbIndex]
          break
        }
      }
      if (!is.na(selectedOutcomeRowId)) {
        break
      }
    }

    testthat::expect_true(!is.na(selectedOutcomeRowId))
    testthat::expect_true(!is.na(selectedDatabaseId))
    testthat::expect_true(!is.na(selectedDatabaseName))

    # now set the outcome using a row that has data
    reactiveOutcomeCaseRowId(selectedOutcomeRowId)
    session$flushReact()

    # Outcome selection can similarly reset after outcomes table first updates.
    if (nrow(reactiveSelectedOutcomeCaseRow()) == 0) {
      reactiveOutcomeCaseRowId(selectedOutcomeRowId)
      session$flushReact()
    }

    testthat::expect_true(nrow(reactiveSelectedOutcomeCaseRow()) == 1)

    # check database
    testthat::expect_true(length(databaseNames()) > 0 )
    testthat::expect_true(length(databaseIds()) > 0 )
    
    #Test characterizationGetRiskFactorData
    data <- characterizationGetRiskFactorData(
      connectionHandler = connectionHandlerCharacterization ,
      resultDatabaseSettings = resultDatabaseSettingsCharacterization,
      characterizationCaseId = reactiveSelectedOutcomeCaseRow()$characterizationCaseId[1],
      databaseId = selectedDatabaseId
    )
    
    testthat::expect_true(inherits(data, 'list'))
    testthat::expect_true(nrow(data$binary) > 0 )
    testthat::expect_true(nrow(data$continuous) > 0 )
    
    session$setInputs(databaseName = selectedDatabaseName)
    session$setInputs(generate = 1)
    session$flushReact()

    # generate should run and keep selected row stable
    testthat::expect_true(reactiveOutcomeCaseRowId() == selectedOutcomeRowId)
    
    #testthat::expect_true(inherits(allData, 'list'))
    #testthat::expect_true( nrow(allData$binary) > 0 )
    #testthat::expect_true( nrow( allData$continuous) > 0 )
    

    
  })


test_that("Test characterizationRiskFactorViewer ui", {
  # Test ui
  ui <- characterizationRiskFactorViewer(id = 'viewer')
  checkmate::expect_list(ui)
})

test_that("Test characteriationRiskFactorColDefs", {
  colDefs <- characteriationRiskFactorColDefs()
  testthat::expect_true(inherits( colDefs, 'list'))
  testthat::expect_true(length(colDefs) > 0 )
})

test_that("Test characteriationRiskFactorContColDefs", {
  colDefs <- characteriationRiskFactorContColDefs()
  testthat::expect_true(inherits( colDefs, 'list'))
  testthat::expect_true(length(colDefs) > 0 )
})

test_that("hasAggregateRiskFactorSupport reflects the installed OhdsiReportGenerator", {
  result <- hasAggregateRiskFactorSupport()
  
  testthat::expect_true(is.logical(result))
  testthat::expect_equal(length(result), 1)
  testthat::expect_equal(
    result,
    "getAggregateBinaryRiskFactors" %in% getNamespaceExports("OhdsiReportGenerator")
  )
})

test_that("addRiskFactorConsistency adds the share of databases that agreed", {
  data <- data.frame(
    covariateName = c('a', 'b', 'c'),
    numDbs = c(4, 4, 0),
    posDbs = c(4, 1, 0),
    negDbs = c(0, 3, 0)
  )
  
  result <- addRiskFactorConsistency(df = data)
  
  testthat::expect_true('consistency' %in% colnames(result))
  testthat::expect_equal(result$consistency, c(1, 0.75, NA_real_))
})

test_that("addRiskFactorConsistency leaves single database results unchanged", {
  data <- data.frame(covariateName = 'a', absSmd = 0.1)
  
  testthat::expect_equal(addRiskFactorConsistency(df = data), data)
  testthat::expect_null(addRiskFactorConsistency(df = NULL))
  testthat::expect_equal(nrow(addRiskFactorConsistency(df = data[0, ])), 0)
})

test_that("trimColumnGroups drops columns and groups that are not in the data", {
  data <- data.frame(caseCount = 1, nonCaseCount = 2)
  
  columnGroups <- list(
    reactable::colGroup(name = 'Case', columns = c('caseCount', 'caseAverage')),
    reactable::colGroup(name = 'Non Case', columns = c('nonCaseCount')),
    reactable::colGroup(name = 'Missing', columns = c('somethingElse'))
  )
  
  result <- trimColumnGroups(columnGroups = columnGroups, df = data)
  
  testthat::expect_equal(length(result), 2)
  testthat::expect_equal(unlist(result[[1]]$columns), 'caseCount')
  testthat::expect_equal(unlist(result[[2]]$columns), 'nonCaseCount')
})

test_that("trimColumnGroups returns NULL when nothing is left to group", {
  data <- data.frame(caseCount = 1)
  
  columnGroups <- list(
    reactable::colGroup(name = 'Case', columns = c('caseAverage'))
  )
  
  testthat::expect_null(trimColumnGroups(columnGroups = columnGroups, df = data))
  testthat::expect_null(trimColumnGroups(columnGroups = NULL, df = data))
  testthat::expect_null(trimColumnGroups(columnGroups = columnGroups, df = NULL))
})

test_that("applyAggregateFilters keeps rows meeting the consistency and database thresholds", {
  data <- data.frame(
    covariateName = c('a', 'b', 'c'),
    consistency = c(1, 0.5, NA_real_),
    numDbs = c(4, 4, 1)
  )
  
  testthat::expect_equal(
    applyAggregateFilters(data = data, minConsistency = 0.75, minNumDbs = 0)$covariateName,
    'a'
  )
  
  testthat::expect_equal(
    applyAggregateFilters(data = data, minConsistency = 0, minNumDbs = 4)$covariateName,
    c('a', 'b')
  )
  
  testthat::expect_equal(
    nrow(applyAggregateFilters(data = data, minConsistency = 0, minNumDbs = 0)),
    2
  )
})

test_that("applyAggregateFilters is a no-op for single database results", {
  data <- data.frame(covariateName = c('a', 'b'), absSmd = c(0.1, 0.2))
  
  testthat::expect_equal(
    applyAggregateFilters(data = data, minConsistency = 0.9, minNumDbs = 10),
    data
  )
  testthat::expect_null(applyAggregateFilters(data = NULL, minConsistency = 0, minNumDbs = 0))
})
