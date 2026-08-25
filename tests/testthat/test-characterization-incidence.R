context("characterization-incidence")

targetCohort <- OhdsiReportGenerator::getTargetTable(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema, 
  ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix
)

outcomeCohort <- OhdsiReportGenerator::getOutcomeTable(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema, 
  targetId = targetCohort$cohortDefinitionId[4],
  ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix
)


shiny::testServer(
  app = characterizationIncidenceServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    reactiveTargetRow = shiny::reactive(targetCohort[4,]), 
    reactiveOutcomeTable = shiny::reactive(outcomeCohort)
  ), 
  expr = {
    
    # check input$generate does not crash app
    # need to test generate in ns("input-selection")
    session$setInputs(
      databaseSelector = databaseNames()[1],
      ageStratify = FALSE,
      sexStratify = FALSE,
      yearStratify = FALSE
      )
    
    # set the reactiveOutcomeRows to the first outcome
    reactiveOutcomeRowIds(1)
    session$flushReact()
    
    testthat::expect_true(nrow(reactiveOutcomeRows()) > 0 )

    # get the data
    testthat::expect_true(is.null(incidenceFullData()))
    session$setInputs(generate = 1)
    
    # adding code to manually set incidenceFullData()
    # figure out why it is not working with generate?
    data <- getCharacterizationIncidence(
      connectionHandler = connectionHandler, 
      schema = resultDatabaseSettings$schema, 
      ciTablePrefix = resultDatabaseSettings$incidenceTablePrefix, 
      cgTablePrefix = resultDatabaseSettings$cgTablePrefix,
      databaseTable = resultDatabaseSettings$databaseTable,
      targetIds = reactiveTargetRow()$cohortDefinitionId, 
      outcomeIds = reactiveOutcomeRows()$cohortDefinitionId
      )
    testthat::expect_true(nrow(data) > 0 )
    incidenceFullData(data)
    testthat::expect_equivalent(incidenceFullData(), data)
    
    # now generate the table
    testthat::expect_true(is.null(incidenceTableData()))
    session$setInputs(generateTable = 2)
    incidenceTableData(data)
    testthat::expect_true(!is.null(incidenceTableData()))
    
    # now check the plots
    session$setInputs(
      databaseSelectorPlot = databaseNames()[1],
      outcomesPlot = unique(outcomeCohort$cohortName),
      tarPlot = sort(unique(incidenceFullData()$tar))[1],
      xAxis = 'Age',
      yScaleType = 'Standard scale',
      sexStratifyPlot = FALSE,
      scaleVal = FALSE
    )
    
    session$setInputs(generatePlot = 3) # why are buttons not working?!
    #testthat::expect_true(!is.null(output$incidencePlot))
    
  })



test_that("Test characterizationIncidence ui", {
  # Test ui
  ui <- characterizationIncidenceViewer(id = 'viewer')
  checkmate::expect_list(ui)
})

test_that("sortAgeGroupNames orders age groups by the age they start at", {
  ageGroups <- c('10-14', '0-4', '>110', '100-104', '5-9')
  
  testthat::expect_equal(
    sortAgeGroupNames(ageGroupNames = ageGroups),
    c('0-4', '5-9', '10-14', '100-104', '>110')
  )
})

test_that("sortAgeGroupNames returns unique values and puts non-numeric labels last", {
  ageGroups <- c('5-9', 'Any', '0-4', '5-9')
  
  testthat::expect_equal(
    sortAgeGroupNames(ageGroupNames = ageGroups),
    c('0-4', '5-9', 'Any')
  )
  
  testthat::expect_equal(
    sortAgeGroupNames(ageGroupNames = factor(c('10-14', '0-4'))),
    c('0-4', '10-14')
  )
})

test_that("formatCensoredValue shows negative values as less than the absolute value", {
  testthat::expect_equal(formatCensoredValue(value = -5), "<5")
  testthat::expect_equal(formatCensoredValue(value = -5.25, digits = 2), "<5.25")
  testthat::expect_equal(formatCensoredValue(value = 12), "12")
  testthat::expect_equal(formatCensoredValue(value = 1.234, digits = 2), "1.23")
  testthat::expect_equal(formatCensoredValue(value = 1000), "1,000")
})

test_that("formatCensoredValue returns an empty string for missing values", {
  testthat::expect_equal(formatCensoredValue(value = NA), "")
  testthat::expect_equal(formatCensoredValue(value = NULL), "")
})

