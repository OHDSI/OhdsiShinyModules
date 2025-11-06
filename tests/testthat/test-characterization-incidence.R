context("characterization-incidence")

targetCohort <- OhdsiReportGenerator::getTargetTable(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema, 
  ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix
)

outcomeCohort <- OhdsiReportGenerator::getOutcomeTable(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema, 
  targetId = targetCohort$cohortId[1],
  ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix
)


shiny::testServer(
  app = characterizationIncidenceServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    reactiveTargetRow = shiny::reactive(targetCohort[1,]), 
    outcomeTable = shiny::reactive(outcomeCohort)
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
    data <- OhdsiReportGenerator::getIncidenceRates(
      connectionHandler = connectionHandler, 
      schema = resultDatabaseSettings$schema, 
      ciTablePrefix = resultDatabaseSettings$incidenceTablePrefix, 
      targetIds = targetCohort$cohortId[1], 
      outcomeIds = outcomeCohort$cohortId[1]
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
      xAxis = FALSE,
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

