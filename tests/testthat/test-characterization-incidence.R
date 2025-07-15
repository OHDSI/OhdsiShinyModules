context("characterization-incidence")

targetCohort <- OhdsiReportGenerator::getTargetTable(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema, 
  ciTablePrefix = resultDatabaseSettingsCharacterization$incidenceTablePrefix
)

outcomeCohort <- OhdsiReportGenerator::getOutcomeTable(
  connectionHandler = connectionHandlerCharacterization,
  schema = resultDatabaseSettingsCharacterization$schema, 
  targetId = targetCohort$cohortId[2],
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
      databaseSelector = ciOptions$databases,
      ageIds = c(1),#ciOptions$ages,
      sexIds = ciOptions$sex,
      startYears = ciOptions$startYear[1],
      tars = ciOptions$sortedTars[1]
      )
    
    # set the reactiveOutcomeRows to the first outcome
    reactiveOutcomeRows(outcomeTable()[1,])

    # before generation the reactives should be NULL
    testthat::expect_true(is.null(incidenceRateTarFilter()))
    testthat::expect_true(is.null(incidenceRateCalendarFilter()))
    testthat::expect_true(is.null(incidenceRateAgeFilter()))
    testthat::expect_true(is.null(incidenceRateGenderFilter()))
    testthat::expect_true(is.null(incidenceRateDbFilter()))
    testthat::expect_true(is.null(outcomeIds()))
    
    session$setInputs(generate = T)
    
    # when generate is true the reactives should be populated
    testthat::expect_true(!is.null(incidenceRateTarFilter()))
    testthat::expect_true(!is.null(incidenceRateCalendarFilter()))
    testthat::expect_true(!is.null(incidenceRateAgeFilter())) # fails
    testthat::expect_true(!is.null(incidenceRateGenderFilter()))
    testthat::expect_true(!is.null(incidenceRateDbFilter()))
    testthat::expect_true(!is.null(outcomeIds()))
    
    # after generating test outcomes set 
    testthat::expect_true(outcomeIds() == reactiveOutcomeRows()$cohortId)
    
    # should have results after generate
    testthat::expect_true(!is.null(extractedData())) # fails
    
    
    idata <- getIncidenceData(
      targetIds = reactiveTargetRow()$cohortId,
      outcomeIds = outcomeTable()$cohortId,
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings
    )
    testthat::expect_is(idata, 'data.frame')
    
    idata <- getIncidenceData(
      targetIds = NULL,
      outcomeIds = outcomeTable()$cohortId,
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings
    )
    testthat::expect_is(idata, 'NULL')
    
    idata <- getIncidenceData(
      targetIds = 1,
      outcomeIds = NULL,
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings
    )
    testthat::expect_is(idata, 'NULL')
    
  })



test_that("Test characterizationIncidence ui", {
  # Test ui
  ui <- characterizationIncidenceViewer(id = 'viewer')
  checkmate::expect_list(ui)
})

test_that("Test as_ggplot global", {
  #Test as_ggplot function
    plot <- cowplot::get_legend(ggplot2::qplot(x=1:5, y=1:5, colour = runif(5)))
    result <- as_ggplot(plot)
    testthat::expect_is(result, 'ggplot')
})

