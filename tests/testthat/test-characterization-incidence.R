context("characterization-incidence")

options <- characterizationGetOptions(
  connectionHandler = connectionHandlerCharacterization,
  resultDatabaseSettings = resultDatabaseSettingsCharacterization,
  includeAggregate = T,
  includeIncidence = T
)
parents <- characterizationGetParents(options)

shiny::testServer(
  app = characterizationIncidenceServer, 
  args = list(
    connectionHandler = connectionHandlerCharacterization,
    resultDatabaseSettings = resultDatabaseSettingsCharacterization,
    parents = parents,
    parentIndex = shiny::reactive(1), # reactive
    outcomes = shiny::reactive(3), # reactive
    targetIds = shiny::reactive(1) # reactive
  ), 
  expr = {
    
    # check input$generate does not crash app
    # need to test generate in ns("input-selection")
    session$setInputs(
      outcomeIds = outcomes()[1],
      databaseSelector = databases,
      ageIds = ages,
      sexIds = sex,
      startYears = startYear[1],
      tars = sortedTars[1]
      )
    
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
    testthat::expect_true(!is.null(incidenceRateAgeFilter()))
    testthat::expect_true(!is.null(incidenceRateGenderFilter()))
    testthat::expect_true(!is.null(incidenceRateDbFilter()))
    testthat::expect_true(!is.null(outcomeIds()))
    
    testthat::expect_true(outcomeIds() == outcomes()[1])
    
    testthat::expect_true(inherits(options, 'list'))
    
    # should have results after generate
    testthat::expect_true(!is.null(extractedData()))
    
    
    idata <- getIncidenceData(
      targetIds = targetIds(),
      outcomeIds = outcomes()[1],
      connectionHandler = connectionHandler,
      resultDatabaseSettings = resultDatabaseSettings
    )
    testthat::expect_is(idata, 'data.frame')
    
    idata <- getIncidenceData(
      targetIds = NULL,
      outcomeIds = outcomes()[1],
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

