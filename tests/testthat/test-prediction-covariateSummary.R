context("prediction-covariateSummary")

shiny::testServer(
  app = predictionCovariateSummaryServer, 
  args = list(
    modelDesignId = shiny::reactiveVal(1),
    developmentDatabaseId = shiny::reactiveVal(1),
    performanceId = shiny::reactiveVal(NULL),
    connectionHandler = connectionHandlerPlp,
    inputSingleView = shiny::reactiveVal("Discrimination"),
    mySchema = resultDatabaseSettingsPlp$schema,
    myTableAppend = resultDatabaseSettingsPlp$tablePrefix
  ), 
  expr = {
    expect_true(is.null(covariateSummary()))
    expect_true(is.null(intercept()))
    performanceId(1)
    inputSingleView('Model')
    expect_true(!is.null(covariateSummary()))
    expect_true(!is.null(intercept()))
    
    edits <- editCovariates(NULL)
    expect_equal(edits$table, data.frame(a=1))
    edits <- editCovariates(covariateSummary())
    expect_true(!is.null(edits$table))
    
    plots <- plotCovariateSummary(shiny::reactive(NULL))
    expect_true(is.null(plots$binary))
    plots <- plotCovariateSummary(covariateSummary)
    expect_true(!is.null(plots$binary))
    
  })