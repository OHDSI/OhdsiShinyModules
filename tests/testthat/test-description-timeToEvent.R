context("description-TimeToEvent")

shiny::testServer(
  app = descriptionTimeToEventServer, 
  args = list(
    con = connectionDesc,
    schema = schemaTest,
    dbms = dbmsTest,
    mainPanelTab = shiny::reactiveVal("Feature Comparison"),
    tablePrefix = descTablePrefix,
    cohortTablePrefix = cohortTablePrefix,
    databaseTable = databaseTable
  ), 
  expr = {
    
    # make sure bothIds returns a list
    testthat::expect_true(class(bothIds) == 'list')
    testthat::expect_true(!is.null(bothIds$outcomeIds) )

    
    # checl targetId does not crash app
    session$setInputs(targetId = names(bothIds$outcomeIds)[1])
    
    # check input$fetchData does not crash app
    session$setInputs(outcomeId = 3)
    session$setInputs(fetchData = T)
    
    
  })
