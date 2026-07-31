context("components-data-viewer")

test_that("components-data-viewer server works", {
  
shiny::testServer(
  app = resultTableServer, 
  args = list(
    df = shiny::reactive({
      data.frame(
        a = 1:5,
        b= 2:6
      )
    })
  ), 
  expr = {
    
    testthat::expect_true(nrow(dfWithActions()) == 5)
    testthat::expect_true(onClick == "select")
    
    testthat::expect_true(actionCount() == 0 )
    testthat::expect_true(is.list(actionIndex()))
    testthat::expect_true(is.na(actionIndex()$index))
    testthat::expect_true(actionType() == 'none' )
    
    session$setInputs(dataCols = columnsToSelectOptions()[1])
    
})
  
})


test_that("Test result table ui", {
  # Test ui
  ui <- resultTableViewer()
  checkmate::expect_list(ui)
})


test_that("createActionButton returns expected defaults and custom values", {
  defaultAction <- createActionButton(actionType = "results")

  testthat::expect_true(is.list(defaultAction))
  testthat::expect_equal(defaultAction$actionType, "results")
  testthat::expect_equal(defaultAction$buttonIcon, "play")
  testthat::expect_equal(defaultAction$buttonLabel, "View results")
  testthat::expect_match(defaultAction$hoverText, "Run action: results", fixed = TRUE)

  customAction <- createActionButton(
    actionType = "fails",
    buttonIcon = "triangle-exclamation",
    hoverText = "Show failed rows",
    buttonClass = "btn btn-xs",
    buttonStyle = "padding: 1px 2px;",
    buttonLabel = "View Fails"
  )

  testthat::expect_equal(customAction$actionType, "fails")
  testthat::expect_equal(customAction$buttonIcon, "triangle-exclamation")
  testthat::expect_equal(customAction$hoverText, "Show failed rows")
  testthat::expect_equal(customAction$buttonClass, "btn btn-xs")
  testthat::expect_equal(customAction$buttonStyle, "padding: 1px 2px;")
  testthat::expect_equal(customAction$buttonLabel, "View Fails")
})


test_that("createActionButton validates actionType", {
  testthat::expect_error(
    createActionButton(actionType = ""),
    "actionType must be a single non-empty string"
  )
  testthat::expect_error(
    createActionButton(actionType = NULL),
    "actionType must be a single non-empty string"
  )
})


test_that("addActions renders configured action buttons", {
  shiny::testServer(
    app = resultTableServer,
    args = list(
      df = shiny::reactive({
        data.frame(a = 1:2, b = 3:4)
      }),
      addActions = list(
        createActionButton(
          actionType = "results",
          buttonIcon = "chart-line",
          hoverText = "Open result details",
          buttonLabel = "Open Results",
          buttonClass = "btn btn-xs",
          buttonStyle = "padding: 3px 10px;"
        )
      )
    ),
    expr = {
      testthat::expect_true("actions" %in% names(colDefsInput))

      actionCell <- colDefsInput[["actions"]]$cell(value = "", index = 2)
      actionHtml <- as.character(actionCell)

      testthat::expect_match(actionHtml, "Open Results", fixed = TRUE)
      testthat::expect_match(actionHtml, "Open result details", fixed = TRUE)
      testthat::expect_match(actionHtml, "btn btn-xs", fixed = TRUE)
      testthat::expect_match(actionHtml, "action_click", fixed = TRUE)
      testthat::expect_match(actionHtml, "index: 2", fixed = TRUE)
    }
  )
})


# extractColumnRelations
