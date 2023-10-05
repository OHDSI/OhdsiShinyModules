test_that("Test about ui", {
  # Test ui
  ui <- aboutViewer()
  checkmate::expect_list(ui)
})
