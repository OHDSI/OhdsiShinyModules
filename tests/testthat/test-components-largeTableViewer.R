# Create Tables
test_that("Large Data Table R6 Class works", {
  # Create connection handler
  cdTest <- DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = ":memory:")
  ch <- ResultModelManager::ConnectionHandler$new(cdTest)
  on.exit(ch$finalize())
  # 1 million random rows
  bigData <- data.frame(row_id = 1:1e6, value = stats::runif(1e6))
  DatabaseConnector::insertTable(ch$getConnection(), data = bigData, tableName = "big_table")

  query <- "SELECT * FROM main.big_table WHERE row_id >= @min_row"
  countQuery <- "SELECT count(*) FROM main.big_table WHERE row_id >= @min_row"

  ldt <- LargeDataTable$new(ch, query, countQuery)

  checkmate::expect_r6(ldt, "LargeDataTable")

  expect_equal(ldt$getAllResults(min_row = 1) %>% nrow(), bigData %>% nrow())
  expect_equal(ldt$getCount(min_row = 2), bigData %>% nrow() - 1)
  expect_equal(ldt$getCount(min_row = 1), bigData %>% nrow())
  checkmate::expect_data_frame(ldt$getPage(2, 10, min_row = 1))
  expect_equal(ldt$getPage(2, 10, min_row = 1) %>% nrow(), 10)
  expect_error(LargeDataTable$new(ch, "SELECT 1; SELECT 2;"))
  expect_error(LargeDataTable$new(ch, query, "SELECT 1; SELECT 2;"))
  expect_error(LargeDataTable$new(ch, query))

  ldt2 <- LargeDataTable$new(ch, query, countQuery = NULL))
  checkmate::expect_string(ldt2$countQuery)

  checkmate::expect_class(largeTableView("foo"), "shiny.tag")
  checkmate::expect_class(largeTableView("foo", fullDownloads = FALSE), "shiny.tag")

  shiny::testServer(
    app = largeTableServer,
    args = list(
      id = "foo",
      ldt = ldt,
      inputParams = list(min_row = 1),
      modifyData = function(df, pageNum, pageSize) {
        df %>% dplyr::mutate(fizzBuzz = ifelse(.data$rowId %% 3 == 0, "fizz", "buzz"))
      },
      columns = list(row_id = reactable::colDef("row id")
    ),
    expr = {

      session$setInputs(pageSize = 10)

      expect_equal(rowCount(), ldt$getCount(min_row = 1))
      expect_equal(pageNum(), 1)
      expect_equal(pageCount(), ceiling(ldt$getCount(min_row = 1) / 10))


      session$setInputs(pageSize = 100)
      expect_equal(pageNum(), 1)
      expect_equal(pageCount(), ceiling(ldt$getCount(min_row = 1) / 100))

      session$setInputs(pageNum = 5)
      expect_equal(pageNum(), 5)
      expect_equal(output$pageNumber, "401 - 500 of 1,000,000 rows")

      checkmate::expect_data_frame(dataPage(), nrow = 100, ncols = 3)
      checkmate::expect_names(colnames(dataPage()), must.include = c("rowId", "value", "fizzBuzz"))
      expect_true(all(dataPage()$fizzBuzz == ifelse(dataPage()$rowId %% 3 == 0, "fizz", "buzz")))

      checkmate::expect_class(output$tableView, "json")
      checkmate::expect_class(output$pageActionButtons , "list")

      session$setInputs(pageSize = 1e6)
      expect_equal(pageCount(), 1)
      checkmate::expect_class(output$pageActionButtons , "list")
      checkmate::expect_file_exists(output$downloadFull)
    }
  )
})