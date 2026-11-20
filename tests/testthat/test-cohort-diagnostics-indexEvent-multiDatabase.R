context("cohort-diagnostics-indexEventBreakdown multi database")

# The year detail for a code must cover *every* selected database, not just the
# database of the summary row that was expanded, and the plot and table must be
# laid out side by side rather than stacked.

makeMultiDbYears <- function() {
  data.frame(
    databaseId = c("db1", "db1", "db1", "db2", "db2", "db3"),
    cohortId = 1L,
    conceptId = c(101L, 101L, 101L, 101L, 101L, 101L),
    cohortStartYear = c(-1L, 2020L, 2021L, -1L, 2020L, 2021L),
    subjectCount = c(400L, 300L, 100L, 50L, 40L, 10L),
    conceptCount = c(900L, 500L, 400L, 60L, 50L, 10L),
    coefficient = 0,
    cohortSubjects = c(634L, 634L, 634L, 100L, 100L, 20L),
    subjectPercent = c(0.63, 0.47, 0.16, 0.50, 0.40, 0.50),
    conceptPercent = c(1, 1, 1, 1, 1, 1),
    stringsAsFactors = FALSE
  )
}

databaseTable <- data.frame(
  databaseId = c("db1", "db2", "db3"),
  databaseName = c("Database One", "Database Two", "Database Three"),
  stringsAsFactors = FALSE
)

test_that("every database reporting a year is prepared, not just one", {
  prepared <- OhdsiShinyModules:::prepareIndexEventYearDetail(
    data = makeMultiDbYears(),
    databaseIds = c("db1", "db2", "db3"),
    databaseTable = databaseTable
  )

  # the -1 overall rows are excluded from the year detail
  expect_false(any(prepared$cohortStartYear == -1))
  expect_setequal(unique(prepared$databaseId), c("db1", "db2", "db3"))
  # db1 contributes 2020 and 2021, db2 2020, db3 2021
  expect_equal(nrow(prepared), 4)
  # database names are carried through for the legend and table
  expect_setequal(
    unique(prepared$databaseName),
    c("Database One", "Database Two", "Database Three")
  )
})

test_that("a database with no rows for the code is not invented", {
  prepared <- OhdsiShinyModules:::prepareIndexEventYearDetail(
    data = makeMultiDbYears(),
    databaseIds = c("db1", "db2", "db3", "db4"),
    databaseTable = rbind(
      databaseTable,
      data.frame(databaseId = "db4", databaseName = "Database Four")
    )
  )

  # db4 reported nothing, so it contributes no plotted points and must not
  # appear as a zero line
  expect_false("db4" %in% prepared$databaseId)
})

test_that("ordering is by database then year", {
  prepared <- OhdsiShinyModules:::prepareIndexEventYearDetail(
    data = makeMultiDbYears(),
    databaseIds = c("db1", "db2", "db3"),
    databaseTable = databaseTable
  )

  expect_equal(
    paste(prepared$databaseId, prepared$cohortStartYear),
    c("db1 2020", "db1 2021", "db2 2020", "db3 2021")
  )
})

test_that("percent mode plots the fraction of cohort subjects", {
  prepared <- OhdsiShinyModules:::prepareIndexEventYearDetail(
    data = makeMultiDbYears(),
    databaseIds = c("db1", "db2", "db3"),
    databaseTable = databaseTable,
    showAsPercent = TRUE
  )

  expect_true(all(prepared$persons <= 1, na.rm = TRUE))
  expect_equal(prepared$persons[prepared$databaseId == "db1" &
    prepared$cohortStartYear == 2020], 0.47)
})

test_that("count mode plots raw persons", {
  prepared <- OhdsiShinyModules:::prepareIndexEventYearDetail(
    data = makeMultiDbYears(),
    databaseIds = c("db1", "db2", "db3"),
    databaseTable = databaseTable,
    showAsPercent = FALSE
  )

  expect_equal(sum(prepared$persons), 300 + 100 + 40 + 10)
})

test_that("detail panel shows all databases and lays table beside plot", {
  panel <- OhdsiShinyModules:::indexEventBreakdownYearDetail(
    data = makeMultiDbYears(),
    databaseIds = c("db1", "db2", "db3"),
    databaseTable = databaseTable,
    showAsPercent = FALSE,
    valueLabel = "Persons"
  )

  rendered <- as.character(panel)
  # all three databases are accounted for in the heading
  expect_true(grepl("detail by database (3)", rendered, fixed = TRUE))
  # side by side, not stacked: a wrapping flex row with a flex-basis for each
  expect_true(grepl("flex-wrap: wrap", rendered, fixed = TRUE))
  expect_true(grepl("flex: 1 1 480px", rendered, fixed = TRUE))
  expect_true(grepl("flex: 0 1 420px", rendered, fixed = TRUE))
})

test_that("the percentage glossary explains the denominator", {
  note <- as.character(OhdsiShinyModules:::indexEventBreakdownPercentNote())

  expect_true(grepl("denominator", note, fixed = TRUE))
  # the distinction that matters most to an epidemiologist: this is not a rate
  expect_true(grepl("cohort start year", note, fixed = TRUE))
  expect_true(grepl("do not sum", note, fixed = TRUE))
})

test_that("missing data returns a message rather than an error", {
  expect_s3_class(
    OhdsiShinyModules:::indexEventBreakdownYearDetail(
      data = NULL,
      databaseIds = "db1",
      databaseTable = databaseTable
    ),
    "shiny.tag"
  )
})
