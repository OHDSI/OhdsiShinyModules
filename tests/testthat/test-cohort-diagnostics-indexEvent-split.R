context("cohort-diagnostics-indexEventBreakdown")

# The breakdown stores two kinds of row:
#   * one overall row per code, tagged cohort_start_year = -1, carrying counts
#     over the whole cohort; and
#   - one row per code per cohort start year, for the trend.
#
# The overall rows must be used as stored. subjectCount is a distinct count
# *within* a period, so adding the per-year rows together counts a subject once
# per year they appear in and can exceed the number of people in the cohort.

makeBreakdownRows <- function(cohortStartYear, subjectCount, conceptCount,
                              conceptId = 101L, databaseId = "db") {
  data.frame(
    databaseId = databaseId,
    cohortId = 1L,
    conceptId = conceptId,
    conceptName = paste("code", conceptId),
    conceptCode = as.character(conceptId),
    domainField = "condition_concept_id",
    vocabularyId = "SNOMED",
    standardConcept = "S",
    cohortSubjects = 634L,
    cohortEntries = 634L,
    cohortStartYear = cohortStartYear,
    subjectCount = subjectCount,
    conceptCount = conceptCount,
    stringsAsFactors = FALSE
  )
}

test_that("overall rows are separated from per-year rows", {
  data <- rbind(
    makeBreakdownRows(-1L, 400L, 900L),
    makeBreakdownRows(2020L, 300L, 500L),
    makeBreakdownRows(2021L, 300L, 400L)
  )

  split <- OhdsiShinyModules:::splitIndexEventBreakdown(data)

  expect_equal(nrow(split$overall), 1)
  expect_equal(nrow(split$byYear), 2)
  expect_equal(split$overall$cohortStartYear, -1L)
})

test_that("overall subject count is not the sum of the per-year counts", {
  # the same 300 people appear in both years
  data <- rbind(
    makeBreakdownRows(-1L, 400L, 900L),
    makeBreakdownRows(2020L, 300L, 500L),
    makeBreakdownRows(2021L, 300L, 400L)
  )

  split <- OhdsiShinyModules:::splitIndexEventBreakdown(data)

  expect_equal(split$overall$subjectCount, 400L)
  expect_equal(sum(split$byYear$subjectCount), 600L)
  expect_false(split$overall$subjectCount == sum(split$byYear$subjectCount))
})

test_that("overall percentage never exceeds 100 per cent", {
  data <- rbind(
    makeBreakdownRows(-1L, 400L, 900L),
    makeBreakdownRows(2020L, 300L, 500L),
    makeBreakdownRows(2021L, 300L, 400L)
  )

  split <- OhdsiShinyModules:::splitIndexEventBreakdown(data)
  fraction <- split$overall$subjectCount / split$overall$cohortSubjects

  expect_true(all(fraction <= 1))
})

test_that("results without a year column are all treated as overall rows", {
  data <- makeBreakdownRows(-1L, 100L, 100L)
  data$cohortStartYear <- NULL

  split <- OhdsiShinyModules:::splitIndexEventBreakdown(data)

  expect_equal(nrow(split$overall), 1)
  expect_null(split$byYear)
})

test_that("empty input is handled", {
  expect_null(OhdsiShinyModules:::splitIndexEventBreakdown(NULL)$byYear)
})
