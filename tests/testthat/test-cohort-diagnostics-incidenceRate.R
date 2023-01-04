context("cohort-diagnostics-incidence")

test_that("plot works", {
  tData <- tibble::tibble(
    cohortCount = 10, personYears = 1000, gender = "m", ageGroup = "18-64", calendarYear = 2010,
    incidenceRate = 1003.4, cohortId = 18347, databaseId = "Eunomia", databaseName = "Eunomia", cohortSubjects = 1000
  )
  plt <- plotIncidenceRate(data = tData,
                    cohortTable = dataSourceCd$cohortTable,
                    stratifyByGender = TRUE,
                    stratifyByCalendarYear = TRUE,
                    yscaleFixed = TRUE,
                    stratifyByAgeGroup = TRUE)

  checkmate::expect_class(plt, "girafe")
})

shiny::testServer(incidenceRatesModule, args = list(
  id = "testIncidenceRates", #Any string is ok?
  dataSource = dataSourceCd,
  cohortTable = getCohortTable(dataSourceCd),
  selectedCohorts = shiny::reactive("Any String"),
  selectedDatabaseIds = shiny::reactive("Eunomia"),
  cohortIds = shiny::reactive({ c(18347) })
), {
  ## input tests will go here
  session$setInputs(
    irStratification = c("Age", "Calendar Year", "Sex"),
    minPersonYear = 0,
    minSubjectCount = 0,
    generatePlot = 1,
    incidenceRateCalenderFilter = c(2010, 2019)
  )
    
  checkmate::expect_data_frame(incidenceRateData())
  checkmate::expect_integerish(incidenceRateCalenderFilter())
})
