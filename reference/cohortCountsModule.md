# Shiny module for cohort counts

Shiny module for cohort counts. Displays reactable table of cohort
counts

## Usage

``` r
cohortCountsModule(
  id,
  dataSource,
  cohortTable = dataSource$cohortTable,
  databaseTable = dataSource$dbTable,
  selectedCohorts,
  selectedDatabaseIds,
  cohortIds
)
```

## Arguments

- id:

  namespace id

- dataSource:

  Backend Data source (DatabaseConnection)

- cohortTable:

  data.frame of all cohorts

- databaseTable:

  data.frame of all databases

- selectedCohorts:

  shiny::reactive - should return cohorts selected or NULL

- selectedDatabaseIds:

  shiny::reactive - should return cohorts selected or NULL

- cohortIds:

  shiny::reactive - should return cohorts selected integers or NULL

## See also

Other CohortDiagnostics: [`cohortCountsView()`](cohortCountsView.md),
[`cohortDefinitionsModule()`](cohortDefinitionsModule.md),
[`cohortDefinitionsView()`](cohortDefinitionsView.md),
[`cohortDiagCharacterizationView()`](cohortDiagCharacterizationView.md),
[`cohortDiagnosticsHelperFile()`](cohortDiagnosticsHelperFile.md),
[`cohortDiagnosticsServer()`](cohortDiagnosticsServer.md),
[`cohortDiagnosticsView()`](cohortDiagnosticsView.md),
[`cohortOverlapView()`](cohortOverlapView.md),
[`compareCohortCharacterizationView()`](compareCohortCharacterizationView.md),
[`conceptsInDataSourceView()`](conceptsInDataSourceView.md),
[`createCdDatabaseDataSource()`](createCdDatabaseDataSource.md),
[`databaseInformationView()`](databaseInformationView.md),
[`getCirceRenderedExpression()`](getCirceRenderedExpression.md),
[`getEnabledCdReports()`](getEnabledCdReports.md),
[`incidenceRatesView()`](incidenceRatesView.md),
[`inclusionRulesView()`](inclusionRulesView.md),
[`indexEventBreakdownView()`](indexEventBreakdownView.md),
[`orpahanConceptsView()`](orpahanConceptsView.md),
[`timeDistributionsView()`](timeDistributionsView.md),
[`visitContextView()`](visitContextView.md)
