# Cohort Diagnostics Explorer main module

Cohort Diagnostics Explorer main module

## Usage

``` r
cohortDiagnosticsServer(
  id,
  connectionHandler,
  resultDatabaseSettings,
  dataSource = NULL
)
```

## Arguments

- id:

  module Id

- connectionHandler:

  ResultModelManager ConnectionHander instance

- resultDatabaseSettings:

  results database settings

- dataSource:

  dataSource optionally created with createCdDatabaseDataSource

## See also

Other CohortDiagnostics:
[`cohortCountsModule()`](cohortCountsModule.md),
[`cohortCountsView()`](cohortCountsView.md),
[`cohortDefinitionsModule()`](cohortDefinitionsModule.md),
[`cohortDefinitionsView()`](cohortDefinitionsView.md),
[`cohortDiagCharacterizationView()`](cohortDiagCharacterizationView.md),
[`cohortDiagnosticsHelperFile()`](cohortDiagnosticsHelperFile.md),
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
