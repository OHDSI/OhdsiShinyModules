# Cohort Definition module

cohort defintion conceptsets, json etc

## Usage

``` r
cohortDefinitionsModule(
  id,
  dataSource,
  cohortDefinitions,
  cohortTable = dataSource$cohortTable,
  cohortCountTable = dataSource$cohortCountTable,
  databaseTable = dataSource$dbTable
)
```

## Arguments

- id:

  Namespace id

- dataSource:

  DatabaseConnection

- cohortDefinitions:

  reactive of cohort definitions to display

- cohortTable:

  data.frame of cohorts, cohortId, cohortName

- cohortCountTable:

  data.frame of cohortCounts, cohortId, subjects records

- databaseTable:

  data.frame of databasese, databaseId, name

## See also

Other CohortDiagnostics:
[`cohortCountsModule()`](cohortCountsModule.md),
[`cohortCountsView()`](cohortCountsView.md),
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
