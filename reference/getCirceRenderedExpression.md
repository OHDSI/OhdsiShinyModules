# Returns list with circe generated documentation

Returns list with circe generated documentation

## Usage

``` r
getCirceRenderedExpression(
  cohortDefinition,
  cohortName = "Cohort Definition",
  includeConceptSets = FALSE
)
```

## Arguments

- cohortDefinition:

  An R object (list) with a list representation of the cohort definition
  expression, that may be converted to a cohort expression JSON using
  jsonlite::toJSON(x = cohortDefinition, digits = 23, pretty = TRUE)

- cohortName:

  Name for the cohort definition

- includeConceptSets:

  Do you want to inclued concept set in the documentation

## Value

list object

## See also

Other CohortDiagnostics:
[`cohortCountsModule()`](cohortCountsModule.md),
[`cohortCountsView()`](cohortCountsView.md),
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
[`getEnabledCdReports()`](getEnabledCdReports.md),
[`incidenceRatesView()`](incidenceRatesView.md),
[`inclusionRulesView()`](inclusionRulesView.md),
[`indexEventBreakdownView()`](indexEventBreakdownView.md),
[`orpahanConceptsView()`](orpahanConceptsView.md),
[`timeDistributionsView()`](timeDistributionsView.md),
[`visitContextView()`](visitContextView.md)
