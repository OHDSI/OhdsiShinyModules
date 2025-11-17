# Create a CD data source from a database

use this to create an interface to cohort diagnostics results data NOTE:
I think this would make a good R6 class for other objects in this
package so you could query them outside of a shiny app. E.g. if you
wanted to make a custom R markdown template

## Usage

``` r
createCdDatabaseDataSource(
  connectionHandler,
  resultDatabaseSettings,
  dataModelSpecificationsPath = system.file("cohort-diagnostics-ref",
    "resultsDataModelSpecification.csv", package = utils::packageName()),
  dataMigrationsRef = system.file("cohort-diagnostics-ref", "migrations.csv", package =
    utils::packageName()),
  displayProgress = FALSE
)
```

## Arguments

- connectionHandler:

  An instance of a ResultModelManager::connectionHander - manages a
  connection to a database.

- resultDatabaseSettings:

  a list containing the result schema and prefixes

- dataModelSpecificationsPath:

  The path to a file containing specifications for the data model used
  by the database.

- dataMigrationsRef:

  The path to a file listing all migrations for the data model that
  should have been applied

- displayProgress:

  display a progress messaage (can only be used inside a shiny reactive
  context)

## Value

An object of class \`CdDataSource\`.

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
[`databaseInformationView()`](databaseInformationView.md),
[`getCirceRenderedExpression()`](getCirceRenderedExpression.md),
[`getEnabledCdReports()`](getEnabledCdReports.md),
[`incidenceRatesView()`](incidenceRatesView.md),
[`inclusionRulesView()`](inclusionRulesView.md),
[`indexEventBreakdownView()`](indexEventBreakdownView.md),
[`orpahanConceptsView()`](orpahanConceptsView.md),
[`timeDistributionsView()`](timeDistributionsView.md),
[`visitContextView()`](visitContextView.md)
