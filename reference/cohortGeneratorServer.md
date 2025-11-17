# The module server for the main cohort generator module

The module server for the main cohort generator module

## Usage

``` r
cohortGeneratorServer(id, connectionHandler, resultDatabaseSettings)
```

## Arguments

- id:

  the unique reference id for the module

- connectionHandler:

  a connection to the database with the results

- resultDatabaseSettings:

  a named list containing the cohort generator results database details
  (schema, table prefix)

## Value

the cohort generator results viewer main module server

## See also

Other CohortGenerator:
[`cohortGeneratorHelperFile()`](cohortGeneratorHelperFile.md),
[`cohortGeneratorViewer()`](cohortGeneratorViewer.md)
