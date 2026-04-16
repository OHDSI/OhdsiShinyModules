# The module server for the main phevaluator module

The module server for the main phevaluator module

## Usage

``` r
phevaluatorServer(id, connectionHandler, resultDatabaseSettings)
```

## Arguments

- id:

  The unique reference id for the module

- connectionHandler:

  A connection to the database with the results

- resultDatabaseSettings:

  A named list containing the cohort generator results database details
  (schema, table prefix)

## Value

The phevaluator main module server

## See also

Other PheValuator:
[`phevaluatorHelperFile()`](phevaluatorHelperFile.md),
[`phevaluatorViewer()`](phevaluatorViewer.md)
