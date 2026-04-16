# The server function for the datasources module

The server function for the datasources module

## Usage

``` r
datasourcesServer(id, connectionHandler, resultDatabaseSettings)
```

## Arguments

- id:

  The unique id for the datasources server namespace

- connectionHandler:

  A connection to the database with the results

- resultDatabaseSettings:

  A named list containing the cohort generator results database details
  (schema, table prefix)

## Value

The server for the datasources module

## See also

Other Utils: [`datasourcesHelperFile()`](datasourcesHelperFile.md),
[`datasourcesViewer()`](datasourcesViewer.md),
[`getLogoImage()`](getLogoImage.md),
[`resultTableViewer()`](resultTableViewer.md)
