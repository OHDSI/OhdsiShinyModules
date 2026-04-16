# The module server for exploring characterization studies

The module server for exploring characterization studies

## Usage

``` r
characterizationServer(
  id,
  connectionHandler,
  resultDatabaseSettings = list(port = 1)
)
```

## Arguments

- id:

  the unique reference id for the module

- connectionHandler:

  a connection to the database with the results

- resultDatabaseSettings:

  a list containing the characterization result schema, dbms,
  tablePrefix, databaseTable and cgTablePrefix

## Value

The server for the characterization module

## Details

The user specifies the id for the module

## See also

Other Characterization:
[`characterizationHelperFile()`](characterizationHelperFile.md),
[`characterizationIncidenceServer()`](characterizationIncidenceServer.md),
[`characterizationIncidenceViewer()`](characterizationIncidenceViewer.md),
[`characterizationViewer()`](characterizationViewer.md)
