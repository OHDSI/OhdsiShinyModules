# The module server for exploring incidence results

The module server for exploring incidence results

## Usage

``` r
characterizationIncidenceServer(
  id,
  connectionHandler,
  resultDatabaseSettings,
  reactiveTargetRow,
  outcomeTable
)
```

## Arguments

- id:

  the unique reference id for the module

- connectionHandler:

  the connection to the prediction result database

- resultDatabaseSettings:

  a list containing the characterization result schema, dbms,
  tablePrefix, databaseTable and cgTablePrefix

- reactiveTargetRow:

  a reactive data.frame with the target of interest details

- outcomeTable:

  A reactive data.frame with the outcome table for the target of
  interest

## Value

The server to the prediction incidence module

## Details

The user specifies the id for the module

## See also

Other Characterization:
[`characterizationHelperFile()`](characterizationHelperFile.md),
[`characterizationIncidenceViewer()`](characterizationIncidenceViewer.md),
[`characterizationServer()`](characterizationServer.md),
[`characterizationViewer()`](characterizationViewer.md)
