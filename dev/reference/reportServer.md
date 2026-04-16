# The module server for the shiny app report module

The module server for the shiny app report module

## Usage

``` r
reportServer(
  id = "reportModule",
  connectionHandler = NULL,
  resultDatabaseSettings = NULL,
  server = Sys.getenv("RESULTS_SERVER"),
  username = Sys.getenv("RESULTS_USER"),
  password = Sys.getenv("RESULTS_PASSWORD"),
  dbms = Sys.getenv("RESULTS_DBMS")
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

- server:

  server for the connection to the results for quarto

- username:

  username for the connection to the results for quarto

- password:

  password for the connection to the results for quarto

- dbms:

  dbms for the connection to the results for quarto

## Value

The server for the shiny app home

## Details

The user specifies the id for the module

## See also

Other Report: [`reportViewer()`](reportViewer.md)
