# The module server for the shiny app home

The module server for the shiny app home

## Usage

``` r
aboutServer(
  id = "homepage",
  connectionHandler = NULL,
  resultDatabaseSettings = NULL,
  config
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

- config:

  the config from the app.R file that contains a list of which modules
  to include

## Value

The server for the shiny app home

## Details

The user specifies the id for the module

## See also

Other About: [`aboutHelperFile()`](aboutHelperFile.md),
[`aboutViewer()`](aboutViewer.md)
