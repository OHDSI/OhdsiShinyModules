# Result Table Server

Result Table Server

## Usage

``` r
resultTableServer(
  id = "result-table",
  df,
  colDefsInput = NULL,
  columnGroups = NULL,
  details = data.frame(),
  selectedCols = NULL,
  elementId = NULL,
  addActions = NULL,
  downloadedFileName = NULL,
  groupBy = NULL,
  selection = NULL,
  getSelected = shiny::reactiveVal(0),
  setSelected = shiny::reactiveVal(0),
  selectedRowId = shiny::reactiveVal(0),
  showPageSizeOptions = TRUE,
  pageSizeOptions = c(10, 25, 50, 500),
  defaultPageSize = 10
)
```

## Arguments

- id:

  string, table id must match resultsTableViewer function

- df:

  reactive that returns a data frame

- colDefsInput:

  named list of reactable::colDefs

- columnGroups:

  list specifying how to group columns

- details:

  The details of the results such as cohort names and database names
  used when downloading table

- selectedCols:

  string vector of columns the reactable should display to start by
  default. Defaults to ALL if not specified.

- elementId:

  optional string vector of element Id name for custom dropdown
  filtering if present in the customColDef list. Defaults to NULL.

- addActions:

  add a button row selector column to the table to a column called
  'actions'. actions must be a column in df

- downloadedFileName:

  string, desired name of downloaded data file. can use the name from
  the module that is being used

- groupBy:

  The columns to group by

- selection:

  NULL/single/multiple (whether to enable table row selection)

- getSelected:

  A reactive that triggers an even to extract the selected rows of the
  table

- setSelected:

  A reactive that triggers an even to set the selected rows of the table

- selectedRowId:

  The selected rows

- showPageSizeOptions:

  Show page size options?

- pageSizeOptions:

  Page size options for the table. Defaults to 10, 25, 50, 100.

- defaultPageSize:

  Default page size for the table. Defaults to 10.

## Value

shiny module server
