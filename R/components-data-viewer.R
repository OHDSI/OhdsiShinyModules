

#inputs: data, named list of colDef options, where name is name of each column,
#potentially:
#output: download buttons, table, and column selector


#' Result Table Viewer
#'
#' @param id string
#' @param downloadedFileName string, desired name of downloaded data file. can use the name from the module that is being used
#'
#' @return shiny module UI
#'
resultTableViewer <- function(
    id = "result-table",
    downloadedFileName = NULL,
    boxTitle = 'Table'
    ) {
  ns <- shiny::NS(id)
  shiny::div(# UI
    shinydashboard::box(
      width = "100%",
      title = shiny::span(shiny::icon("table"), boxTitle),
      shiny::fluidPage(
        shiny::fluidRow(
          shiny::column(
            width = 7,
            shiny::uiOutput(ns("columnSelector"))
            ),
          shiny::column(
            width = 2,
            shiny::downloadButton(
              ns('downloadDataFull'),
              label = "Download (Full)",
              icon = shiny::icon("download")
            )
          ),
          shiny::column(
            width = 3,
            shiny::actionButton(
              ns('downloadDataFiltered'),
              label = "Download (Filtered)",
              icon = shiny::icon("download"),
              onclick = paste0(
                "Reactable.downloadDataCSV('",
                ns('resultData'),
                "', 'result-data-filtered-",
                downloadedFileName,
                Sys.Date(),
                ".csv')"
              )
            )
          )
        ),
        shiny::fluidRow(
          shinycssloaders::withSpinner(
            reactable::reactableOutput(
              outputId = ns("resultData"),
              width = "100%")
            )
        )
      )
    ))
}




#tooltip function
withTooltip <- function(value, tooltip, ...) {
  shiny::div(
    style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
    tippy::tippy(value, tooltip, ...)
    )
}

# customColDefs needs to be named list of colDefs
# example usage:
# Define custom colDefs for the Name and Age columns
# custom_colDefs <- list(
#   mpg = reactable::colDef(align = "left",
#                format = reactable::colFormat(digits = 2),
#                header = withTooltip("MPG column name", "MPG tooltip")),
#   disp = reactable::colDef(align = "center",
#                 header = withTooltip("Disp column name", "Disp tooltip"))
# )


create_colDefs_list <- function(
    df, 
    customColDefs = NULL
    ) {
  # Get the column names of the input data frame
  col_names <- colnames(df)
  
  # Create an empty list to store the colDefs
  colDefs_list <- vector("list", length = length(col_names))
  names(colDefs_list) <- col_names
  
  # Define custom colDefs for each column if provided
  if (!is.null(customColDefs)) {
    for (col in seq_along(col_names)) {
      if (col_names[col] %in% names(customColDefs)) {
        colDefs_list[[col]] <- customColDefs[[col_names[col]]]
      } else {
        colDefs_list[[col]] <- reactable::colDef(name = col_names[col])
      }
      
      if (!is.null(customColDefs[[col_names[col]]]$header)) {
        colDefs_list[[col]]$header <- customColDefs[[col_names[col]]]$header
      }
      
      if (!is.null(customColDefs[[col_names[col]]]$tooltip)) {
        colDefs_list[[col]]$header <-
          withTooltip(colDefs_list[[col]]$header, customColDefs[[col_names[col]]]$tooltip)
      }
    }
  } else {
    # Define default colDefs if customColDefs is not provided
    for (col in seq_along(col_names)) {
      colDefs_list[[col]] <- reactable::colDef(name = col_names[col])
    }
  }
  
  # Return the list of colDefs
  return(colDefs_list)
}

ohdsiReactableTheme <- reactable::reactableTheme(
  color = "white",
  backgroundColor = "#003142",
  stripedColor = "#333333",
  highlightColor = "#f19119",
  style = list(
    fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI,
    Roboto, Helvetica, Arial, sans-serif, Apple Color Emoji, 
    Segoe UI Emoji, Segoe UI Symbol"
  )
  #,
  #headerStyle = list(
  #)
)



#' Result Table Server
#'
#' @param id string, table id must match resultsTableViewer function
#' @param df reactive that returns a data frame
#' @param colDefsInput named list of reactable::colDefs
#' @param selectedCols string vector of columns the reactable should display to start by default. Defaults to ALL if not specified.
#' @param sortedCols string vector of columns the reactable should sort by by default. Defaults to no sort if not specified.
#' @param elementId optional string vector of element Id name for custom dropdown filtering if present in the customColDef list. Defaults to NULL.
#' @param downloadedFileName string, desired name of downloaded data file. can use the name from the module that is being used
#' @param addActions add a button row selector column to the table to a column called 'actions'.  
#'                   actions must be a column in df
#' @param downloadedFileName string, desired name of downloaded data file. can use the name from the module that is being used
#' @param groupBy The columns to group by 
#'
#' @return shiny module server
#'
resultTableServer <- function(
    id, #string
    df, #data.frame
    colDefsInput,
    selectedCols = NULL,
    sortedCols = NULL,
    elementId = NULL,
    addActions = NULL,
    downloadedFileName = NULL,
    groupBy = NULL
) #list of colDefs, can use checkmate::assertList, need a check that makes sure names = columns) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # convert a data.frame to a reactive
      if(!inherits(df, 'reactive')){
        df <- shiny::reactiveVal(df)
      }
      
      # initialize the reactables
      actionCount <- shiny::reactiveVal(0)
      actionIndex <- shiny::reactiveVal(0)
      actionType <- shiny::reactiveVal('none')
        
      # add action column to data
      newdf <- shiny::reactive({
        if(!is.null(nrow(df())) & !is.null(addActions)){
          cbind(
            actions = rep("", nrow(df())),
            df()
          )} else{
            df()
          }
      })
      
      selectedColumns <- shiny::reactive({
        if(!is.null(selectedCols)){
          intersect(colnames(newdf()), selectedCols)
        }
        else{
            colnames(newdf())
        }
      })
      
      sortedColumns <- shiny::reactive({
        if(!is.null(sortedCols)){
          sortedCols
        }
        else{
          NULL
        }
      })
      
      elementIdName <- shiny::reactive({
        if(!is.null(elementId)){
          elementId
        }
        else{
          NULL
        }
      })
      
      # add a new entry to colDefs with an action dropdown menu
      # add a onClick action
      if(!is.null(addActions)){
        
        onClickText <- paste0(
          "function(rowInfo, column) {",
          paste("if(column.id == 'actions'){
      Shiny.setInputValue('",session$ns(paste0('action_index')),"', { index: rowInfo.index + 1 }, { priority: 'event' })
    }", collapse = ' ', sep = ''),
          "}"
        )
        onClick <- reactable::JS(onClickText)
        
        colDefsInput <- addTableActions(
          colDefsInput = colDefsInput,
          addActions = addActions,
          session = session
          )
      
      } else{
        onClick <- NULL
      }
      

      output$columnSelector <- shiny::renderUI({
        
        shinyWidgets::pickerInput(
          inputId = session$ns('dataCols'),
          label = 'Select Columns to Display: ',
          choices = colnames(newdf()),
          selected = selectedColumns(),
          choicesOpt = list(style = rep_len("color: black;", 999)),
          multiple = T,
          options = shinyWidgets::pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            size = 10,
            liveSearchStyle = "contains",
            liveSearchPlaceholder = "Type here to search",
            virtualScroll = 50
          ),
          width = "75%"
        )
        
      })

      
      #need to try adding browser() to all reactives to see why selected cols isnt working
      
      colDefs <- shiny::reactive(
        if(!is.null(newdf())){
          create_colDefs_list(
            df = newdf()[, input$dataCols],
            customColDefs = colDefsInput
          )
        } else{
          NULL
        }
      )
      
      js_code <- "
// Custom range filter with value label
function rangeFilter(column, state) {
  // Get min and max values from raw table data
  let min = Infinity;
  let max = 0;
  state.data.forEach(function(row) {
    const value = row[column.id];
    if (value < min) {
      min = Math.floor(value);
    } else if (value > max) {
      max = Math.ceil(value);
    }
  });

  const filterValue = column.filterValue || min;
  const input = React.createElement('input', {
    type: 'range',
    value: filterValue,
    min: min,
    max: max,
    onChange: function(event) {
      // Set to undefined to clear the filter
      column.setFilter(event.target.value || undefined);
    },
    style: { width: '100%', marginRight: '8px' },
    'aria-label': 'Filter ' + column.name
  });

  return React.createElement(
    'div',
    { style: { display: 'flex', alignItems: 'center', height: '100%' } },
    [input, filterValue]
  );
}

// Filter method that filters numeric columns by minimum value
function filterMinValue(rows, columnId, filterValue) {
  return rows.filter(function(row) {
    return row.values[columnId] >= filterValue;
  });
}
"
      output$resultData <- reactable::renderReactable({
          if (is.null(input$dataCols)) {
            data = newdf()
          }
          else{
            data = newdf()[, input$dataCols, drop = FALSE]
          }
        # Display message when dat is empty
        shiny::validate(shiny::need(hasData(data), "No data for selection"))
        # set row height based on nchar of table
        if(max(apply(data, 1, function(x) max(nchar(x))), na.rm = T) < 120){
          height <- 40*3
        } else{
          height <- NULL
        }
          
          reactable::reactable(
            data,
            columns = colDefs(),
            onClick = onClick,
            groupBy = groupBy,
            #these can be turned on/off and will overwrite colDef args
            sortable = TRUE,
            resizable = TRUE,
            filterable = TRUE,
            searchable = TRUE,
            showPageSizeOptions = TRUE,
            outlined = TRUE,
            showSortIcon = TRUE,
            striped = TRUE,
            highlight = TRUE,
            #defaultColDef = reactable::colDef(align = "left"),
            defaultSorted = sortedColumns(),
            rowStyle = list(
              height = height
              ),
            elementId = elementIdName()
            #, experimental
            #theme = ohdsiReactableTheme
          )
        })
      
      
      # download full data button
      output$downloadDataFull <- shiny::downloadHandler(
        filename = function() {
          paste('result-data-full-', downloadedFileName, Sys.Date(), '.csv', sep = '')
        },
        content = function(con) {
          utils::write.csv(
            x = df(), 
            file = con,
            row.names = F
          )
        }
      )
      
      # capture the actions
      shiny::observeEvent(input$action_index, {
        actionIndex(input$action_index)
      })
      
      shiny::observeEvent(input$action_type, {
          # update type
          actionType(input$action_type$value)
          # update count
          actionCount(input$action_type$seed)
        })
      
      return(
        list(
          actionType = actionType, 
          actionIndex = actionIndex,
          actionCount = actionCount 
        )
      )
    })


# HELPERS
addTableActions <- function(
    colDefsInput,
    addActions,
    session
){
  
  args <- list(
    label = "Actions",
    status = "primary",
    circle = FALSE,
    width = "300px",
    margin = "5px",
    inline = T
  )
  
  args <- append(
    args, 
    lapply(
    X = addActions,
    FUN = function(x){
      shiny::actionLink(
        inputId = session$ns(x),
        label = paste0('View ',x),
        icon = shiny::icon("play"),
        onClick = reactable::JS(
          paste0(
            "function() {
                  Shiny.setInputValue('",session$ns(paste0('action_type')),"', { value: '",x,"', seed: Math.random()})
                  }"
          )
        )
      )
    })
  )
  
  tableActionfunction <- function(){ 
  
  cellFunction <- do.call(
    args = args,
    what = shinyWidgets::dropdownButton
  )
  return(cellFunction)
  }
  
  # add the actions dropdown
  colDefsInput[[length(colDefsInput) + 1 ]] <- reactable::colDef(
    name = "",
    sortable = FALSE,
    filterable = FALSE,
    minWidth = 150,
    cell = tableActionfunction
  ) 
  names(colDefsInput)[length(colDefsInput)] <- 'actions'
  
  return(colDefsInput)
}
