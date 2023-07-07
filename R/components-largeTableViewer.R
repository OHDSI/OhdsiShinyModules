#' Large Data Table
#' @export
#' @description
#' Large data table R6 class.
#'
#' Uses ResultModelManager::ConnectionHandler class to create paginating tables
#'
#' NOTE Only currently works with sqlite and postgresql database backends (probably redshift too)
#' as this method uses limit and offset for the queries
#'
#' Alternatively, you might want to subclass this class. For example, if your backend query is against an API such
#' as and ATLAS instance or ATHENA
#'
#' @field baseQuery         query string sql
#' @field countQuery        count query string (should match query). Can be auto generated with sub query (default) but
#'                          this will likely result in slow results
#' @field columnDefs        reactable Coulmn definitions
#' @field connectionHandler ResultModelManager connection handler to execute query inside
LargeDataTable <- R6::R6Class(
  classname = "LargeDataTable",
  public = list(
    connectionHandler = NULL,
    baseQuery = NULL,
    countQuery = NULL,
    columnDefs = NULL,
    #' initialize
    #'
    #' @param connectionHandler         ResultModelManager connectionHandler instance
    #' @param baseQuery                 base sql query
    #' @param countQuery                count query string (should match query). Can be auto generated with sub query
    #'                                  (default) but this will likely result in slow results
    #' @param columnDefs                list of
    #'
    #' @return self
    #' @export
    #'
    #' @examples
    initialize = function(connectionHandler, baseQuery, countQuery = NULL, columnDefs = list()) {
      checkmate::assertR6(connectionHandler, "ConnectionHandler")
      checkmate::assertString(baseQuery)
      checkmate::assertString(countQuery, null.ok = TRUE)
      checkmate::assertList(columnDefs, null.ok = TRUE, types = "colDef")
      # Cannot use multiple statments in a base query
      stopifnot(length(strsplit(baseQuery, ";")) == 1)
      self$connectionHandler <- connectionHandler
      self$baseQuery <- baseQuery
      self$columnDefs <- columnDefs

      if (!is.null(countQuery)) {
        self$countQuery <- countQuery
      } else {
        self$countQuery <-  SqlRender::render("SELECT COUNT(*) as count FROM (@sub_query);",
                                              sub_query = self$baseQuery)
      }

      self
    },

    #' get column defs
    #'
    #' @return columnDefs
    getColumnDefs = function() {
      self$columnDefs
    },

    #' get count
    #' @description
    #' execute count query with specified parameters
    #' @param ...
    #'
    #' @return count
    getCount = function(...) {
      sql <- SqlRender::render(sql = self$countQuery, ...)
      count <- self$connectionHandler$queryDb(sql)
      return(count$count)
    },

    #' Get Page
    #'
    #' @param pageNum       page number
    #' @param pageSize      page size
    #' @param ...
    #'
    #' @return data.frame of query result
    getPage = function(pageNum, pageSize = self$pageSize, ...) {
      mainQuery <- SqlRender::render(sql = self$baseQuery, ...)

      pageOffset <- ((pageNum - 1) * pageSize) + 1
      self$connectionHandler$queryDb("@main_query LIMIT @page_size OFFSET @page_offset",
                                     main_query = mainQuery,
                                     page_size = pageSize,
                                     page_offset = pageOffset)
    },

    #' get all results
    #'
    #' @param ...
    #'
    #' @return data.frame of all results. Used for large file downloads
    getAllResults = function(...) {
      self$connectionHandler$queryDb(self$baseQuery, ...)
    }
  )
)

#' Large Table Component Viewer
#' @description
#' Componenet for results sets with many thousands of rows
#' More limited than other table components in terms of automatic handling of search and
#' filtering but will allow responsive apps
#' @export
#'
#' @param id    Shiny module id. Must match largeTableServer
#' @param pageSizeChoices    numeric selection options for pages
largeTableView <- function(id, pageSizeChoices = c(10,20,50,100)) {
  ns <- shiny::NS(id)
  checkmate::assertNumeric(pageSizeChoices)
  shiny::div(
    id = ns("display-table"),
    shiny::fluidRow(
      shiny::column(
        width = 9
      ),
      shiny::column(
        width = 3,
        shiny::selectInput(ns("pageSize"), choices = pageSizeChoices, label = "Page Size")
      )
    ),
    shinycssloaders::withSpinner(reactable::reactableOutput(ns("tableView"))),
    shiny::fluidRow(
      shiny::column(
        width = 1,
        shiny::actionButton(ns("previousButton"), label = "Previous")
      ),
      shiny::column(
        width = 1,
        shiny::actionButton(ns("nextButton"), label = "Next")
      ),
      shiny::column(
        width = 3,
        shiny::p(shiny::textOutput(ns("pageNumber")))
      )
    )
  )
}

#' Large Table Component Server
#' @description
#' Display large data tables in a consistent way - server side pagination for reactable objects
#' @export
#' @param id            Shiny module id. Must match Large Table Viewer
#' @param ldt           LargeDataTable instance
#' @param inputParams   reactive that returns list of parameters to be passed to ldt
#' @param modifyData    optional callback function that takes the data page, page number, page size as parameters
#'                      must return data.frame compatable instance
largeTableServer <- function(id,
                             ldt,
                             inputParams,
                             modifyData = NULL) {
  checkmate::assertR6(ldt, "LargeDataTable")
  checkmate::assertClass(inputParams, "reactive")
  shiny::moduleServer(id, function(input, output, session) {
    pageNum <- shiny::reactiveVal(1)
    pageSize <- shiny::reactive(as.integer(input$pageSize))

    pageCount <- shiny::reactive({
      count <- do.call(ldt$getCount, inputParams())
      ceiling(count/pageSize())
    })

    shiny::observeEvent(input$nextButton, pageNum(min(pageNum() + 1, pageCount())))
    shiny::observeEvent(input$previousButton, pageNum(max(pageNum() - 1, 1)))
    # Reset page on page size change
    shiny::observeEvent(input$pageSize, { pageNum(1) })

    output$pageNumber <- shiny::renderText(paste("Page", pageNum(), "of", format(pageCount(), big.mark = ",")))

    output$tableView <- reactable::renderReactable({
      params <- inputParams()
      checkmate::assertList(params)
      params$pageNum <- pageNum()
      params$pageSize <- pageSize()

      dataPage <- do.call(ldt$getPage, params)

      if (is.function(modifyData)) {
        dataPage <- dataPage %>% modifyData(pageNum(), pageSize())
      }

      reactable::reactable(dataPage,
                           columns = ldt$getColumnDefs(),
                           searchable = FALSE,
                           sortable = FALSE,
                           pagination = FALSE)
    })
  })
}