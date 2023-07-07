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
      stopifnot(length(strsplit(baseQuery, ";")[[1]]) == 1)
      self$connectionHandler <- connectionHandler
      self$baseQuery <- baseQuery
      self$columnDefs <- columnDefs

      if (!is.null(countQuery)) {
        stopifnot(length(strsplit(countQuery, ";")[[1]]) == 1)
        self$countQuery <- countQuery
      } else {
        self$countQuery <-  SqlRender::render("SELECT COUNT(*) as count FROM (@sub_query);", sub_query = self$baseQuery)
      }
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

      pageOffset <- ((pageNum - 1) * pageSize)
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
#' @param selectedPageSize   numeric selection options for pages
#' @param fullDownloads     allow download button of full dataset from query
largeTableView <- function(id, pageSizeChoices = c(10,25,50,100), selectedPageSize = 10, fullDownloads = TRUE) {
  ns <- shiny::NS(id)
  checkmate::assertNumeric(pageSizeChoices, min.len = 1, finite = TRUE, lower = 1)
  checkmate::assertTRUE(selectedPageSize %in% pageSizeChoices)

  inlineStyle <- ".inline-tv label{ display: table-cell; text-align: center; vertical-align: middle; padding-right: 1em; }
                  .inline-tv .selectize-input { min-width:70px;}
                 .inline-tv .form-group { display: table-row;}"

  shiny::div(
    id = ns("display-table"),
    shiny::tags$head(
      shiny::tags$style(type = "text/css", inlineStyle)
    ),
     shiny::fluidRow(
       if (fullDownloads) {
         shiny::column(
           width = 4,
           shiny::downloadButton(ns("downloadFull"),
                                 label = "Download (Full)",
                                 icon = shiny::icon("download"))
         )
       }
     ),
    shinycssloaders::withSpinner(reactable::reactableOutput(ns("tableView"))),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        class = "inline-tv",
        shiny::textOutput(ns("pageNumber")),
        shiny::selectInput(ns("pageSize"),
                           choices = pageSizeChoices,
                           selected = selectedPageSize,
                           label = "show",
                           width = "90px")
      ),
      shiny::column(
        width = 2
      ),
      shiny::column(
        width = 6,
        style = "text-align:right;",
        shiny::uiOutput(ns("pageActionButtons"))
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

  if (!is.list(inputParams))
    checkmate::assertClass(inputParams, "reactive")

  shiny::moduleServer(id, function(input, output, session) {

    if (is.list(inputParams)) {
      realParams <- inputParams
      inputParams <- shiny::reactive(realParams)
    }

    ns <- session$ns
    pageNum <- shiny::reactiveVal(1)
    pageSize <- shiny::reactive(as.integer(input$pageSize))

    rowCount <- shiny::reactive({
      do.call(ldt$getCount, inputParams())
    })

    pageCount <- shiny::reactive({
      ceiling(rowCount()/pageSize())
    })

    shiny::observeEvent(input$nextButton, pageNum(min(pageNum() + 1, rowCount())))
    shiny::observeEvent(input$previousButton, pageNum(max(pageNum() - 1, 1)))

    shiny::observeEvent(input$pageNum, pageNum(input$pageNum))

    # Reset page on page size change
    shiny::observeEvent(input$pageSize, { pageNum(1) })

    output$pageNumber <- shiny::renderText({

      minNum <- format(((pageNum() - 1) * pageSize()) + 1, big.mark = ",", scientific = FALSE)
      maxNum <- format((pageNum() - 1)  * pageSize() + pageSize(), big.mark = ",", scientific = FALSE)
      rc <- format(rowCount(), big.mark = ",", scientific = FALSE)
      paste(minNum, "-", maxNum, "of", rc, "rows")
    })

    dataPage <- shiny::reactive({
      params <- inputParams()
      checkmate::assertList(params)
      params$pageNum <- pageNum()
      params$pageSize <- pageSize()

      dataPage <- do.call(ldt$getPage, params)

      if (is.function(modifyData)) {
        dataPage <- dataPage %>% modifyData(pageNum(), pageSize())
      }
      dataPage
    })

    output$tableView <- reactable::renderReactable({
      reactable::reactable(dataPage(),
                           columns = ldt$getColumnDefs(),
                           searchable = FALSE,
                           sortable = FALSE,
                           resizable = TRUE,
                           outlined = TRUE,
                           showSortIcon = TRUE,
                           striped = TRUE,
                           highlight = TRUE,
                           defaultColDef = reactable::colDef(align = "left"),
                           pagination = FALSE)
    })

    output$pageActionButtons <-  shiny::renderUI({
      pc <- pageCount()
      if (pc == 1) {
        return(shiny::span(style="width:80px;", shiny::HTML("&nbsp;")))
      }

      createPageLink <- function(pageLink) {
        js <- sprintf("Shiny.setInputValue('%s', %s);", ns("pageNum"), pageLink)
        shiny::tags$a(onclick = js, style = "cursor:pointer;", format(pageLink, big.mark = ",", scientific = FALSE))
      }

      linkNums <- unique(c(1, max(2, pageNum() - 2):min(pageCount() - 1, pageNum() + 3)))
      links <- lapply(linkNums, createPageLink)

      ## render action buttons
      # Always show 1
      # Show row up to 5
      # Always show max
      shiny::tagList(
        shiny::actionLink(ns("previousButton"), label = "Previous"),
        links,
        if (pageCount() != pageNum()) {
          shiny::span("...")
        },
        createPageLink(pageCount()),
        shiny::actionLink(ns("nextButton"), label = "Next")
      )
    })

    output$downloadFull <- shiny::downloadHandler(
      filename = function() {
        paste0('result-data-full-', id, Sys.Date(), '.csv')
      },
      content = function(con) {
        shiny::withProgress(
          message = "preparing download",
          value = 15,
          readr::write_csv(do.call(ldt$getAllResults, inputParams()), con)
        )
      }
    )
  })
}