#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(OhdsiShinyModules)
library(shiny)

if (FALSE)
  RSQLite::rsqliteVersion()

ui <- fluidPage(

  # Application title
  titlePanel("Big table example"),
  largeTableView("tblView")

)
###--- Fill in connection details with a real db ---###
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = ":memory:")
ch <- ResultModelManager::ConnectionHandler$new(connectionDetails)
# 1 million random rows
bigData <- data.frame(row_id = 1:1e6, value = stats::runif(1e6))
DatabaseConnector::insertTable(ch$getConnection(), data = bigData, tableName = "big_table")

server <- function(input, output) {

  baseQuery <- "SELECT * FROM main.big_table WHERE row_id >= @min_row"
  countQuery <- "SELECT count(*) FROM main.big_table WHERE row_id >= @min_row"

  ldt <- createLargeSqlQueryDt(connectionHandler = connectionHandler,
                               baseQuery = baseQuery,
                               countQuery = countQuery)

  largeTableServer(id = "tblView",
                   ldt = ldt,
                   inputParams =  reactive(list(min_row = 1)),
                   columns = list(
                     "rowId" = reactable::colDef(name = "row id")
                   ))
}

# Run the application
shinyApp(ui = ui, server = server)
