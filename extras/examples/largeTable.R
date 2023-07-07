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

  ldt <- LargeDataTable$new(ch,
                            baseQuery,
                            countQuery = countQuery,
                            columnDefs = list(
                              "rowId" = reactable::colDef(name = "row id")
                            ))

  largeTableServer("tblView", ldt, reactive(list(min_row = 1)))
}

# Run the application
shinyApp(ui = ui, server = server)
