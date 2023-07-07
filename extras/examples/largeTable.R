#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(OhdisShinyModules)
library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("Big table example"),
           largeTableView("tblView")

)
###--- Fill in connection details with a real db ---###
connectionDetails <- DatabaseConnector::createConnectionDetails()

ch <- ResultModelManager::ConnectionHandler$new(connectionDetails = connectionDetails)
# Define server logic required to draw a histogram
server <- function(input, output) {
  # Set to schema you are using
  testSchema <- "ase_004"

  baseQuery <- "SELECT * FROM @test_schema.cd_temporal_covariate_value"
  countQuery <- "SELECT count(*) as count FROM @test_schema.cd_temporal_covariate_value"

  ldt <- LargeDataTable$new(ch,
                            baseQuery,
                            countQuery = countQuery,
                            columnDefs = list(
                              "cohortId" = reactable::colDef(name = "cohort id")
                            ))

  largeTableServer("tblView", ldt, reactive(list(test_schema = testSchema)))
}

# Run the application
shinyApp(ui = ui, server = server)
