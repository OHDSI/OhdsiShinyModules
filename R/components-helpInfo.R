infoHelperViewer <- function(
    id = "helper",
    helpLocation= system.file("datasources-www", "datasources.html", package = utils::packageName())
    ) {
  ns <- shiny::NS(id)
  
shinydashboard::box(
  collapsible = TRUE,
  collapsed = TRUE,
  title = shiny::span( shiny::icon("circle-question"), "Help & Information"),
  width = "100%",
  shiny::htmlTemplate(helpLocation)
)
}
