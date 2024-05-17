#remotes::install_github('ohdsi/OhdsiShinyModules', ref = 'develop')
#remotes::install_github("ohdsi/ShinyAppBuilder", ref = "develop")
library(dplyr)
library(ShinyAppBuilder) # need to install if you do not have it 
library(markdown)

options(java.parameters = "-Xss5m")

# makes sure you have the database connection driver folder set up
Sys.setenv(DATABASECONNECTOR_JAR_FOLDER = './drivers')
if(!dir.exists('./drivers')){
  dir.create('./drivers')
  DatabaseConnector::downloadJdbcDrivers(
    dbms = 'postgresql',
    pathToDriver = './drivers'
    )
}

# connection details to an example sqlite database in the package
connectionDetails <- OhdsiShinyModules::getExampleConnectionDetails()
schema <- "main"

est <- ShinyAppBuilder::createModuleConfig(
  moduleId = 'estimation', 
  tabName = 'Estimation', 
  shinyModulePackage = 'OhdsiShinyModules', 
  moduleUiFunction = 'estimationViewer', 
  moduleServerFunction = 'estimationServer', 
  moduleInfoBoxFile = 'esimationHelperFile()', 
  moduleIcon = 'list'
    )

# Specify the config - create a new one and then add 
# each shiny module you want to include
config <- initializeModuleConfig() %>%
  addModuleConfig(
    createDefaultAboutConfig()
  )  %>%
  addModuleConfig(
    createDefaultDatasourcesConfig()
  )  %>%
  addModuleConfig(
    createDefaultCohortGeneratorConfig()
  ) %>%
  addModuleConfig(
    createDefaultCohortDiagnosticsConfig()
  ) %>%
  addModuleConfig(
    est 
  )  %>%
  addModuleConfig(
    createDefaultCharacterizationConfig()
  ) %>%
  addModuleConfig(
    createDefaultPredictionConfig()
  ) %>%
  addModuleConfig(
    ShinyAppBuilder::createDefaultReportConfig()
    )

# create result schema settings
resultDatabaseSettings <- createDefaultResultDatabaseSettings(
  schema = schema 
)

# now create the shiny app based on the config file and view the results
# based on the connection
ShinyAppBuilder::createShinyApp(
  config = config,
  connectionDetails = connectionDetails,
  usePooledConnection = T,
  resultDatabaseSettings = resultDatabaseSettings, 
  title = 'Testing OhdsiShinyModules with ShinyAppBuilder',
  protocolLink = 'http://ohdsi.org'
)

