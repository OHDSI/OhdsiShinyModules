#remotes::install_github('ohdsi/OhdsiReportGenerator', ref = 'develop')
#remotes::install_github("ohdsi/OhdsiShinyAppBuilder")
library(dplyr)
library(OhdsiShinyAppBuilder) # need to install if you do not have it 
library(markdown)

options(java.parameters = "-Xss5m")

# makes sure you have the database connection driver folder set up
Sys.setenv(DATABASECONNECTOR_JAR_FOLDER = file.path(getwd(),'drivers'))
if(!dir.exists(file.path(getwd(),'drivers'))){
  dir.create(file.path(getwd(),'drivers'))
  DatabaseConnector::downloadJdbcDrivers(
    dbms = 'postgresql',
    pathToDriver = file.path(getwd(),'drivers')
    )
}

# connection details to an example sqlite database in the package
connectionDetails <- OhdsiReportGenerator::getExampleConnectionDetails()
schema <- "main"

#Sys.setenv(RESULTS_SERVER = system.file("extdata", "results.sqlite", package = "OhdsiShinyModules"))
#Sys.unsetenv('RESULTS_USER')
#Sys.unsetenv('RESULTS_PASSWORD')
#Sys.setenv(RESULTS_DBMS = "sqlite")

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
    createDefaultEstimationConfig()
  )  %>%
  addModuleConfig(
    createDefaultCharacterizationConfig()
  ) %>%
  addModuleConfig(
    createDefaultPredictionConfig()
  ) %>%
  addModuleConfig(
    createDefaultReportConfig()
    )

# create result schema settings
resultDatabaseSettings <- createDefaultResultDatabaseSettings(
  schema = schema
)

# now create the shiny app based on the config file and view the results
# based on the connection
Sys.setenv(RESULTS_SERVER=connectionDetails$server())
Sys.setenv(RESULTS_USER='')
Sys.setenv(RESULTS_PASSWORD='')
Sys.setenv(RESULTS_DBMS="sqlite")

OhdsiShinyAppBuilder::createShinyApp(
  config = config,
  connectionDetails = connectionDetails,
  usePooledConnection = TRUE,
  resultDatabaseSettings = resultDatabaseSettings, 
  title = 'Testing OhdsiShinyModules with ShinyAppBuilder',
  protocolLink = 'http://ohdsi.org'
)
