# create a config settings for the shiny that includes
# the tabs that are necessary for the analysis

#remotes::install_github('ohdsi/OhdsiShinyModules')

options(java.parameters = '-Xss100m')

schema <- 'ase_026'

library(dplyr)
library(ShinyAppBuilder)
library(OhdsiShinyModules)

#Sys.setenv("DATABASECONNECTOR_JAR_FOLDER"= "./")

# if (!any(grepl("postgresql", list.files("./", pattern = ".jar")))) {
#   DatabaseConnector::downloadJdbcDrivers("postgresql")
# }

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
    createDefaultCharacterizationConfig()
  ) %>%
  addModuleConfig(
    createDefaultPredictionConfig()
  ) %>%
  addModuleConfig(
    createDefaultCohortMethodConfig()
  ) %>%
  addModuleConfig(
    createDefaultSccsConfig()
  ) %>%
  addModuleConfig(
    createDefaultEvidenceSynthesisConfig()
  )

keyringName = 'ohda'
#password for ohda = "7^Lkm#Ib-3k|V7B"
Sys.setenv(resultsAdmin = "ohda_project_sa")
Sys.setenv(resultsAdminPassword = "B500pkk=en-3")
Sys.setenv(resultsDbConnectionString = "jdbc:postgresql://ohda-results.cterqq54xyuu.us-east-1.rds.amazonaws.com:5432/ohda_results")
# Store the results database connection info -----
# keyring::key_set_with_value("resultsAdmin", password = "ohda_project_sa", keyring = keyringName)
# keyring::key_set_with_value("resultsAdminPassword", password = "B500pkk=en-3", keyring = keyringName)
# keyring::key_set_with_value("resultsReadOnlyUser", password = "ohda_project_ro", keyring = keyringName)
# keyring::key_set_with_value("resultsReadOnlyPassword", password = "O9gFaP=YTO6v", keyring = keyringName)
# resultsDbConnectionString <- "jdbc:postgresql://ohda-results.cterqq54xyuu.us-east-1.rds.amazonaws.com:5432/ohda_results"
# keyring::key_set_with_value("resultsServer", password = resultsDbConnectionString, keyring = keyringName)
# connectionDetails = DatabaseConnector::createConnectionDetails(
#   dbms = "postgresql",
#   connectionString = resultsDbConnectionString,
#   user = keyring::key_get("resultsAdmin", keyring = keyringName),
#   password = keyring::key_get("resultsAdminPassword", keyring = keyringName)
# )

# specify the connection to the results database
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'postgresql', 
  user = Sys.getenv("resultsAdmin"), 
  password = Sys.getenv("resultsAdminPassword"), 
  connectionString = Sys.getenv("resultsDbConnectionString")
)

# create a connection handler using the ResultModelManager package
connection <- ResultModelManager::ConnectionHandler$new(connectionDetails)



# create result schema settings
resultDatabaseSettings <- createDefaultResultDatabaseSettings(
  schema = schema
)

# now create the shiny app based on the config file and view the results
# based on the connection 
##ShinyAppBuilder::createShinyApp(config = config, connection = connection)
ShinyAppBuilder::createShinyApp(
  config = config, 
  connection = connection, 
  resultDatabaseSettings = resultDatabaseSettings,
  title = "My Study",
  studyDescription = "This is a sample description about my study. Woohoo!"
)
