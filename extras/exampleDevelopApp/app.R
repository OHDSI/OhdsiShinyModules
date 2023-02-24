##
# the following is an example app for developing with live test data

# Change parameters as you see need
# Required packages - devtools, githib.com/OHDSI/ShinyAppBuilder
# use devtools::install() to get required package from OhdsiShinyModules
##


# Dev tools will automatically load the R source
devtools::load_all()

#####
# Change these to match your test environment.
# Do not commit these lines
#####
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'postgresql',
  server = "test.com",
  user = "test",
  password = "findMe"
)

# Set to schema you are using
testSchema <- "test_schema"

config <- ShinyAppBuilder::initializeModuleConfig() |>
  
  # about module
  ShinyAppBuilder::addModuleConfig(
    ShinyAppBuilder::createDefaultAboutConfig(useKeyring = T,
                                              resultDatabaseDetails = list())
  ) |>
  
  # cohort generator module
  ShinyAppBuilder::addModuleConfig(
    ShinyAppBuilder::createDefaultCohortGeneratorConfig(useKeyring = T,
                                                        resultDatabaseDetails = list(dbms = "postgresql",                                                                                 dbms = "postgresql",
                                                                                     tablePrefix = "cg_",
                                                                                     cohortTablePrefix = "cg_",
                                                                                     databaseTablePrefix = "cg_",
                                                                                     incidenceTablePrefix = "ci_",
                                                                                     schema = testSchema))
  ) |>
  
  # cohort diagnostics module
  ShinyAppBuilder::addModuleConfig(
    ShinyAppBuilder::createDefaultCohortDiagnosticsConfig(useKeyring = T,
                                                          resultDatabaseDetails = list(dbms = "postgresql",
                                                                                       tablePrefix = "cd_",
                                                                                       vocabularyDatabaseSchema = testSchema,
                                                                                       schema = testSchema))
  ) |>
  
  # cohort characterization module
  ShinyAppBuilder::addModuleConfig(
    ShinyAppBuilder::createDefaultCharacterizationConfig(useKeyring = T,
                                                         resultDatabaseDetails = list(dbms = "postgresql",
                                                                                      tablePrefix = "c_",
                                                                                      cohortTablePrefix = "cg_",
                                                                                      databaseTablePrefix = "cd",
                                                                                      schema = testSchema,
                                                                                      databaseTable = "database_meta_data",
                                                                                      incidenceTablePrefix = "ci_"))

    # Add other modules here from ohdsi-shiny modules
    # Use ShinyAppBuilder::addModuleConfig(  ShinyAppBuilder::createModuleConfig() for new modules not added to package
  )

# Note the use of a ConnectionHandler rather than pooled connections is better in test settings
# Use ResultModelManager::PooledConnectionHandler$new in a multi-user environment
connection <- ResultModelManager::ConnectionHandler$new(connectionDetails)
ShinyAppBuilder::viewShiny(config, connection)
