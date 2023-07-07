devtools::load_all()

config <- ShinyAppBuilder::initializeModuleConfig() |>
  
  # about module
  ShinyAppBuilder::addModuleConfig(
    ShinyAppBuilder::createDefaultAboutConfig(useKeyring = F,
                                              resultDatabaseDetails = list())
  ) |>
  
  # cohort generator module
  ShinyAppBuilder::addModuleConfig(
    ShinyAppBuilder::createDefaultCohortGeneratorConfig(useKeyring = F,
                                                        resultDatabaseDetails = list(dbms = "postgresql",                                                                                 dbms = "postgresql",
                                                                                     tablePrefix = "cg_",
                                                                                     cohortTablePrefix = "cg_",
                                                                                     databaseTablePrefix = "cg_",
                                                                                     incidenceTablePrefix = "ci_",
                                                                                     schema = "epi_1025"))
  ) |>
  
  # cohort diagnostics module
  ShinyAppBuilder::addModuleConfig(
    ShinyAppBuilder::createDefaultCohortDiagnosticsConfig(useKeyring = TRUE,
                                                          resultDatabaseDetails = list(dbms = "postgresql",
                                                                                       tablePrefix = "cd_",
                                                                                       vocabularyDatabaseSchema = "epi_1025",
                                                                                       schema = "epi_1025"))
  ) |>
  
  # cohort characterization module
  ShinyAppBuilder::addModuleConfig(
    ShinyAppBuilder::createDefaultCharacterizationConfig(useKeyring = F,
                                                         resultDatabaseDetails = list(dbms = "postgresql",
                                                                                      tablePrefix = "c_",
                                                                                      cohortTablePrefix = "cg_",
                                                                                      databaseTablePrefix = "cd",
                                                                                      schema = "epi_1025",
                                                                                      databaseTable = "database_meta_data",
                                                                                      incidenceTablePrefix = "ci_"))
  )


connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql", 
                                                                server = "reward.cterqq54xyuu.us-east-1.rds.amazonaws.com/strategus_test",
                                                                user = "reward_user",
                                                                password = "4GAnEA~m6-Hk",
                                                                port = 5432)

connection <- ResultModelManager::ConnectionHandler$new(connectionDetails)
ShinyAppBuilder::viewShiny(config, connection)
