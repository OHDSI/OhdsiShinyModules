options("shiny-test-env-enabled" = TRUE)
jarFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER", unset = "")
if (jarFolder == "") {
  tempJarFolder <- tempfile("jdbcDrivers")
  dir.create(tempJarFolder)
  Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = tempJarFolder)
  DatabaseConnector::downloadJdbcDrivers("postgresql")
  
  withr::defer({
    unlink(tempJarFolder, recursive = TRUE, force = TRUE)
    Sys.unsetenv("DATABASECONNECTOR_JAR_FOLDER")
  }, testthat::teardown_env())
}

dbmsTest <- 'sqlite'
schemaTest <- 'main'

# =========== CG START
cohortTablePrefix <- 'cg_'

connectionDetailsCG <- DatabaseConnector::createConnectionDetails(
  server = "../resources/cgDatabase/databaseFile.sqlite",
  dbms = 'sqlite'
)
connectionHandlerCG <- ResultModelManager::ConnectionHandler$new(connectionDetailsCG, loadConnection = FALSE)

# =========== CG START
  
# =========== PLP START
serverPlp <- "../resources/plpDatabase/databaseFile.sqlite"
connectionDetailsPlp <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = serverPlp
)

connectionHandlerPlp <- ResultModelManager::ConnectionHandler$new(connectionDetailsPlp, loadConnection = FALSE)

resultDatabaseSettingsPlp <- list(
  dbms = 'sqlite', # should this be removed - can use connection
  plpTablePrefix = '',
  cohortTablePrefix = '',
  databaseTablePrefix = '',
  schema = 'main'
)
# =========== PLP End



# =========== characterization START
serverCharacterization <- "../resources/characterizationDatabase/databaseFile.sqlite"
connectionDetailsCharacterization <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = serverCharacterization
)

connectionHandlerCharacterization <- ResultModelManager::ConnectionHandler$new(
  connectionDetailsCharacterization, 
  loadConnection = FALSE
  )

resultDatabaseSettingsCharacterization <- list(
  dbms = 'sqlite', # should this be removed - can use connection
  tablePrefix = 'c_',
  cohortTablePrefix = 'cg_',
  databaseTablePrefix = '',
  schema = 'main',
  databaseTable = 'DATABASE_META_DATA',
  incidenceTablePrefix = 'i_',
  tempEmulationSchema = NULL
)


# =========== Characterization END


# =========== Cohort Method START
connectionDetailsCm <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite', 
  server = "../resources/cmDatabase/databaseFile.sqlite"
)

connectionHandlerCm  <- ResultModelManager::ConnectionHandler$new(
  connectionDetailsCm, 
  loadConnection = FALSE
  )

resultDatabaseSettingsCm <- list(
  dbms = 'sqlite',
  tablePrefix = 'cm_',
  cohortTablePrefix = 'cg_',
  databaseTable = 'DATABASE_META_DATA',
  schema = "main",
  tempEmulationSchema = NULL
)

# =========== Cohort Method END


# =========== Data diag START
connectionDetailsDataDiag <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite', 
  server = "../resources/datadiagDatabase/databaseFile.sqlite"
)

connectionHandlerDataDiag <- ResultModelManager::ConnectionHandler$new(connectionDetailsDataDiag, loadConnection = FALSE)

resultDatabaseSettingsDataDiag <- list(
  dbms = 'sqlite',
  tablePrefix = '',
  schema = "main"
)

# =========== Data diag End


# =========== Cohort Diagnostics
connectionDetailsCohortDiag <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = "../resources/cdDatabase/databaseFile.sqlite"
)

resultDatabaseSettingsCohortDiag <- list(
  dbms = 'sqlite',
  tablePrefix = '',
  schema = "main",
  cohortTableName = "cohort",
  databaseTableName = "database"
)

connectionHandlerCohortDiag <- ResultModelManager::ConnectionHandler$new(connectionDetailsCohortDiag, loadConnection = FALSE)

dataSourceCd <-
  createCdDatabaseDataSource(
    connectionHandler = connectionHandlerCohortDiag,
    schema = "main",
    vocabularyDatabaseSchema = "main",
    tablePrefix = "",
    cohortTableName = "cohort",
    databaseTableName = "database",
    displayProgress = FALSE
  )

#  ======

# ====== Sccs

connectionDetailsSccs <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = "../resources/sccsDatabase/databaseFile.sqlite"
)

connectionHandlerSccs  <- ResultModelManager::ConnectionHandler$new(connectionDetailsSccs, loadConnection = FALSE)

resultDatabaseSettingsSccs <- list(
  dbms = 'sqlite',
  tablePrefix = 'sccs_',
  cohortTablePrefix = 'cg_',
  databaseTable = 'DATABASE_META_DATA',
  schema = "main",
  tempEmulationSchema = NULL
)

#  ====

# ====== evidence Synth

connectionDetailsES <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = "../resources/esDatabase/databaseFile.sqlite"
)

connectionHandlerES  <- ResultModelManager::ConnectionHandler$new(
  connectionDetailsES, 
  loadConnection = FALSE
  )

resultDatabaseSettingsES <- list(
  dbms = 'sqlite',
  tablePrefix = 'es_',
  cgTablePrefix = 'cg_',
  cmTablePrefix = 'cm_',
  sccsTablePrefix = 'sccs_',
  databaseTable = 'DATABASE_META_DATA', 
  databaseMetaData = 'DATABASE_META_DATA', 
  schema = "main",
  tempEmulationSchema = NULL
)

#  ====

## cleanup after tests complete
withr::defer({
  options("shiny-test-env-enabled" = FALSE)
  connectionHandlerCG$finalize()
  connectionHandlerPlp$finalize()
  connectionHandlerCharacterization$finalize()
  connectionHandlerDataDiag$finalize()
  connectionHandlerCm$finalize()
  connectionHandlerCohortDiag$finalize()
  connectionHandlerSccs$finalize()
  connectionHandlerES$finalize()
}, testthat::teardown_env())