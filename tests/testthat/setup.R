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
  tablePrefix = '',
  cohortTablePrefix = '',
  databaseTablePrefix = '',
  schema = 'main'
)
# =========== PLP End



# =========== Desc START
serverDesc <- "../resources/descDatabase/databaseFile.sqlite"
connectionDetailsDesc <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = serverDesc
)

connectionHandlerDesc <- ResultModelManager::ConnectionHandler$new(connectionDetailsDesc, loadConnection = FALSE)

resultDatabaseSettingsDesc <- list(
  dbms = 'sqlite', # should this be removed - can use connection
  tablePrefix = 'c_',
  cohortTablePrefix = 'cg_',
  databaseTablePrefix = '',
  schema = 'main',
  databaseTable = 'DATABASE_META_DATA',
  incidenceTablePrefix = 'i_',
  tempEmulationSchema = NULL
)


# =========== Desc START


# =========== Estimation START
connectionDetailsEst <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite', 
  server = "../resources/estDatabase/databaseFile.sqlite"
)

connectionHandlerEst  <- ResultModelManager::ConnectionHandler$new(connectionDetailsEst, loadConnection = FALSE)

resultDatabaseSettingsEst <- list(
  dbms = 'sqlite',
  tablePrefix = 'cm_',
  cohortTablePrefix = 'cg_',
  databaseTable = 'DATABASE_META_DATA',
  schema = "main",
  tempEmulationSchema = NULL
)

# =========== Estimation END


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

## cleanup after tests complete
withr::defer({
  connectionHandlerCG$finalize()
  connectionHandlerPlp$finalize()
  connectionHandlerDesc$finalize()
  connectionHandlerDataDiag$finalize()
  connectionHandlerEst$finalize()
  connectionHandlerCohortDiag$finalize()
}, testthat::teardown_env())