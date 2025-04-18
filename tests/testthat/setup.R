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

# home test setup
#==============
# create a html file in a folder
# add the folder location to environmental var shiny_report_folder
homeTempDir <- file.path(tempdir(),'reports')
if(!dir.exists(homeTempDir)){
  dir.create(homeTempDir, recursive = T)
}
write.table(x = c(a=1, b=2), file = file.path(homeTempDir, 'Prediction.html'))
Sys.setenv(shiny_report_folder = homeTempDir)
shiny::addResourcePath('www-reports', homeTempDir)
#==============

dbmsTest <- 'sqlite'
schemaTest <- 'main'

# =========== CG START
cgTablePrefix <- 'cg_'

connectionDetailsCG <- DatabaseConnector::createConnectionDetails(
  server = "../resources/cgDatabase/databaseFile.sqlite",
  dbms = 'sqlite'
)
connectionHandlerCG <- ResultModelManager::ConnectionHandler$new(connectionDetailsCG, loadConnection = FALSE)

resultDatabaseSettingsCG <- list(
  dbms = 'sqlite',
  cgTablePrefix = 'cg_',
  cgTablePrefix = 'cg_',
  databaseTable = 'DATABASE_META_DATA',
  databaseTablePrefix = '',
  schema = 'main',
  tempEmulationSchema = NULL
)

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
  cgTablePrefix = '',
  databaseTablePrefix = '',
  databaseTable = 'DATABASE_META_DATA',
  schema = 'main'
)
# =========== PLP End



# =========== characterization START
serverCharacterization <- "../resources/cDatabase/databaseFile.sqlite"
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
  cTablePrefix = 'c_',
  cgTablePrefix = 'cg_',
  databaseTablePrefix = '',
  schema = 'main',
  databaseTable = 'DATABASE_META_DATA',
  incidenceTablePrefix = 'i_',
  tempEmulationSchema = NULL
)



# =========== Characterization END


# =========== Estimation START
connectionDetailsEstimation <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite', 
  server = "../resources/estDatabase/databaseFile.sqlite"
)

connectionHandlerEstimation  <- ResultModelManager::ConnectionHandler$new(
  connectionDetailsEstimation, 
  loadConnection = FALSE
  )

resultDatabaseSettingsEstimation <- list(
  dbms = 'sqlite',
  cmTablePrefix = 'cm_',
  esTablePrefix = 'es_',
  sccsTablePrefix = 'sccs_',
  cgTablePrefix = 'cg_',
  databaseTable = 'DATABASE_META_DATA',
  schema = "main",
  tempEmulationSchema = NULL
)

resultDatabaseSettingsEstimationCm <- list(
  dbms = 'sqlite',
  cmTablePrefix = 'cm_',
  esTablePrefix = 'es2_',
  sccsTablePrefix = 'sccs2_',
  cgTablePrefix = 'cg_',
  databaseTable = 'DATABASE_META_DATA',
  schema = "main",
  tempEmulationSchema = NULL
)

resultDatabaseSettingsEstimationSccs <- list(
  dbms = 'sqlite',
  cmTablePrefix = 'cm2_',
  esTablePrefix = 'es2_',
  sccsTablePrefix = 'sccs_',
  cgTablePrefix = 'cg_',
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
  ddTablePrefix = '',
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
  cdTablePrefix = '',
  schema = "main",
  vocabularyDatabaseSchema = "main"
)

connectionHandlerCohortDiag <- ResultModelManager::ConnectionHandler$new(connectionDetailsCohortDiag, loadConnection = FALSE)

dataSourceCd <-
  createCdDatabaseDataSource(
    connectionHandler = connectionHandlerCohortDiag,
    resultDatabaseSettings = resultDatabaseSettingsCohortDiag,
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
  sccsTablePrefix = 'sccs_',
  cgTablePrefix = 'cg_',
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
  esTablePrefix = 'es_',
  cgTablePrefix = 'cg_',
  cmTablePrefix = 'cm_',
  sccsTablePrefix = 'sccs_',
  databaseTable = 'DATABASE_META_DATA', 
  databaseMetaData = 'DATABASE_META_DATA', 
  schema = "main",
  tempEmulationSchema = NULL
)

#  ====

# ====== PheValuator

connectionDetailsPV <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = "../resources/pvDatabase/phevaluator.sqlite"
)

connectionHandlerPV  <- ResultModelManager::ConnectionHandler$new(
  connectionDetailsPV, 
  loadConnection = FALSE
)

resultDatabaseSettingsPV = list(
  dbms = 'sqlite',
  pvTablePrefix = 'pv_',
  schema = 'main'
)

#  ====

# ====== DataSources

connectionDetailsDS <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = "../resources/DSDatabase/databaseFile.sqlite"
)

connectionHandlerDS  <- ResultModelManager::ConnectionHandler$new(
  connectionDetailsDS, 
  loadConnection = FALSE
)

resultDatabaseSettingsDS = list(
  dbms = 'sqlite',
  databaseTablePrefix = '',
  schema = 'main',
  databaseTable = 'DATABASE_META_DATA'
)

#  ====

## cleanup after tests complete
withr::defer({
  options("shiny-test-env-enabled" = FALSE)
  connectionHandlerCG$finalize()
  connectionHandlerPlp$finalize()
  connectionHandlerCharacterization$finalize()
  connectionHandlerDataDiag$finalize()
  connectionHandlerCohortDiag$finalize()
  connectionHandlerEstimation$finalize()
  connectionHandlerSccs$finalize()
  connectionHandlerES$finalize()
  connectionHandlerDS$finalize()
  connectionHandlerPV$finalize()
}, testthat::teardown_env())
