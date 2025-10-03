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


# =========== characterization START

# moving to OhdsiReportGenerator database
connectionDetailsCharacterization <- OhdsiReportGenerator::getExampleConnectionDetails()
connectionHandlerCharacterization <- ResultModelManager::ConnectionHandler$new(
  connectionDetailsCharacterization, 
  loadConnection = FALSE
)
resultDatabaseSettingsCharacterization <- OhdsiShinyAppBuilder::createDefaultResultDatabaseSettings()
# =========== Characterization END



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


## cleanup after tests complete
withr::defer({
  options("shiny-test-env-enabled" = FALSE)
  connectionHandlerCharacterization$finalize()
  connectionHandlerDataDiag$finalize()
  connectionHandlerCohortDiag$finalize()
  connectionHandlerPV$finalize()
}, testthat::teardown_env())
