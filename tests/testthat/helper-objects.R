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

serverPlp <- "../resources/plpDatabase/databaseFile.sqlite"
connectionDetailsPlp <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = serverPlp
)

connectionPlp <- DatabaseConnector::connect(
  connectionDetails = connectionDetailsPlp, 
  dbms = 'sqlite', 
  user = NULL, 
  password = NULL, 
  server = serverPlp,
  port = NULL#, 
  #pathToDriver = 
)

schemaTest <- 'main'
dbmsTest <- 'sqlite'
tablePrefixTest <- ''





serverDesc <- "../resources/descDatabase/databaseFile.sqlite"
connectionDetailsDesc <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = serverDesc
)
connectionDesc <- DatabaseConnector::connect(
  connectionDetails = connectionDetailsDesc, 
  dbms = 'sqlite', 
  user = NULL, 
  password = NULL, 
  server = serverDesc,
  port = NULL#, 
  #pathToDriver = 
)
descTablePrefix <- 'c_'
cohortTablePrefix <- 'cg_'
databaseTable <- 'DATABASE_META_DATA'
