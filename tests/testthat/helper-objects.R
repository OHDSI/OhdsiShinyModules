jarFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER", unset = "")
if (jarFolder == "") {
  tempJarFolder <- tempfile("jdbcDrivers")
  dir.create(tempJarFolder)
  Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = tempJarFolder)
  downloadJdbcDrivers("postgresql")
  
  withr::defer({
    unlink(tempJarFolder, recursive = TRUE, force = TRUE)
    Sys.unsetenv("DATABASECONNECTOR_JAR_FOLDER")
  }, testthat::teardown_env())
}

server <- "../resources/databaseFile.sqlite"
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = server
)

connection <- DatabaseConnector::connect(
  connectionDetails = connectionDetails, 
  dbms = 'sqlite', 
  user = NULL, 
  password = NULL, 
  server = server,
  port = NULL#, 
  #pathToDriver = 
)

mySchemaTest <- 'main'
targetDialectTest <- 'sqlite'
myTableAppendTest <- ''


