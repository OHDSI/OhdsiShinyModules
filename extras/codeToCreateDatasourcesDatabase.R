# code to create test data

testDir <- tempdir()
testDir <- 'D:/shiny_test/GitHub Desktop/standardization/OhdsiShinyModules/tests/resources'

serverDS <- "tests/resources/dsDatabase/databaseFile.sqlite"
connectionDetailsDS <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = serverDS
)
connectionDS <- DatabaseConnector::connect(
  connectionDetails = connectionDetailsDS, 
  dbms = 'sqlite', 
  user = NULL, 
  password = NULL, 
  server = serverDS,
  port = NULL
)

DatabaseConnector::insertTable(
  connection = connectionDS, 
  databaseSchema = 'main', 
  tableName = 'database_meta_data', 
  data = data.frame(
    databaseId  = '1',
    cdmSourceName = 'eunomia',
    cdmSourceAbbreviation = 'eunomia'
  ), 
  createTable = T, 
  camelCaseToSnakeCase = T
)

# DatabaseConnector::dbRemoveTable(connectionDS, "cg_cohort_count")
# DatabaseConnector::dbRemoveTable(connectionDS, "cg_cohort_generation")
# DatabaseConnector::dbRemoveTable(connectionDS, "cg_cohort_inclusion")
# DatabaseConnector::dbRemoveTable(connectionDS, "cg_cohort_inc_result")
# DatabaseConnector::dbRemoveTable(connectionDS, "cg_cohort_summary_stats")
# DatabaseConnector::dbRemoveTable(connectionDS, "cg_cohort_definition")
