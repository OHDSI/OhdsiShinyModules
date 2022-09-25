# code to create test data

testDir <- tempdir()
testDir <- '/Users/jreps/Documents/github/OhdsiShinyModules/tests/resources'

serverCG <- "tests/resources/cgDatabase/databaseFile.sqlite"
connectionDetailsCG <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = serverCG
)
connectionCG <- DatabaseConnector::connect(
  connectionDetails = connectionDetailsCG, 
  dbms = 'sqlite', 
  user = NULL, 
  password = NULL, 
  server = serverCG,
  port = NULL
)

DatabaseConnector::insertTable(
  connection = connectionCG, 
  databaseSchema = 'main', 
  tableName = 'CG_COHORT_COUNT', 
  data = data.frame(
    made_up_colunm = 1
    ), 
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = F
  )

DatabaseConnector::insertTable(
  connection = connectionCG, 
  databaseSchema = 'main', 
  tableName = 'CG_COHORT_GENERATION', 
  data = data.frame(
    made_up_colunm = 1
  ), 
  createTable = T, dropTableIfExists = T, 
  camelCaseToSnakeCase = F
)

DatabaseConnector::insertTable(
  connection = connectionCG, 
  databaseSchema = 'main', 
  tableName = 'CG_COHORT_SUMMARY_STATS', 
  data = data.frame(
    database_id = 1, 
    cohort_definition_id =1,
    made_up_colunm = 1
  ), 
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = F
)

