# code to create test data

testDir <- tempdir()

server <- '/Users/jreps/Documents/GitHub/OhdsiShinyModules/tests/resources/sccsDatabase/databaseFile.sqlite'

results <- '/Users/jreps/Documents/fakeSccs'

# create sqlite and insert each table:
tables <- dir(file.path(results), pattern = '.csv')
resultDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite', 
  server = server #"/Users/jreps/Documents/github/OhdsiShinyModules/tests/resources/estDatabase/databaseFile.sqlite"
    )
sqliteCon <- DatabaseConnector::connect(connectionDetails = resultDetails)
for(table in tables){
  DatabaseConnector::insertTable(
    connection = sqliteCon,
    databaseSchema = 'main', 
    tableName = gsub('.csv','',table), 
    data = utils::read.csv(file.path(results, table)), 
    createTable = T, 
    dropTableIfExists = T, 
    camelCaseToSnakeCase = F
      )
}



DatabaseConnector::insertTable(
  connection = sqliteCon, 
  databaseSchema = 'main', 
  tableName = 'cg_cohort_definition', 
  data = data.frame(
    cohortDefinitionId = c(1,2,3,4),
    cohortName = c('target 1 example','target 2 example','outcome example','target 4 example'),
    description = rep('',4),
    json = rep('{}', 4),
    sqlCommand = rep('',4)
  ), 
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = T
)

DatabaseConnector::insertTable(
  connection = sqliteCon, 
  databaseSchema = 'main', 
  tableName = 'database_meta_data', 
  data = data.frame(
    databaseId  = '1',
    cdmSourceName = 'eunomia',
    cdmSourceAbbreviation = 'eunomia'
  ), 
  createTable = T, dropTableIfExists = T,
  camelCaseToSnakeCase = T
)
