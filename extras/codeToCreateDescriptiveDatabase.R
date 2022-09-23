# code to create test data

testDir <- tempdir()
testDir <- '/Users/jreps/Documents/github/OhdsiShinyModules/tests/resources'

library(DescriptiveStudies)
connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

targetIds <- 1
outcomeIds <- 2

covSet <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, 
                                                     useDemographicsAge = T, 
                                                     useDemographicsRace = T,
                                                     useDemographicsEthnicity = T, 
                                                     useDemographicsAgeGroup = T,
                                                     useConditionGroupEraLongTerm = T, 
                                                     useDrugEraStartLongTerm  = T, 
                                                     endDays = -1
)

descSet <- DescriptiveStudies::createCharacterizationSettings(
  timeToEventSettings = DescriptiveStudies::createTimeToEventSettings(
    targetIds = targetIds, 
    outcomeIds = outcomeIds
  ) , 
  dechallengeRechallengeSettings = DescriptiveStudies::createDechallengeRechallengeSettings(
    targetIds = targetIds, 
    outcomeIds = outcomeIds
  ), 
  aggregateCovariateSettings = DescriptiveStudies::createAggregateCovariateSettings(
    targetIds = targetIds, 
    outcomeIds = outcomeIds, 
    riskWindowStart = 1, riskWindowEnd = 365, 
    covariateSettings = covSet
  )
)

DescriptiveStudies::runCharacterizationAnalyses(
  connectionDetails = connectionDetails, 
  targetDatabaseSchema = "main", 
  targetTable = "cohort", 
  outcomeDatabaseSchema = "main", 
  outcomeTable = "cohort", 
  cdmDatabaseSchema = "main", 
  characterizationSettings = descSet, 
  saveDirectory = file.path(testDir,'descDatabase'), 
  databaseId = 'eunomia', 
  tablePrefix = 'c_'
  )


if(F){
# add in rhe database_meta_data and cohort_definitions tables

serverDesc <- "tests/resources/descDatabase/databaseFile.sqlite"
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
  port = NULL
)

DatabaseConnector::insertTable(
  connection = connectionDesc, 
  databaseSchema = 'main', 
  tableName = 'cg_cohort_definition', 
  data = data.frame(
    cohortDefinitionId = c(1,2),
    cohortName = c('target example','outcome example'),
    description = c('',''),
    json = c('{}', '{}'),
    sqlCommand = c('','')
    ), 
  createTable = T, 
  camelCaseToSnakeCase = T
  )

DatabaseConnector::insertTable(
  connection = connectionDesc, 
  databaseSchema = 'main', 
  tableName = 'database_meta_data', 
  data = data.frame(
    databaseId  = 'eunomia',
    cdmSourceName = 'eunomia',
    cdmSourceAbbreviation = 'eunomia'
  ), 
  createTable = T, 
  camelCaseToSnakeCase = T
)

}
