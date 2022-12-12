# code to create test data

testDir <- tempdir()

library(PatientLevelPrediction)
connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

covSet <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, 
                                                     useDemographicsAge = T, 
                                                     useDemographicsRace = T,
                                                     useDemographicsEthnicity = T, 
                                                     useDemographicsAgeGroup = T,
                                                     useConditionGroupEraLongTerm = T, 
                                                     useDrugEraStartLongTerm  = T, 
                                                     endDays = -1
)


databaseDetails <- createDatabaseDetails(
  connectionDetails = connectionDetails, 
  cdmDatabaseSchema = "main", 
  cdmDatabaseName = "eunomia", 
  cdmDatabaseId = 'eunomia',
  cohortDatabaseSchema = "main", 
  cohortTable = "cohort", 
  outcomeDatabaseSchema = "main", 
  outcomeTable =  "cohort",
  targetId = 1, 
  outcomeIds = 3, #make this ids
  cdmVersion = 5)

restrictPlpDataSettings <- createRestrictPlpDataSettings(  
  firstExposureOnly = T, 
  washoutPeriod = 365
)

modelDesign <- PatientLevelPrediction::createModelDesign(
  targetId = 1, 
  outcomeId = 3, 
  restrictPlpDataSettings =  restrictPlpDataSettings, 
  populationSettings = createStudyPopulationSettings(), 
  splitSettings = createDefaultSplitSetting(splitSeed = 12), 
  sampleSettings = createSampleSettings(), 
  featureEngineeringSettings = createFeatureEngineeringSettings(), 
  preprocessSettings = createPreprocessSettings(), 
  modelSettings = setLassoLogisticRegression(seed = 12)
)

PatientLevelPrediction::runMultiplePlp(
  databaseDetails = databaseDetails, 
  modelDesignList = list(modelDesign), 
  saveDirectory = testDir, 
)

PatientLevelPrediction::validateMultiplePlp(
  analysesLocation = testDir, 
  validationDatabaseDetails = databaseDetails, 
  validationRestrictPlpDataSettings = restrictPlpDataSettings, 
  recalibrate = NULL, 
  saveDirectory = file.path(testDir, 'Validation'))

#PatientLevelPrediction::viewMultiplePlp(testDir)
