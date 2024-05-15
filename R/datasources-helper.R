# 
# # libraries
# library(Achilles)
# library(dplyr)
# library(knitr)
# library(magrittr)
# 
# options(connectionObserver = NULL)
# 
# # variables
# serverRoot <- "ohda-prod-1.cldcoxyrkflo.us-east-1.redshift.amazonaws.com"
# 
# #extract most recent cdms
# baseUrl <- "https://epi.jnj.com:8443/WebAPI" # production
# 
# # Authorize using windows authentication
# httr::set_config(httr::config(ssl_verifypeer = FALSE))
# ROhdsiWebApi::authorizeWebApi(baseUrl, "windows")
# 
# # Pulls list of active sources from WebApi
# sources <- ROhdsiWebApi::getCdmSources(baseUrl)
# 
# # Parses database names and versions from strings
# for(i in 1:nrow(sources)){
#   if(sources$sourceName[i] == "Default Vocabulary"){next}else{
#     cdmSchema <- sources$sourceKey[i]
#     resultsSchema <- sources$resultsDatabaseSchema[i]
#     database <- substr(cdmSchema,5,nchar(cdmSchema)-6)
#     version <- substr(cdmSchema,nchar(cdmSchema)-3,nchar(cdmSchema))
#     if(exists("cdmSources") == FALSE){cdmSources <- data.frame(database, cdmSchema, resultsSchema, version)}
#     else{cdmSources <- rbind(cdmSources, c(database,cdmSchema,resultsSchema,version))}
#   }
# }
# 
# latestDbs<- dplyr::distinct(cdmSources %>%
#   dplyr::group_by(database) %>%
#   dplyr::slice_max(version, n=1) %>%
#   #filter(version %in% c(2889,2552,2745,2788,2784,2906,2885,2883,2754,2887,2888,2886,2737))
#   dplyr::filter(database %in% c("cprd", "ims_australia_lpd", "ims_france", "ims_germany", "health_verity_cc_ehr_cce",
#                          "iqvia_pharmetrics_plus","jmdc","optum_ehr","optum_extended_ses", "optum_extended_dod",
#                          "premier", "truven_ccae",
#                          "truven_mdcr","truven_mdcd"))
# )
#   
# latestDbs$database[latestDbs$database == 'health_verity_cc_ehr_cce'] <- 'health_verity'
# 
# databases <- latestDbs %>%
#   dplyr::mutate(
#     sourceCountry = dplyr::case_when(
#       database %in% "cprd" ~ "UK",
#       database %in% "ims_australia_lpd" ~ "Australia",
#       database %in% "ims_france" ~ "France",
#       database %in% "ims_germany" ~ "Germany",
#       database %in% "jmdc" ~ "Japan",
#       .default = "US"
#     ),
#     sourceProvenance = dplyr::case_when(
#       database %in% c("cprd", "ims_australia_lpd", "ims_france", "ims_germany") ~ "General Practitioner",
#       database %in% c("premier") ~ "Hospital Billing Data",
#       database %in% c("optum_ehr") ~ "Electronic Health Records",
#       .default = "Administrative Claims"
#     ),
#     cdmSourceAbbreviation = dplyr::case_when(
#       database == "ims_australia_lpd" ~ "LPDAU",
#       database == "ims_france" ~ "France DA",
#       database == "ims_germany" ~ "German DA",
#       database == "jmdc" ~ "JMDC",
#       database == "iqvia_pharmetrics_plus" ~ "PharMetrics",
#       database == "optum_ehr" ~ "Optum EHR",
#       database == "optum_extended_dod" ~ "OPTUM Extended DOD",
#       database == "optum_extended_ses" ~ "OPTUM Extended SES",
#       database == "truven_ccae" ~ "IBM CCAE",
#       database == "truven_mdcd" ~ "IBM MDCD",
#       database == "truven_mdcr" ~ "IBM MDCR",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   dplyr::rename(cdmDatabaseSchema = "cdmSchema",
#                 cdmResultsSchema = "resultsSchema") %>%
#   dplyr::filter(database != "cprd" & database != "health_verity")
# 
# 
# datalist <- list()
# for(i in 1:nrow(databases)){
#   
#   connectionDetailsCdm <- DatabaseConnector::createConnectionDetails(
#     dbms = "redshift",
#     user = keyring::key_get("redShiftUserName", keyring = keyringName),
#     password = keyring::key_get("redShiftPassword", keyring = keyringName),
#     server = paste0(serverRoot, "/", databases$database[i]),
#     port = 5439,
#     extraSettings = "ssl=true&sslfactory=com.amazon.redshift.ssl.NonValidatingFactory")
#     
#     dbSummary <- Achilles::generateDbSummary(connectionDetailsCdm,
#                                              cdmDatabaseSchema = databases$cdmDatabaseSchema[i],
#                                              resultsDatabaseSchema = databases$cdmResultsSchema[i],
#                                              country = databases$sourceCountry[i],
#                                              provenance = databases$sourceProvenance[i])
#     
#     tableOutputNew <- dbSummary$summary
#     
#     # Sort and take the top 7 source vocabs
#     sourceVocabSort <- dbSummary$sourceVocabs[order(-dbSummary$sourceVocabs$COUNT_VALUE),]
# 
#     if(nrow(sourceVocabSort)>7){sourceVocabSort <- sourceVocabSort[1:7,]}
#     sourceVocabSort <- sourceVocabSort[!(sourceVocabSort$VOCABULARY_ID=="None"|sourceVocabSort$VOCABULARY_ID=="NA"),]
# 
#     # Create the final table
#     tableOutputNew$"Source Vocabularies" <- paste(sourceVocabSort$VOCABULARY_ID, collapse="<br>")
#     tableOutputNew$"Visits" <- paste(dbSummary$visitDist$TERMINAL_ANCESTOR_CONCEPT_NAME, collapse="<br>")
# 
#     # Update the DB names
#     if(!(tableOutputNew$"Data Source Abbreviation" %in% c("CPRD", "JMDC", "PREMIER", "OPTUM Extended SES", "OPTUM Extended DOD", "Optum EHR", "PharMetrics"))) {
#       tableOutputNew$"Data Source" <- paste0(tableOutputNew$"Data Source Name", " (", tableOutputNew$"Data Source Abbreviation", ")")
#     } else {
#       tableOutputNew$"Data Source" <- tableOutputNew$"Data Source Name"
#     }
# 
#     tableOutputNew <- tableOutputNew %>%
#       dplyr::relocate("Data Source", .before = "Data Source Name") %>%
#       dplyr::relocate("Visits", .before = "Source Vocabularies")
# 
#     tableOutputNew <- dplyr::select(tableOutputNew, -c("Data Source Name", "Data Source Abbreviation", "Visits per Person", "Inpatient Visits per Person") )
#   #   
#     datalist[[i]] <- tableOutputNew
#     
#   #   
#   #   tableOutput <- rbind(tableOutput, tableOutputNew)
#   #   rm(tableOutputNew)
#   #   rm(sourceVocabSort)
#   #   
#   # }
# }
# 
# dbSummaryTable = dplyr::bind_rows(datalist) %>%
#   dplyr::mutate(
#     cdmSourceAbbreviation = dplyr::case_when(
#       `Data Source` == "LPD Australia (LPDAU)" ~ "LPDAU",
#       `Data Source` == "France Disease Analyzer (France DA)" ~ "France DA",
#       `Data Source` == "German Disease Analyzer (German DA)" ~ "German DA",
#       `Data Source` == "Japan Medical Data Center (JMDC)" ~ "JMDC",
#       `Data Source` == "PharMetrics Plus" ~ "PharMetrics",
#       `Data Source` == "Optum EHR" ~ "Optum EHR",
#       `Data Source` == "Optum’s  Clinformatics® Extended Data Mart – Date of Death (DOD)" ~ "OPTUM Extended DOD",
#       `Data Source` == "Optum’s  Clinformatics® Extended Data Mart – Socio-Economic Status (SES)" ~ "OPTUM Extended SES",
#       `Data Source` == "IBM Health MarketScan® Commercial Claims and Encounters Database (IBM CCAE)" ~ "IBM CCAE",
#       `Data Source` == "IBM Health MarketScan® Multi-State Medicaid Database (IBM MDCD)" ~ "IBM MDCD",
#       `Data Source` == "IBM Health MarketScan® Medicare Supplemental and Coordination of Benefits Database (IBM MDCR)" ~ "IBM MDCR",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   dplyr::relocate(cdmSourceAbbreviation, .after = `Data Source`)
# 
# getDbSummaryTable <- function(){
#   return(dbSummaryTable)
# }
