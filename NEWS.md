OhdsiShinyModules v3.1.0
========================
- Removed percentage calculation from records field in CohortDiagnostics
- Updated the About module to reflect new module names and structure
- Updated module description vignette files to reflect new module names and structure
- Added interactive scatterplots for binary covariates when doing database and cohort comparisons in Characterization

OhdsiShinyModules v3.0.2
========================
- Fixed bug with orphan concepts not loading

OhdsiShinyModules v3.0.1
========================
- Bug fixes

OhdsiShinyModules v3.0.0
========================
- Removed CohortMethod/SCCS/EvidenceSyntheis modules as these
are replaced with Estimation 

OhdsiShinyModules v2.2.1
========================
- Fixed issue in DatabaseConnector check for pooled connections of sqlite databases on cohort diagnotiscs load (from main hotfix)

OhdsiShinyModules v2.2.0
========================
- Combined cohort method, sccs and evidence synthesis into one estimation module with shared target and outcome ids
- Characterizations now share the target id 
- Updated tests to get them all working
- Cleaned R check (but cohort incidence still has many notes)


OhdsiShinyModules v2.1.5
========================
Fixed bug of orphan concepts report not displaying
Fixed bug in orphan concepts where negative database ids were causing dynamic sql queries to crash

OhdsiShinyModules v2.1.4
========================
Fixed missing call to dplyr in CohortDiagnostics load up

OhdsiShinyModules v2.1.3
========================
Hotfix release to fix issue with cohort diagnostics reports hanging on load when using `DatabaseConnector::dbListTables`
on postgres backends

OhdsiShinyModules v2.1.2
========================
Fixed bug in cohort diagnostics incidence rate plots not showing for different strata

OhdsiShinyModules v2.1.1
========================
Fixed bug in cohort diagnostics load up of orphan concepts server causing app to crash on load

OhdsiShinyModules v2.1.0
========================
Support for data models from SCCS version 5.2.0
Support for data models from Cohort Method version 5.2.0
Fixes for CohortDiagnostics Orpahan concept table by re-writing with DB level pagination
Automated release of Docker Images upon package release

OhdsiShinyModules v2.0.2
========================
edited characterization server to work with new aggregate features method in characterization package
edited characterization server to work when one or more characterization result is missing
edited evidence synth module to highlight the bayesian and fix issue with comparison names not showing

OhdsiShinyModules v2.0.1
========================
Bug Fixes:
- Fix for CohortDiagnostic app not loading when characterization was set to FALSE

OhdsiShinyModules v2.0.0
========================
- updated all models to use the same resultDatabaseSettings
- made module function naming consistent (modules named after analysis packages)
- made table prefix inputs consistent across modules
- updated patientlevelprediction, cohortmethod and sccs to look similar.
- Updated Cohort Diagnostics characterization to improve overall performance
- Added plots for incidence in characterization module
- Added PheValuator package
- Numerous bug fixes (see github issues)

OhdsiShinyModules v1.1.0
========================
- Udated the style for Characterization
- added SCCS module (jpg)
- added meta (evidence synthesis) module
- removal of ggiraph package and replacement with plotly plots for CohortDiagnostics plots: Time Distributions, Overlap, Incidence and Compare Cohort Characterization (jpg)

OhdsiShinyModules v1.0.4
========================
- Fixed an issue with hide/show tabs and reactives in the plp viewer

OhdsiShinyModules v1.0.3
========================
- Fixed a glitchy prediction viewer

OhdsiShinyModules v1.0.2
========================
- Added Cohort Diagnostics module
- Changed module input to have connectionHandler
- cleaned check notes

OhdsiShinyModules v1.0.1
========================
- Preparing for HADES

OhdsiShinyModules v1.0.0
========================
- Version ready for release


OhdsiShinyModules v0.2.4
========================
- Test coverage > 80%
- Updated website
- Modules included for HADES pacakges: PatientLevelPrediction, DescriptiveStudies, CohortGenerator and CohortMethod
