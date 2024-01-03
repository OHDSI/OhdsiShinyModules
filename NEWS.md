OhdsiShinyModules v2.0.3
========================
Support for data models from SCCS version 5.2.0
Support for data models from Cohort Method version 5.2.0
Fixes for CohortDiagnostics Orpahan concept table by re-writing with DB level pagination

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
