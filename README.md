OhdsiShinyModules
======================

OhdsiShinyModules is an R package containing shiny modules that can be used within shiny result interfaces.

The OHDSI tools often provide shiny interfaces for viewing and exploring results.  Many of these shiny apps have overlapping features.  To ensure consistency we have created a repository containing useful shiny modules that can be used in multiple result exporers.

Current Modules
========
- form interface for connecting to an ATLAS webapi (with or without authorisation) [webApiViewer/webApiServer]
- button to extracting the all cohorts once connected to an ATLAS webapi [extractCohortsViewer/extractCohortsServer]
- form for selecting cohorts of interest [cohortViewer/cohortServer] 
- form for creating FeatureExtraction covariate setting objects [covariateViewer/covariateServer]
- form for create population settings (used by PatientLevelPrediction) [populationViewer/populationServer]
- form for selecting a PatientLevelPrediction model (plus settings) [modelViewer/modelServer]
- ...

Screenshots
===========

<table>

<tr>
<td> webApi Module </td>
<td>
<img src="https://github.com/OHDSI/OhdsiShinyModules/raw/master/vignettes/screenshots/webApiExample.png" alt="webApiExample" width = 100px height = 100px title="webApi Module Example" />
</td>
</tr>

<tr>
<td> extractCohort Module </td>
<td>
<img src="https://github.com/OHDSI/OhdsiShinyModules/raw/master/vignettes/screenshots/extractCohortExample.png" alt="extractCohortExample" width = 100px height = 30px title="extractCohort Module Example" />
</td>
</tr>

<tr>
<td> cohort Module </td>
<td>
<img src="https://github.com/OHDSI/OhdsiShinyModules/raw/master/vignettes/screenshots/cohortExample.png" alt="extractCohortExample" width = 500px height = 500px title="Cohort Module Example" />
</td>
</tr>

<tr>
<td> model Module </td>
<td>
<img src="https://github.com/OHDSI/OhdsiShinyModules/raw/master/vignettes/screenshots/modelExample.png" alt="extractCohortExample" width = 500px height = 500px title="Model Module Example" />
</td>
</tr>

</table>


Technology
==========
OhdsiShinyModules is an R package that uses the R shiny library.  

System Requirements
===================
Requires R (version 3.3.0 or higher). 

Getting Started
===============

- To install the package:

```
install.packages(devtools)
devtools::install_github('ohdsi/OhdsiShinyModules')
```

Please read the main vignette for the package:

- [Using OHDSI shiny Models](https://github.com/OHDSI/OhdsiShinyModules/blob/master/inst/doc/UsingOhdsiShinyModules.pdf)

Package manual: [OhdsiShinyModules.pdf](https://github.com/OHDSI/OhdsiShinyModules/blob/master/extras/OhdsiShinyModules.pdf)


Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/OhdsiShinyModules/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
* Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package. 
* This [website](https://mastering-shiny.org/scaling-modules.html) may be helpful if you want to see an introduction into how to write shiny modules
 
License
=======
OhdsiShinyModules is licensed under Apache License 2.0

Development
===========
OhdsiShinyModules is being developed in R Studio.


# Acknowledgements

- The package is maintained by Jenna Reps and has been developed with major contributions from ...