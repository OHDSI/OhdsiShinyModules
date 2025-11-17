# Prediction

## Introduction

Patient-level prediction lets users answer the question, who is at risk
or an outcome during some time period within a target population. For
example, you can answer: who is at risk of developing angioedema within
a year of starting lisinopril, in new users or lisinopril?

## Features and Functionalities

We define a **target cohort** as a set of patients with an exposure or
interest and/or with evidence of having an indication of interest, an
**outcome cohort** as a set of patients with evidence of the outcome of
interest, and a **time-at-risk** as a period of time to where the
patient is at risk of developing the outcome. The package shows
performance of models developed to predict the outcome during the
time-at-risk relative to the target cohort index for patients in the
target cohort.

The first page lets you pick one or more target cohorts and outcome
cohorts to restrict to. Then a model design summary table is displayed
restricted to the selected target and outcome cohorts. The model design
summary aggregates the performances of models developed across different
databases for the same model design (target cohort, outcome,
time-at-risk, population inclusion criteria, model and data
preprocessing). The summary table includes the model design id, target
cohort name, outcome cohort name, time-at-risk, the min/mean/max AUROC
for models developed using the model design across databases as well as
the number of databases included in diagostics, model development and
model validation.

The first column of the summary table is a button that enables users to
dive deeper into the results. Users can select from:

- view models (this lets users see the performance of all models
  developed using the model design)
- view diagnostic (this lets users see the diagnostic results showing
  the suitability of the database for the model design)
- view report (this lets users view a html summary report of all the
  models developed using the model design)

### View models

The view models view shows all the model development/validation results
for the selected model design. The table shows the development database,
validation data, target name, outcome name, time-at-risk (TAR), the
AUROC, AUPRC, number of people in the target cohort, number of target
cohort with the outcome during TAR, the percentage of the target
population used for model validation (this is 100% when displaying
external validation) and percentage of target population with the
outcome during TAR.

Users can select a result view to explore the model more:

- View results (this lets users view the model,
  calibration/discrimination plots and net benefit plots).
- View attrition (this lets users see where patients were lost between
  paients in the database and the final target population).

### View diagnostic

The view diagnostic view shows diagnostics based on PROBAST, that aims
to see the risk of bias in a model design when applied to a specific
database. Click [here](https://pubmed.ncbi.nlm.nih.gov/30596875/) to
read more about PROBAST.

### View report

The view report view displays a summary report of the results for the
selected model design. This requires additional R package dependencies
that must be installed. If the dependencies are not installed, a warning
will display telling the user what package is missing.

## Utility and Application

Patient-level prediction enables users to identify who is at risk of
developing some future outcome. This can be used to guide clinical
interventions or risk mitigation or early detection.

To find out more about the analyses execution details and see examples,
please see
[here](https://ohdsi.github.io/PatientLevelPrediction/articles/BuildingMultiplePredictiveModels.html).

To see the code behind the PatientLevelPrediction R package, please see
[here](https://github.com/OHDSI/PatientLevelPrediction).
