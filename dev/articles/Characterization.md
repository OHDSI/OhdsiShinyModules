# Characterization

## Introduction

The OHDSI Characterization package lets users extract descriptive
analyses from observational healthcare data sets mapped to the OMOP CDM.
There are currently four different types of characterizations analyses
(incidence rates, time-to-event, dechallenge-rechallenge and various
aggregate covariate cohort comparisons).

The Characterization package currently lets users answer the following
questions:

- **Incidence Rate**: How often does `<add outcome>` occur within
  `<add time-at-risk>` after first record of
  `<add exposure/indication>`?
- **Time-to-event**: When does `<add outcome>` occur relative to the
  first recorded of `<add exposure/indication>`? Is it more common
  before or after `<add exposure>`?
- **Dechallenge-rechallenge**: Is there any evidence of `<add outcome>`
  causing `<add exposure>` to be discontinued and then `<add outcome>`
  re-occurring once `<add exposure>` restarts?
- **Cohort Comparison**: What is different at index between patients in
  `<add exposure/outcome/indication>` and patients in
  `<add different exposure/outcome/indication>`?
- **Database Comparison**: What is different at index between patients
  in `<add exposure/outcome/indication>` across two or more OMOP CDM
  databases?
- **Risk factors**: What are the risk factors of `<add outcome>`
  occurring within `<add time-at-risk>` for those exposed to
  `<add exposure>`?
- **Case-series**: What happens to cases (those exposure to
  `<add exposure>` who have `<add outcome>` during `<add time-at-risk>`)
  before exposure, between exposure and outcome start and after outcome
  start? How bad are the cases prognosis?

## Features and Functionalities

Defining a **target cohort** as a set of patients with an exposure or
interest and/or with evidence of having an indication of interest and an
**outcome cohort** as a set of patients with evidence of the outcome of
interest, we run the following analyses:

1.  **Cohort Summary** - computes aggregate covariate summaries for
    cohorts (targets and/or outcomes), offering a granular view of the
    cohort’s demographics, conditions, drug exposures, and more. This
    enables a deeper understanding of the cohort’s characteristics at
    various time points at or relative to the index date.:
    - **Database Comparison** Lets you compare the same cohort across
      two or more databases and adds in the standardized mean different
      calculation when exactly two databases are selected. This is a
      measure of association between the feature and the cohort,
      therefore identifying which features differ across databases.  
    - **Cohort Comparison** Lets you compare two or more cohorts across
      a database and adds in the standardized mean different calculation
      when exactly two cohort are selected. This is a measure of
      association between the feature and the cohort, therefore
      identifying which features differ across databases.
2.  **Exposed Case Series** - characterizations that look at people in
    the target cohort who have the outcome during some specified
    time-at-risk:
    - **Risk Factor** Compares aggregate covariate summaries for
      patients in the target who have the outcome during the
      time-at-risk period vs patients in the target who do not have the
      outcome during the time-at-risk period. The standardized mean
      difference is added to identify covariates that differ between the
      cohorts.
    - **Case Series** Compares aggregate covariate summaries before
      target start, between target start up to outcome start and after
      outcome start for people in the target cohort who have the outcome
      during some specified time-at-risk. This lets you see covariates
      that are common before exposure and what happens afterwards.
    - **Time to Event**: Shows the distribution of when the outcome
      occurs relative to the target start. This can show you whether the
      outcome occurs more after or before target exposure.
    - **Dechallenge Rechallenge**: Offers the ability to compute
      dechallenge (withdrawal of a drug or treatment) and rechallenge
      (reintroduction) results. This analysis is critical for
      understanding the causality between exposures and outcomes,
      especially in pharmacovigilance studies and when adverse events
      following exposure to a drug may occur.
3.  **Incidence Rate**: Utilizing the
    [CohortIncidence](https://github.com/OHDSI/CohortIncidence "CohortIncidence")
    R package, this set of analyses computes incidence rates for both
    target and outcome cohorts during the time at risk selected. This
    feature is essential for assessing the frequency of outcomes or
    conditions within the specified timeframe, providing a quantitative
    measure of risk or occurrence. Incidence measures are provided in
    both tabular and graphical form, and can be stratified across
    calendar year, age, and sex.

## Utility and Application

Characterization serves as a powerful tool for researchers aiming to
dissect and understand the nuances of patient cohorts in observational
health data. Its capabilities allow for the detailed examination of
cohort attributes, the incidence of health outcomes, and the effects of
treatment exposures over time. By facilitating a comprehensive analysis
of target and comparator cohorts, Characterization enables researchers
to draw meaningful conclusions about patient care, treatment efficacy,
and health outcomes, thereby contributing to the advancement of
evidence-based medicine.

To find out more about the analyses execution details and see examples,
please see
[here](https://ohdsi.github.io/OhdsiShinyModules/articles/Characterization.html).

To see the code behind the Characterization R package, please see
[here](https://github.com/OHDSI/Characterization).
