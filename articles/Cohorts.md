# Cohorts

## Introduction

Cohort identification within real-world evidence (RWE) analyses of data
adhering to the Observational Medical Outcomes Partnership Common Data
Model (OMOP CDM) is a foundational step in understanding healthcare
outcomes and treatment effects. Observational health data, sourced from
diverse sources including Electronic Health Records (EHRs), health
insurance claims, registries, and patient-generated data, offer valuable
insights into patient health status and healthcare delivery. However,
these data were not originally collected for research purposes, leading
to the need for sophisticated methods to infer relevant clinical
information for research objectives. This summary explores the
importance of cohort identification, the methods endorsed by the
Observational Health Data Sciences and Informatics (OHDSI) community,
and the tools available for creating cohorts within OMOP CDM datasets.

## Importance of Cohort Identification

Cohort identification is crucial for conducting meaningful analyses in
healthcare research. A cohort, defined as a set of individuals meeting
one or more inclusion criteria over a duration of time, serves as the
basis for studying healthcare outcomes, treatment effects, and disease
epidemiology. In the context of OHDSI research, cohorts are fundamental
building blocks used for executing research questions, developing
phenotypes, and conducting comparative effectiveness studies. OHDSI’s
approach emphasizes the independence and reusability of cohort
definitions, allowing researchers to define cohorts tailored to specific
research questions while ensuring consistency and reproducibility across
studies.

## Methods for Cohort Identification

OHDSI endorses standardized methods for cohort definition, ensuring
transparency, and reproducibility in research. Two main approaches are
employed for constructing cohorts:

### Rule-based Cohort Definitions

Rule-based cohort definitions rely on explicitly stated inclusion
criteria to define cohort membership. These criteria are typically based
on domain expertise and clinical knowledge, allowing researchers to
specify cohort attributes such as clinical conditions, procedures,
medications, and temporal relationships. OHDSI provides standardized
components for assembling these criteria, including domains, concept
sets, domain-specific attributes, and temporal logic.

### Probabilistic Cohort Definitions

Probabilistic cohort definitions leverage machine learning techniques to
compute the probability of cohort inclusion based on patient
characteristics and clinical events. These models are trained on example
data to automatically identify relevant patient characteristics
predictive of cohort membership. The resulting probabilities can be used
to classify patients into cohorts or as inputs for certain study
designs.

## Tools for Cohort Creation & Identification

OHDSI offers a suite of open-source tools to support cohort
identification within OMOP CDM datasets, such as:

1.  [ATLAS](http://atlas-demo.ohdsi.org/ "ATLAS"): A web-based tool that
    enables researchers to define, execute, and share cohort definitions
    using standardized terminologies and criteria. ATLAS streamlines the
    cohort creation process by providing an intuitive interface for
    specifying cohort criteria and visualizing cohort characteristics.
2.  [SQL](https://www.geeksforgeeks.org/what-is-sql/ "What is SQL?"):
    Structured Query Language (SQL) provides a powerful means for
    defining cohorts through custom queries. OHDSI encourages the use of
    SQL for advanced cohort definition tasks and complex analyses.
3.  [CohortGenerator](https://github.com/OHDSI/CohortGenerator?tab=readme-ov-file "Cohort Generator"):
    The CohortGenerator R package is a tool within the Observational
    Health Data Sciences and Informatics (OHDSI) ecosystem designed to
    facilitate the creation of cohorts from observational healthcare
    data stored in databases adhering to the OMOP Common Data Model
    (CDM). It is an R Package that streamlines the cohort creation
    process, allowing for efficient and reproducible cohort
    identification across different datasets.

## The Cohorts Module

In the Cohorts tab of the OHDSI Analysis Viewer, there are 3 main
sections, each with their own tab, the user can explore:

1.  **Cohort Counts**: Gives the counts of both the number of subjects
    and the number of records for each cohort and each database.
2.  **Cohort Generation**: Gives the cohort generation information for
    each cohort and each database, including an indicator if the cohort
    was generated (or not), the generation start and end time, and the
    duration (in minutes) of the cohort generation.
3.  **Inclusion Rules & Attrition**: Gives both a tabular and graphical
    representation of cohort attrition statistics for each cohort and
    each database. The user may select whether they want to view the
    results at the subject or record-level.
