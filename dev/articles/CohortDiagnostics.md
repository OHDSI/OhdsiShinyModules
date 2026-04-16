# Cohort Diagnostics

## Introduction

Asessubg and diagnosing the characteristics of cohorts & phenotypes is
fundamental to ensuring the reliability and credibility of observational
research for OMOP CDM-compliant data. This is also an essential step in
phenotype development. The OHDSI community has developed an R package,
CohortDiagnostics, which provides researchers with a systematic approach
to examine various facets of cohorts, enabling them to identify
potential biases, assess data completeness and compare characteristics
of cohorts, and validate the suitability of cohorts for analysis. This
tool is crucial for researchers working within the Observational Health
Data Sciences and Informatics (OHDSI) ecosystem, enabling them to ensure
the accuracy and reliability of cohort definitions through a detailed
examination of incidence rates, cohort characteristics, and the specific
codes triggering cohort inclusion criteria. CohortDiagnostics
streamlines the process of cohort evaluation by:

1.  Generating a broad spectrum of diagnostics against a CDM database -
    see more details here: [Features and
    Functionalities](#features-and-functionalities)
2.  Providing an interactive R Shiny application within the package for
    an intuitive exploration and visualization of these diagnostics. For
    more information on R Shiny, see
    [here](https://www.rstudio.com/products/shiny/ "R Shiny").
3.  For more detailed information and documentation on
    CohortDiagnostics, visit the Github site for the package
    [here](https://ohdsi.github.io/CohortDiagnostics/index.html).

## Features and Functionalities

CohortDiagnostics offers a suite of features designed to deepen the
understanding of cohort dynamics and the intricacies of cohort
definitions, including:

1.  **Cohort Definition**: Facilitates the examination and validation of
    the logic behind cohort definitions, ensuring they accurately
    capture the intended population and allows for the asessment of
    cohort inclusion rule logic & attrition.
2.  **Concepts in Data Source**: Identifies the specific concepts
    present within the data source that are relevant to the cohort
    definitions, enabling a deeper understanding of standard and
    non-standard concepts present in the underlying patient population
    for each database.
3.  **Orphan Concepts**: Highlights concepts that, despite their
    relevance, are not captured within a cohort’s definition. This helps
    in refining concept sets and cohort criteria to ensure
    comprehensiveness and relevance.
4.  **Cohort Counts**: Provides counts of individuals and records within
    cohorts, offering a basic measure of cohort size and scope.
5.  **Incidence Rate**: Calculates the incidence rate of cohorts,
    stratified by various demographic and temporal factors such as age,
    sex, and calendar year, to assess the frequency of patients/records
    in the cohort and potential patterns over these strata.
6.  **Time Distributions**: Examines the distribution of time-related
    variables within cohorts, such as observation time before and after
    cohort index date as well as cohort duration, offering insights into
    cohort dynamics over time and available observation time.
7.  **Index Event Breakdown**: Summarizes the specific concepts (both
    standard and non-standard) that patients in the cohort are entering
    on across each database.
8.  **Visit Context**: Analyzes the healthcare context (e.g., inpatient,
    outpatient, laboratory visit) of the index events, highlighting the
    relationship between the cohort start date and the visits recorded
    in each database, both before, during, and after cohort entry.
9.  **Cohort Overlap**: Assesses the degree of patient overlap between
    cohorts, which can inform on potential biases, errors, or
    redundancies in cohort construction.
10. **Cohort Characterization**: Characterizes cohorts by detailing
    prevalent conditions, medication use, procedures, and more, to
    understand the clinical profile of cohort members over various time
    periods relative to index.
11. **Compare Cohort Characterization**: Enables the direct comparison
    of characteristics between cohorts, facilitating the identification
    of unique or shared features across different cohorts and across
    time points (both before and after index).
12. **Meta Data**: Provides meta-information about the data and analyses
    conducted.

Together, these features equip researchers with the tools necessary for
a thorough examination of cohort definitions, enhancing the quality and
reliability of observational health research.

## Utility and Application

CohortDiagnostics significantly contributes to the field of
observational health research by providing a robust framework for the
evaluation and validation of cohort definitions. Its utility spans
several critical areas:

1.  **Enhancing Cohort Definition Confidence**: By offering detailed
    diagnostics, CohortDiagnostics helps researchers refine their cohort
    definitions, ensuring they accurately capture the intended
    population. This is a critical step in phenotype development, which
    is a cornerstone of modern observational health data research.
2.  **Identifying Missing Concepts & Cohort Entry Events**: Through the
    identification of orphan concepts and the detailed breakdown of
    index events, researchers can pinpoint gaps or misspecifications in
    cohort definitions. Iterating over multiple potential cohort
    definitions after analyzing these diagnostics is an encouraged and
    common practice.
3.  **Facilitating the Ideas Behind Comparative Analyses**: The
    package’s capabilities to characterize and compare cohorts, as well
    as to analyze cohort overlaps, are invaluable for researchers
    looking to understand the nuances and dynamics of their study
    populations. These diagnostics can help inform comparative studies
    in the future, after the cohorts and phenotypes are refined and
    finalized.
4.  **Supporting Transparent Research**: By enabling the listing of
    source codes, data source information, and providing a platform for
    detailed diagnostics exploration, CohortDiagnostics fosters a
    culture of transparency and reproducibility in observational
    research.
