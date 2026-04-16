# Data Sources

## Introduction

The Observational Medical Outcomes Partnership (OMOP) was a
collaborative initiative chaired by the U.S. Food and Drug
Administration (FDA) aimed at improving the understanding of how
observational studies can contribute to evaluating the safety and
effectiveness of medications. It focused on developing methods, tools,
and standards to enhance the quality and reliability of real-world
evidence generated from healthcare data. The OMOP’s legacy continues to
influence current research methodologies, particularly through the
adoption and evolution of its Common Data Model (CDM) in projects like
those undertaken by the Observational Health Data Sciences and
Informatics (OHDSI) community.

The OMOP CDM standardizes observational healthcare data, enabling the
harmonization of disparate data sources into a consistent format. This
standardization encompasses structured data schemas, controlled
vocabularies for medical terminologies, and unique identifiers for
clinical concepts, ensuring that data from varied healthcare systems and
studies can be compared and analyzed collectively. By transforming
diverse datasets into a unified model, the OMOP CDM facilitates robust,
scalable, and reproducible research across the global healthcare
research community, enhancing the reliability and validity of findings
derived from real-world data.

The Extract-Transform-Load (ETL) process is a critical data management
procedure that prepares raw data for analysis by extracting it from
various sources, transforming it into a consistent format that aligns
with analysis requirements, and loading it into a destination system for
use. In the context of generating real-world evidence (RWE) and ensuring
adherence to a CDM like OMOP, the ETL process is indispensable. It
enables the standardization of observational healthcare data—often
fragmented across different systems and formats—into a unified
structure. This standardization is essential not only for the
interoperability of data across different healthcare databases but also
for ensuring that analyses conducted on these data are reliable,
scalable, and reproducible. For more information on the ETL process and
how OHDSI tools can be utilizied to perform it for a particular data
source, please visit [this
page](https://ohdsi.github.io/TheBookOfOhdsi/ExtractTransformLoad "ETL").

## Key Data Sources for RWE Analyses

Real-world data sources that adhere to the OMOP CDM are varied and rich,
including, but not limited to:

1.  Electronic Health Records (EHRs): These are one of the primary
    sources of RWD, containing detailed patient health information from
    clinical encounters.
2.  Claims and Billing Data: These data sources provide information on
    healthcare utilization, including diagnoses, procedures, and
    medication prescriptions, primarily for billing purposes.
3.  Patient Registries: Disease or condition-specific registries offer
    valuable insights into patient outcomes, treatment patterns, and
    disease epidemiology.
4.  Wearable Devices and Mobile Health Applications: An emerging source
    of RWD, these technologies capture real-time data on patient health
    and activity levels.

## Example OMOP-mapped Datasets

The sensitivity of observational health data underscores the need for
robust privacy protection measures due to the potential identification
of individuals and the disclosure of sensitive health information. This
sensitivity arises from the richness of health data, including
diagnoses, treatments, and demographic details, which can reveal
personal health conditions and behaviors. Ensuring privacy in
observational health data is paramount to maintain trust and compliance
with regulations such as the Health Insurance Portability and
Accountability Act (HIPAA). Despite these challenges, several publicly
available data sources mapped to the OMOP CDM offer valuable insights
for research while safeguarding patient privacy. Some examples include:

1.  [HowOften](http://howoften.org/ "HowOften")
2.  [Public Datasets from
    OHDSI](http://data.ohdsi.org/ "OHDSI Datasets")
3.  [SynPUFs](https://www.cms.gov/data-research/statistics-trends-and-reports/medicare-claims-synthetic-public-use-files "Medicare Claims Synthetic Public Use Files (SynPUFs)")

## The DataSources Module

In the DataSources tab of the OHDSI Analysis Viewer, a table is
displayed to the user which gives information on each of the data
sources used in the analysis. Information such as database name, CDM
version, and the maximum observation period end date are available.
