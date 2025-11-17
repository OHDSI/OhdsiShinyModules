# Estimation

### Introduction

Observational healthcare data, comprising administrative claims and
electronic health records, present a rich source for generating
real-world evidence pertinent to treatment effects that directly impact
patient well-being. Within this realm, population-level effect
estimation assumes a pivotal role, focusing on elucidating the average
causal effects of exposures—such as medical interventions like drug
exposures or procedures—on specific health outcomes of interest.
Population-level effect estimation delves into two primary realms:
direct effect estimation and comparative effect estimation. In direct
effect estimation, the focus lies on discerning the effect of an
exposure on the risk of an outcome compared to no exposure, while
comparative effect estimation aims to delineate the effect of a target
exposure against a comparator exposure. By contrasting factual outcomes
with counterfactual scenarios—what happened versus what would have
occurred under different circumstances—these estimation tasks offer
critical insights into treatment selection, safety surveillance, and
comparative effectiveness. Whether probing individual hypotheses or
exploring multiple hypotheses concurrently, the overarching goal remains
consistent: to derive high-quality estimates of causal effects from the
intricate fabric of observational healthcare data.

## 1. CohortMethod

### Features and Functionalities

The [CohortMethod](https://ohdsi.github.io/CohortMethod/ "CohortMethod")
R package, a cornerstone of population-level estimation within the OHDSI
framework, offers a robust methodology for conducting comparative
effectiveness research and pharmacoepidemiology studies. Some of the
features offered by conducting population-level effect estimation using
the CohortMethod module are:

1.  **Data Extraction**: Extracts necessary data from databases
    structured in the OMOP Common Data Model (CDM) format, ensuring
    uniformity and compatibility across diverse healthcare settings.
2.  **Covariate Selection**: Utilizing a comprehensive set of
    covariates, including drugs, diagnoses, procedures, age, and
    comorbidity indexes, CohortMethod constructs propensity and outcome
    models tailored to specific research questions.
3.  **Large-Scale Regularized Regression**: Employing large-scale
    regularized regression techniques, CohortMethod fits propensity and
    outcome models with precision and efficiency, accommodating the
    complexities of real-world healthcare data.
4.  **Propensity Score Adjustment**: Facilitates propensity score
    adjustment through trimming, stratification, matching, and
    weighting, enabling researchers to address confounding and balance
    covariate distributions across treatment groups. Results are
    viewable both graphically and in tabular form to assess the model.
5.  **Diagnostic Functions**: Diagnostic functions within CohortMethod
    offer insights into propensity score distributions and covariate
    balance before and after matching or trimming, enhancing
    transparency and robustness in estimation procedures.
6.  **Supported Outcome Models**: Supported outcome models include
    (conditional) logistic regression, (conditional) Poisson regression,
    and (conditional) Cox regression, providing flexibility in modeling
    various types of outcomes in observational health data research.
7.  **Power**: Incorporates power analysis techniques to estimate the
    statistical power of the study design, aiding in sample size
    determination and study planning, and provides a minimum-detectable
    relative risk (MDRR) statistic.
8.  **Attrition**: Assesses attrition rates within cohorts, providing
    insights into potential biases introduced by data loss during the
    study period, and provides a visualization of attrition across
    various cohort criteria.
9.  **Population Characteristics**: Analyzes population characteristics
    to understand the demographic and clinical makeup of the study
    cohorts, informing interpretation of estimation results both before
    and after propensity score matching.
10. **Covariate Balance**: Visually monitors covariate balance before
    and after matching or trimming, ensuring that confounding variables
    are adequately controlled for in the analysis.
11. **Systematic Error**: Assesses effect size estimates for negative
    controls (true hazard ratio = 1) and positive controls (true hazard
    ratio \> 1) both before and after calibration. Estimates below the
    diagonal dashed lines are statistically significant (alpha = 0.05)
    different from the true effect size. A well-calibrated estimator
    should have the true effect size within the 95 percent confidence
    interval 95 percent of times, providing researchers with confidence
    in the reliability of the estimation process and the accuracy of the
    obtained results.

### Utility and Application

**Comparative Effectiveness Research**: CohortMethod empowers
researchers to conduct comparative effectiveness studies by estimating
treatment effects while accounting for potential confounding factors and
bias inherent in observational data.

**Pharmacoepidemiology and Drug Safety Studies**: In
pharmacoepidemiology research, CohortMethod facilitates the evaluation
of drug safety and effectiveness by quantifying the association between
drug exposures and clinical outcomes in real-world populations.

## 2. Self-Controlled Case Series

### Introduction

The Self-Controlled Case Series (SCCS) method offers a nuanced approach
to investigating the relationship between exposures and outcomes within
individual patients over time. SCCS designs are particularly adept at
comparing the rate of outcomes during times of exposure to rates during
periods of non-exposure, including before, between, and after exposure
episodes. By leveraging a Poisson regression that is conditioned on the
individual, the SCCS design inherently addresses the question: “Given
that a patient has the outcome, is the outcome more likely to occur
during exposed time compared to non-exposed time?” The design choices
outlined in the method are pivotal for defining an SCCS question, with
each choice playing a critical role in the study’s design and outcomes:

**Target Cohort**: This represents the treatment under investigation.
**Outcome Cohort**: This cohort signifies the outcome of interest.
**Time-at-Risk**: Identifies the specific times when the risk of the
outcome is considered, often relative to the start and end dates of the
target cohort. **Model**: Defines the statistical model used to estimate
the effect, including adjustments for time-varying confounders if
necessary.

One of the SCCS design’s strengths is its robustness to confounding by
factors that differ between individuals, as each participant serves as
their own control. However, it remains sensitive to time-varying
confounding factors. To mitigate this, adjustments can be made for
factors such as age, seasonality, and calendar time, enhancing the
model’s accuracy.

An advanced variant of the SCCS also considers all other drug exposures
recorded in the database, significantly expanding the model’s variables.
This approach employs L1-regularization, with cross-validation used to
select the regularization hyperparameter for all exposures except the
one of interest.

An important assumption of the SCCS is that the observation period’s end
is independent of the outcome date. This may not hold true for outcomes
that can be fatal, such as stroke. To address this, extensions to the
SCCS model have been developed that correct for any dependency between
the observation period end and the outcome.

### Features and Functionalities

The
[SelfControlledCaseSeries](https://ohdsi.github.io/SelfControlledCaseSeries/ "SCCS")
R package allows the user to perform SCCS analyses in an observational
database in the OMOP Common Data Model. Some of the features offered by
the SCCS module include:

1.  \*\*Data Extraction: Extracts necessary data from databases
    structured in the OMOP Common Data Model (CDM) format, ensuring
    uniformity and compatibility across diverse healthcare settings.
2.  **Seasonality Adjustment**: Offers the option to adjust for
    seasonality effects using a spline function, enhancing the model’s
    accuracy by accounting for seasonal variation in exposure and
    outcome rates.
3.  **Age Adjustment**: Provides the option to incorporate age
    adjustments using a spline function, allowing for more nuanced
    analyses that consider the impact of age on the exposure-outcome
    relationship.
4.  **Calendar Time Adjustment**: Enables the inclusion of calendar time
    adjustments using a spline function, addressing potential temporal
    trends in the data that could confound the exposure-outcome
    relationship.
5.  **Event-dependent Censoring Correction**: Features the ability to
    correct for event-dependent censoring of the observation period,
    ensuring that the end of the observation period is appropriately
    handled, especially in cases where it might be related to the
    outcome.
6.  **Comprehensive Covariate Inclusion**: Allows for the addition of a
    wide array of covariates in one analysis, such as all recorded drug
    exposures, facilitating a thorough examination of potential
    confounders and effect modifiers.
7.  **Risk Window Customization**: Supports the construction of various
    types of covariates and risk windows, including pre-exposure
    windows, to capture contra-indications and other relevant temporal
    patterns related to exposure and outcome.
8.  **Regularization of Covariates**: Applies regularization to all
    covariates except the outcome of interest, employing techniques like
    L1-regularization with cross-validation for selecting the
    regularization hyperparameter, thereby preventing overfitting and
    enhancing model reliability.
9.  **Self-Controlled Risk Interval Design**: Incorporates the
    self-controlled risk interval design as a specific application of
    the SCCS method, offering additional methodological flexibility for
    studying short-term effects of exposures.
10. **Power**: Incorporates power analysis techniques to estimate the
    statistical power of the study design, aiding in sample size
    determination and study planning, and provides a minimum-detectable
    relative risk (MDRR) statistic.
11. **Attrition**: Assesses attrition rates within cohorts, providing
    insights into potential biases introduced by data loss during the
    study period, and provides a visualization of attrition across
    various cohort criteria.
12. **Spanning**: Analyzes the number of subjects observed for 3
    consecutive months, providing insights into the cohort’s consistency
    and stability over time.
13. **Time Trend**: Assesses the ratio of observed to expected outcomes
    per month, with adjustments for calendar time, seasonality, and/or
    age as specified in the model, to examine time trends in the data.
14. **Time to Event**: Evaluates the number of events and subjects
    observed per week relative to the start of the first exposure,
    offering critical insights into the temporal relationship between
    exposure and outcome.
15. **Event-dependent Observation**: Provides histograms for the time
    between the first occurrence of the outcome and the end of
    observation, stratified by censored and uncensored ends of
    observation, to assess the impact of event-dependent observation
    periods.
16. **Systematic Error**: Assesses effect size estimates for negative
    controls (true hazard ratio = 1) and positive controls (true hazard
    ratio \> 1) both before and after calibration. Estimates below the
    diagonal dashed lines are statistically significant (alpha = 0.05)
    different from the true effect size. A well-calibrated estimator
    should have the true effect size within the 95 percent confidence
    interval 95 percent of times, providing researchers with confidence
    in the reliability of the estimation process and the accuracy of the
    obtained results.

### Utility and Application

The SCCS method is particularly applicable in several key areas of
epidemiological research and pharmacovigilance:

**Drug Safety Surveillance**: The SCCS method is widely used in drug
safety surveillance to identify adverse effects of medications
post-marketing. It is well-suited to detect short-term risks associated
with drug exposures, especially where the onset of the adverse event is
expected to be temporally close to the exposure.

**Vaccine Safety Evaluation**: The SCCS design is ideal for assessing
the safety of vaccines, especially in evaluating the risk of adverse
events following immunization. Its self-controlled nature helps to
address concerns about confounding by indication and other biases that
can affect observational studies in vaccine safety.

**Comparative Effectiveness Research**: While primarily designed for
evaluating the safety of medical interventions, the SCCS method can also
be adapted to compare the effectiveness of different treatments or
interventions within the same individual over time, particularly for
acute conditions.

**Epidemiological Research**: More broadly, the SCCS method is used in
epidemiological research to study the temporal relationships between
exposures and outcomes, offering insights into the causality and
mechanisms underlying health conditions and diseases.

## 3. Evidence Synthesis (Meta, Meta Analysis)

### Introduction

Meta-analysis plays a pivotal role in healthcare research by enabling
the synthesis of findings from multiple studies to draw more
generalizable conclusions. In the context of distributed health data
networks, where data are spread across various sites with diverse
populations and practices, synthesizing evidence becomes both a
challenge and a necessity. The
[EvidenceSynthesis](https://github.com/OHDSI/EvidenceSynthesis "EvidenceSynthesis")
R package addresses these challenges head-on. It offers a suite of tools
designed for combining causal effect estimates and study diagnostics
from multiple data sites, all while adhering to stringent patient
privacy requirements and navigating the complexities inherent to
observational data. This approach enhances the robustness of
meta-analytical conclusions and extends the utility of distributed
health data for research purposes.

### Features and Functionalities

The Meta module which utilizes the EvidenceSynthesis R package makes use
of the following features to summarize the results of a study:

1.  **Meta-Analysis Methods**: Facilitates both traditional
    fixed-effects and random-effects meta-analyses, accommodating
    studies with different degrees of between-site or between-database
    variability.
2.  **Forest Plot Generation**: Provides capabilities for creating
    forest plots, visual summaries that illustrate the effects estimated
    by individual studies, their confidence intervals, and the
    synthesized overall effect.
3.  **Non-Normal Likelihood Approximations**: Utilizes non-normal
    approximations for the per-data-site likelihood function to reduce
    bias in scenarios with small or zero counts, a frequent issue in
    distributed research environments.

The syntheses are generated for both Cohort Method and Self-Controlled
Case Series estimation results from the study, providing both
information on the diagnostic results within each database and the
visualized and tabular results of the meta analysis.

### Utility and Application

The EvidenceSynthesis package is instrumental in synthesizing evidence
from observational studies across multiple healthcare databases. Its
significance is underscored in scenarios characterized by:

**Comparative Effectiveness Research**: Synthesizing evidence from
disparate sources allows for stronger, more reliable comparisons of
treatment outcomes, enriching the foundation for clinical
decision-making.

**Safety Surveillance**: Aggregated safety data across databases enhance
the detection and understanding of adverse drug reactions, contributing
to safer patient care.

**Policy and Clinical Guidelines Development**: Meta-analytical findings
informed by comprehensive, real-world data can guide policy formulation
and the updating of clinical guidelines, ensuring they are grounded in
broad-based evidence.

**Addressing Challenges of Small Sample Sizes**: The EvidenceSynthesis
package notably advances the field by tackling the issue of small sample
sizes and zero event counts, which traditional meta-analytical methods
often handle poorly. Its innovative use of non-normal likelihood
approximations enables more precise effect size estimation under such
conditions, ensuring that the insights derived from meta-analyses are
both accurate and meaningful. This attribute is particularly beneficial
in distributed health data networks, where individual site/database data
may be limited but collectively hold significant informational value.

## The Estimation Module

In the Estimation tab of the OHDSI Analysis Viewer, the user can find
CohortMethod, SCCS, and/or meta-analysis results (depending on what was
included in the analysis specifications).

First, the user selects a target and an outcome of interest. After this
is completed, a table will be shown below the input selection, which has
2 parent tabs: “Diagnostics” & “Results”, and 2 child tabs:
“CohortMethod” and “SCCS”.

In the Diagnostics tab, the user can find information on whether
diagnostics passed for each database, analysis, target, and comparator
combination included in their study. These results will be shown for
CohortMethod and SCCS, respectively, based on which child tab the user
selects.

In the Results tab, the user can find more detailed information on the
specific results for each database, analysis, target, and comparator
combination included in their study. There is both a tabular and a
graphical summary of the results for both CohortMethod and SCCS included
underneath the parent Results tab. The user may click on the “View
results” link in the “Actions” column to view more detailed results for
each individual database, analysis, target, and comparator combination
included in their study. More information on what kinds of results are
shown can be found in the [1. CohortMethod](#cohortmethod) and [2.
Self-Controlled Case Series](#self-controlled-case-series) sections
above. The graphical summary renders a forest plot of the meta-analysis
results across each of the databases for the given selection.
