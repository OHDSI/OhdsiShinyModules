# @file selfControlledCohort-largeQueries.R
#
# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of OhdsiShinyModules
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# The parameterized SQL used by the large (server side paginated) tables.
# The SqlRender @params are rendered on every page request so the filters can
# be changed without re-instantiating the LargeDataTable.  Only sqlite /
# postgresql backends are supported by the LargeDataTable component (it
# appends LIMIT / OFFSET to the query).

#' Build the signal discovery SQL
#'
#' @description
#' Returns a parameterized SQL query (no limit / offset) for the signal
#' discovery grid.  The query only counts databases that have unblinded the
#' exposure-outcome pair and uses a single (most recent) evidence synthesis
#' analysis for the meta analytic columns so each exposure-outcome pair is
#' returned once
#'
#' @details
#' The query is rendered by the LargeDataTable with the parameters passed
#' through the largeTableServer inputParams (schema, table prefixes,
#' analysisId, benefitRr, lowerBenefitRr, riskRr, pCut, filterByMeta,
#' minBenefitSources, maxRiskSources, targetSearch, outcomeSearch)
#'
#' @param schema the schema containing the results
#' @param sccTablePrefix the self controlled cohort table prefix
#' @param cgTablePrefix the cohort generator table prefix
#' @param esTablePrefix the evidence synthesis table prefix
#' @family SelfControlledCohort
#' @return a string containing the parameterized sql
#' @export
selfControlledCohortSignalsSql <- function(
    schema,
    sccTablePrefix = "scc_",
    cgTablePrefix = "cg_",
    esTablePrefix = "es_"
) {
  sql <- "
  WITH pair_sources AS (
    SELECT
      sr.target_cohort_id,
      sr.outcome_cohort_id,
      sr.database_id,
      CASE WHEN COALESCE(sdun.diagnostic_value, 0) = 0 THEN NULL
           ELSE sr.calibrated_rr END AS measure_rr,
      CASE WHEN COALESCE(sdun.diagnostic_value, 0) = 0 THEN NULL
           ELSE sr.calibrated_p_value END AS measure_p
    FROM @schema.@scc_table_prefixresult sr
    LEFT JOIN @schema.@scc_table_prefixdiagnostics_summary sdun ON (
      sdun.database_id = sr.database_id AND
      sdun.analysis_id = sr.analysis_id AND
      sdun.target_cohort_id = sr.target_cohort_id AND
      sdun.outcome_cohort_id = sr.outcome_cohort_id AND
      sdun.diagnostic_name = 'UNBLIND'
    )
    WHERE sr.analysis_id = @analysis_id
  ),
  benefit_t AS (
    SELECT
      target_cohort_id,
      outcome_cohort_id,
      COUNT(DISTINCT database_id) AS benefit_count
    FROM pair_sources
    WHERE measure_rr <= @benefit_rr AND measure_rr >= @lower_benefit_rr
      AND measure_p < @p_cut
    GROUP BY target_cohort_id, outcome_cohort_id
  ),
  risk_t AS (
    SELECT
      target_cohort_id,
      outcome_cohort_id,
      COUNT(DISTINCT database_id) AS risk_count
    FROM pair_sources
    WHERE measure_rr >= @risk_rr AND measure_p < @p_cut
    GROUP BY target_cohort_id, outcome_cohort_id
  ),
  meta_t AS (
    SELECT
      esr.target_cohort_id,
      esr.outcome_cohort_id,
      CASE WHEN COALESCE(esds.unblind, 0) = 0 THEN NULL
           ELSE esr.calibrated_rr END AS meta_rr,
      CASE WHEN COALESCE(esds.unblind, 0) = 0 THEN NULL
           ELSE esr.calibrated_p END AS meta_p,
      esds.i_2 AS i2,
      esr.n_databases
    FROM @schema.@es_table_prefixscc_result esr
    INNER JOIN @schema.@es_table_prefixscc_diagnostics_summary esds ON (
      esds.target_cohort_id = esr.target_cohort_id AND
      esds.outcome_cohort_id = esr.outcome_cohort_id AND
      esds.analysis_id = esr.analysis_id AND
      esds.evidence_synthesis_analysis_id = esr.evidence_synthesis_analysis_id
    )
    WHERE esr.analysis_id = @analysis_id
      AND esr.evidence_synthesis_analysis_id = (
        SELECT MAX(esr2.evidence_synthesis_analysis_id)
        FROM @schema.@es_table_prefixscc_result esr2
        WHERE esr2.target_cohort_id = esr.target_cohort_id
          AND esr2.outcome_cohort_id = esr.outcome_cohort_id
          AND esr2.analysis_id = esr.analysis_id
      )
  )
  SELECT
    fr.target_cohort_id || '|' || fr.outcome_cohort_id AS pair_key,
    fr.target_cohort_id AS target_id,
    cgt.cohort_name AS target_name,
    fr.outcome_cohort_id AS outcome_id,
    cgo.cohort_name AS outcome_name,
    COALESCE(bt.benefit_count, 0) AS benefit_count,
    COALESCE(rt.risk_count, 0) AS risk_count,
    mt.meta_rr,
    mt.meta_p,
    mt.i2,
    mt.n_databases
  FROM (
    SELECT DISTINCT rs.target_cohort_id, rs.outcome_cohort_id
    FROM @schema.@scc_table_prefixresult rs
    INNER JOIN @schema.@scc_table_prefixoutcome_exposure oex ON (
      oex.target_cohort_id = rs.target_cohort_id AND
      oex.outcome_cohort_id = rs.outcome_cohort_id AND
      oex.true_effect_size IS NULL
    )
    WHERE rs.analysis_id = @analysis_id
  ) fr
  INNER JOIN @schema.@cg_table_prefixcohort_definition cgt
    ON cgt.cohort_definition_id = fr.target_cohort_id
  INNER JOIN @schema.@cg_table_prefixcohort_definition cgo
    ON cgo.cohort_definition_id = fr.outcome_cohort_id
  LEFT JOIN benefit_t bt ON
    bt.target_cohort_id = fr.target_cohort_id AND
    bt.outcome_cohort_id = fr.outcome_cohort_id
  LEFT JOIN risk_t rt ON
    rt.target_cohort_id = fr.target_cohort_id AND
    rt.outcome_cohort_id = fr.outcome_cohort_id
  LEFT JOIN meta_t mt ON
    mt.target_cohort_id = fr.target_cohort_id AND
    mt.outcome_cohort_id = fr.outcome_cohort_id
  WHERE 1 = 1
    AND lower(cgt.cohort_name) LIKE '%' || lower('@target_search') || '%'
    AND lower(cgo.cohort_name) LIKE '%' || lower('@outcome_search') || '%'
    AND (
      CASE WHEN @filter_by_meta = 1 THEN
        mt.meta_rr <= @benefit_rr AND mt.meta_rr >= @lower_benefit_rr
        AND mt.meta_p < @p_cut AND mt.meta_rr IS NOT NULL
      ELSE
        COALESCE(bt.benefit_count, 0) >= @min_benefit_sources
        AND COALESCE(rt.risk_count, 0) <= @max_risk_sources
      END
    )
  ORDER BY fr.target_cohort_id ASC, fr.outcome_cohort_id ASC"

  return(sql)
}

#' Build the meta analytic pair exploration SQL
#'
#' @description
#' Returns a parameterized SQL query (no limit / offset) for the meta analytic
#' target-outcome pair exploration table.  The study diagnostic status is
#' computed in the query and effect estimates are masked (returned as NULL) for
#' evidence synthesis analyses that failed a diagnostic or are blinded
#'
#' @details
#' The query is rendered by the LargeDataTable with the parameters passed
#' through the largeTableServer inputParams (schema, table prefixes,
#' analysisId and status where status is one of 'All', 'Pass' or 'Fail')
#'
#' @param schema the schema containing the results
#' @param sccTablePrefix the self controlled cohort table prefix
#' @param cgTablePrefix the cohort generator table prefix
#' @param esTablePrefix the evidence synthesis table prefix
#' @family SelfControlledCohort
#' @return a string containing the parameterized sql
#' @export
selfControlledCohortMetaSql <- function(
    schema,
    sccTablePrefix = "scc_",
    cgTablePrefix = "cg_",
    esTablePrefix = "es_"
) {
  sql <- "
  WITH meta_t AS (
    SELECT
      esr.target_cohort_id,
      esr.outcome_cohort_id,
      esr.analysis_id,
      esr.evidence_synthesis_analysis_id,
      ev.evidence_synthesis_description AS database_name,
      esr.num_persons,
      esr.time_at_risk_exposed,
      esr.time_at_risk_unexposed,
      esr.num_outcomes_exposed,
      esr.num_outcomes_unexposed,
      esr.num_exposures,
      esr.n_databases,
      esds.mdrr,
      esds.i_2 AS i2,
      esds.tau,
      esds.ease,
      esds.unblind,
      CASE WHEN esds.mdrr_diagnostic = 'FAIL' OR esds.i_2_diagnostic = 'FAIL'
                OR esds.tau_diagnostic = 'FAIL' OR esds.ease_diagnostic = 'FAIL'
           THEN 'Fail' ELSE 'Pass' END AS overall_status,
      CASE WHEN esds.mdrr_diagnostic = 'FAIL' OR esds.i_2_diagnostic = 'FAIL'
                OR esds.tau_diagnostic = 'FAIL' OR esds.ease_diagnostic = 'FAIL'
                OR COALESCE(esds.unblind, 0) <> 1
           THEN NULL ELSE esr.calibrated_rr END AS calibrated_rr,
      CASE WHEN esds.mdrr_diagnostic = 'FAIL' OR esds.i_2_diagnostic = 'FAIL'
                OR esds.tau_diagnostic = 'FAIL' OR esds.ease_diagnostic = 'FAIL'
                OR COALESCE(esds.unblind, 0) <> 1
           THEN NULL ELSE esr.calibrated_ci_95_lb END AS calibrated_ci_95_lb,
      CASE WHEN esds.mdrr_diagnostic = 'FAIL' OR esds.i_2_diagnostic = 'FAIL'
                OR esds.tau_diagnostic = 'FAIL' OR esds.ease_diagnostic = 'FAIL'
                OR COALESCE(esds.unblind, 0) <> 1
           THEN NULL ELSE esr.calibrated_ci_95_ub END AS calibrated_ci_95_ub
    FROM @schema.@es_table_prefixscc_result esr
    INNER JOIN @schema.@es_table_prefixscc_diagnostics_summary esds ON (
      esds.target_cohort_id = esr.target_cohort_id AND
      esds.outcome_cohort_id = esr.outcome_cohort_id AND
      esds.analysis_id = esr.analysis_id AND
      esds.evidence_synthesis_analysis_id = esr.evidence_synthesis_analysis_id
    )
    INNER JOIN @schema.@es_table_prefixanalysis ev
      ON ev.evidence_synthesis_analysis_id = esr.evidence_synthesis_analysis_id
    WHERE esr.analysis_id = @analysis_id
  )
  SELECT
    mt.target_cohort_id || '|' || mt.outcome_cohort_id AS pair_key,
    mt.database_name AS database_name,
    mt.evidence_synthesis_analysis_id,
    mt.analysis_id,
    a.description,
    mt.target_cohort_id AS target_id,
    cgt.cohort_name AS target_name,
    mt.outcome_cohort_id AS outcome_id,
    cgo.cohort_name AS outcome_name,
    mt.num_persons,
    mt.time_at_risk_exposed,
    mt.time_at_risk_unexposed,
    mt.num_outcomes_exposed,
    mt.num_outcomes_unexposed,
    mt.num_exposures,
    mt.n_databases,
    mt.mdrr,
    mt.i2,
    mt.tau,
    mt.ease,
    mt.unblind,
    mt.overall_status,
    mt.calibrated_rr,
    mt.calibrated_ci_95_lb,
    mt.calibrated_ci_95_ub
  FROM meta_t mt
  INNER JOIN @schema.@cg_table_prefixcohort_definition cgt
    ON cgt.cohort_definition_id = mt.target_cohort_id
  INNER JOIN @schema.@cg_table_prefixcohort_definition cgo
    ON cgo.cohort_definition_id = mt.outcome_cohort_id
  INNER JOIN @schema.@scc_table_prefixanalysis_setting a
    ON a.analysis_id = mt.analysis_id
  WHERE 1 = 1
    AND ('@status_value' = 'All' OR mt.overall_status = '@status_value')
  ORDER BY mt.target_cohort_id ASC, mt.outcome_cohort_id ASC,
           mt.evidence_synthesis_analysis_id ASC"

  return(sql)
}
