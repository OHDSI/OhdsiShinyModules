# Screenshots — index event breakdown by cohort start year

Reference images for OHDSI/CohortDiagnostics#1197.

## How these were captured

They are screenshots of the **real `indexEventBreakdownModule`** rendered in a
headless Chrome browser, served by a minimal standalone Shiny app that mounts
only that module. The data source is the package's own
`tests/resources/cdDatabase/databaseFile.sqlite` fixture, with a
`cohort_start_year` column added for the year-stratified images.

They are therefore representative of the module's behaviour, but they are **not**
a screenshot of a full DiagnosticsExplorer running against a real CDM.

## Images

| File | Shows |
|---|---|
| `01-all-years.png` | Year selector set to "All years", the new Cohort Start Year column, and the trend plot |
| `02-filtered-2021.png` | Filtered to 2021: the table narrows to two rows while the plot keeps the full 2018-2022 trend |
| `03-legacy-backwards-compatible.png` | Results without the `cohort_start_year` column: the selector, column and plot are all absent |

`03` shows "There is no data for the selected combination" because the legacy
fixture has no index event rows that join to `concept` for the selected cohort —
this is a property of the fixture, not of the display change.
