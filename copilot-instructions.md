# Project Overview

This project is an R package that lets users view analysis results via
an R shiny app. The package provides functions to extract the necessary
data from a results database and launch the app. The app allows users to
explore their data and results in an interactive way, making it easier
to understand and communicate their findings.

## Primary Goals

- Keep the codebase easy to understand, test, and extend for new
  contributors.
- Keep the Shiny experience intuitive for non-technical users and lay
  audiences.
- Prefer maintainable, modular, and predictable code over clever one-off
  solutions.

## Folder Structure

- `/R`: Contains R functions for the package.
- `/inst`: Contains SQL code for extracting results and an example
  result database to test the package.
- `/man`: Contains documentation for the project, created using
  roxygen2.
- `/tests`: Contains the testthat unit test for the package.
- `/vignettes`: Contains rmarkdown files for creating vignettes.

## Contribution-Friendly Design Principles

- Favor small, single-responsibility functions and modules.
- Keep business/data logic separate from UI rendering logic.
- Prefer explicit inputs and outputs (clear function signatures, no
  hidden global dependencies).
- Reuse existing helper functions before adding new abstractions.
- Keep changes minimal and focused; avoid refactoring unrelated code in
  the same PR.
- If introducing a non-obvious pattern, document the rationale in
  roxygen or vignette notes.

## Shiny App Architecture Guidelines

- Use module-oriented design for features (paired UI + server functions
  where appropriate).
- Keep server functions thin: delegate data processing to helper
  functions in `/R`.
- Validate required inputs early and fail with user-friendly messages.
- Use reactive expressions to avoid duplicate computation and improve
  clarity.
- Avoid deeply nested reactive chains when a small helper function can
  simplify intent.
- Keep SQL and database concerns isolated from view logic.

## User Experience Standards (Layperson-First)

- Write labels, titles, and help text in plain language; avoid jargon
  where possible.
- If technical terms are necessary, define them inline (short
  tooltip/help text).
- Present results in progressive detail: summary first, details on
  demand.
- Prioritize readability of tables/plots (clear titles, units, legends,
  and sensible defaults).
- Use consistent naming for concepts across tabs, modules, and
  documentation.
- Handle empty/invalid/no-data states with clear guidance on what users
  should do next.
- Prefer predictable interactions over dense control panels.

## Documentation Expectations

- All exported functions must have complete roxygen2 docs with examples
  when feasible.
- For complex modules, include a short “how it works” section in code
  comments or vignettes.
- Update `README.md` or vignettes when user-visible behavior changes.
- Keep terminology in docs aligned with UI text.

## Testing Expectations

- Add or update `testthat` tests for any behavior change in computation
  or data transformation.
- For Shiny-related logic, test helper functions and core reactive logic
  where feasible.
- Cover edge cases: missing values, empty result sets, invalid inputs,
  and boundary conditions.
- Avoid brittle snapshot-like checks unless output stability is
  intentional.

## Libraries and Frameworks

- R shiny: For building the interactive web application.
- roxygen2: For generating documentation from R code comments.
- testthat: For unit testing the package functions.

## Performance and Reliability

- Avoid repeated expensive queries or computations inside reactive
  contexts.
- Cache or memoize only when it improves responsiveness and remains easy
  to reason about.
- Surface failures with actionable error messages for users and
  developers.
- Do not silently swallow errors that hide data quality or pipeline
  issues.

## Copilot Guidance for This Repository

- Match existing naming and file organization before proposing new
  patterns.
- Generate code that is explicit and readable for new contributors.
- Prefer incremental changes over large rewrites.
- When adding a new feature, suggest where tests and docs should be
  updated in the same change.
- Do not introduce new package dependencies unless clearly justified.

## Coding Standards

- We use camelCase in R. Function and variable names all start with
  lowercase. Package names start with uppercase.
- Function names typically start with a verb. Variable names are
  typically nouns. Do not encode the data type in the variable names.
  Also, everything is data, so no need to say that unless unavoidable.
- Place spaces around all infix operators (=, +, -, \<-, etc.). The same
  rule applies when using = in function calls. Always put a space after
  a comma, and never before (just like in regular English).
- Always indent the code inside curly braces. It’s ok to leave very
  short statements on the same line.
- Use \<-, not =, for assignment.
- When calling a function that has more than one argument, make sure to
  refer to each argument by name instead of relying on the order of
  arguments.

## Pull Request Checklist (for contributors and Copilot)

- Is the change understandable by a developer new to the project?
- Are function/module responsibilities clear and focused?
- Are user-facing labels/messages plain language and consistent?
- Were tests and docs updated for behavior changes?
- Does the UI communicate key results simply before exposing advanced
  detail?
