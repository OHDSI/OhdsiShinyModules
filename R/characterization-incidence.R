# @file characterization-timeToEvent.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
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


#' The module viewer for exploring incidence results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the description incidence module
#'
#' @export
characterizationIncidenceViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    #shinydashboard::box(
    #  collapsible = TRUE,
    #  collapsed = TRUE,
    #  title = "Incidence Rates",
    #  width = "100%",
    #  shiny::htmlTemplate(system.file("characterization-www", "help-incidenceRate.html", package = utils::packageName()))
    #  ),
    
    infoHelperViewer(
      id = "helper",
      helpLocation= system.file("characterization-www", "help-incidenceRate.html", package = utils::packageName())
    ),
    
    inputSelectionViewer(
      id = ns("input-selection")
    ),
    
    shiny::conditionalPanel(
      condition = 'input.generate != 0',
      ns = shiny::NS(ns("input-selection")),
      
      shiny::tabsetPanel(
        type = 'pills',
        id = ns('incMainPanel'),
        
        shiny::tabPanel(
          title = "Incidence Rate Table",
          resultTableViewer(ns("incidenceRateTable"),
                            downloadedFileName = "incidenceRateTable-")
        ),
        shiny::tabPanel(
          title = "Incidence Rate Plots",
          #code to view plot here
        )
      )
    )
  )
}


#' The module server for exploring incidence results 
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' @param connectionHandler the connection to the prediction result database
#' @param mainPanelTab the current tab
#' @param resultDatabaseSettings a list containing the characterization result schema, dbms, tablePrefix, databaseTable and cgTablePrefix
#' 
#' @return
#' The server to the prediction incidence module
#'
#' @export
characterizationIncidenceServer <- function(
    id, 
    connectionHandler,
    mainPanelTab,
    resultDatabaseSettings
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      cohorts <- getTargetOutcomes(
        connectionHandler,
        resultDatabaseSettings
      )
      
      # input selection component
      inputSelected <- inputSelectionServer(
        id = "input-selection", 
        inputSettingList = list(
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 6,
            varName = 'targetIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Target: ',
              choices = cohorts$targetIds,
              selected = cohorts$targetIds[1],
              multiple = T,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          ),
          createInputSetting(
            rowNumber = 1,                           
            columnWidth = 6,
            varName = 'outcomeIds',
            uiFunction = 'shinyWidgets::pickerInput',
            uiInputs = list(
              label = 'Outcome: ',
              choices = cohorts$outcomeIds,
              selected = cohorts$outcomeIds[1],
              multiple = T,
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE,
                size = 10,
                liveSearchStyle = "contains",
                liveSearchPlaceholder = "Type here to search",
                virtualScroll = 50
              )
            )
          )
        )
      )
      
      
      
      # allDataDownload <- shiny::reactiveVal(data.frame())
      # selectedInputs <- shiny::reactiveVal()
      # output$IRinputsText <- shiny::renderUI(selectedInputs())
      
      #if generate is pushed, extract the data
      allData <- shiny::reactive({
        getIncidenceData(targetIds = inputSelected()$targetIds,
                         outcomeIds = inputSelected()$outcomeIds,
                         connectionHandler = connectionHandler,
                         resultDatabaseSettings = resultDatabaseSettings
        ) %>%
          dplyr::relocate(.data$tar, .before = .data$outcomes) %>%
          dplyr::mutate(dplyr::across(dplyr::where(is.numeric), round, 4))
      })
      
      
      create_select_input <- function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
          # "All" has an empty value to clear the filter, and is the default option
          shiny::tags$option(value = "", "All"),
          lapply(unique(values), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
      
      js_code <- "
      // Custom range filter with value label
      function rangeFilter(column, state) {
        // Get min and max values from raw table data
        let min = Infinity;
        let max = 0;
        state.data.forEach(function(row) {
          const value = row[column.id];
          if (value < min) {
            min = Math.floor(value);
          } else if (value > max) {
            max = Math.ceil(value);
          }
        });
      
        const filterValue = column.filterValue || min;
        const input = React.createElement('input', {
          type: 'range',
          value: filterValue,
          min: min,
          max: max,
          onChange: function(event) {
            // Set to undefined to clear the filter
            column.setFilter(event.target.value || undefined);
          },
          style: { width: '100%', marginRight: '8px' },
          'aria-label': 'Filter ' + column.name
        });
      
        return React.createElement(
          'div',
          { style: { display: 'flex', alignItems: 'center', height: '100%' } },
          [input, filterValue]
        );
      }
      
      // Filter method that filters numeric columns by minimum value
      function filterMinValue(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return row.values[columnId] >= filterValue;
        });
      }
      "
      
      
      
      
      #read in custom column name colDef list from rds file, generated by 
      #heplers-componentsCreateCustomColDefList.R
      
      # customColDefs <- createCustomColDefList(
      #   rawColNames = names(incidenceColList),
      #   niceColNames = c("Database",
      #                    "Ref ID",
      #                    "Database ID",
      #                    "Source ID",
      #                    "Target ID",
      #                    "Target Name",
      #                    "Subgroup ID",
      #                    "Outcome ID",
      #                    "Outcome Def ID",
      #                    "Outcome Name",
      #                    "Clean Window",
      #                    "Age ID",
      #                    "Age Group",
      #                    "Gender ID",
      #                    "Gender",
      #                    "Year",
      #                    "Persons At Risk PE",
      #                    "Persons At Risk",
      #                    "Person Days PE",
      #                    "Person Days",
      #                    "Person Outcomes PE",
      #                    "Person Outcomes",
      #                    "Total Outcomes PE",
      #                    "Total Outcomes",
      #                    "Inc. Proportion Per 100P",
      #                    "Inc. Rate Per 100PY",
      #                    "Time At Risk"),
      #   tooltipText = c("The name of the database",
      #                   "The reference ID",
      #                   "The database ID",
      #                   "The source ID",
      #                   "The cohort definition ID of the target",
      #                   "The name of the target cohort",
      #                   "The name of the subgroup",
      #                   "The cohort definition ID of the outcome",
      #                   "The cohort definition ID of the outcome (duplicated)",
      #                   "The name of the outcome cohort",
      #                   "The clean window (in days)",
      #                   "The age ID",
      #                   "The age group category (in years)",
      #                   "The gender ID",
      #                   "The gender category",
      #                   "The start year of the analysis period",
      #                   "The distinct persons at risk before removing excluded time (pre-exclude) from TAR",
      #                   "The distinct persons at risk after removing excluded time from TAR",
      #                   "Total TAR (in days) before excluded time was removed (pre-exclude)",
      #                   "Total TAR (in days) after excluded time was removed",
      #                   "The distinct persons with the outcome before removing excluded time (pre-exclude) from TAR",
      #                   "The distinct persons with the outcome after removing excluded time from TAR",
      #                   "Total outcomes before removing excluded time (pre-exclude) from TAR",
      #                   "Total outcomes after removing excluded time from TAR",
      #                   "The incidence proportion (per 100 people), calculated by personOutcomes/personsAtRisk*100",
      #                   "The incidence rate (per 100 person years), calculated by outcomes/personDays/365.25*100",
      #                   "The TAR window (in days)"
      #                   ),
      #   customColDefOptions = list(
      #     list(filterInput = function(values, name) {
      #            tags$select(
      #              # Set to undefined to clear the filter
      #              onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
      #              # "All" has an empty value to clear the filter, and is the default option
      #              tags$option(value = "", "All"),
      #              lapply(unique(values), tags$option),
      #              "aria-label" = sprintf("Filter %s", name),
      #              style = "width: 100%; height: 28px;"
      #            )
      #          }),
      #     list(show = F),
      #     list(show = F),
      #     list(show = F),
      #     list(filterInput = function(values, name) {
      #       tags$select(
      #         # Set to undefined to clear the filter
      #         onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
      #         # "All" has an empty value to clear the filter, and is the default option
      #         tags$option(value = "", "All"),
      #         lapply(unique(values), tags$option),
      #         "aria-label" = sprintf("Filter %s", name),
      #         style = "width: 100%; height: 28px;"
      #       )
      #     }),
      #     list(filterInput = function(values, name) {
      #       tags$select(
      #         # Set to undefined to clear the filter
      #         onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
      #         # "All" has an empty value to clear the filter, and is the default option
      #         tags$option(value = "", "All"),
      #         lapply(unique(values), tags$option),
      #         "aria-label" = sprintf("Filter %s", name),
      #         style = "width: 100%; height: 28px;"
      #       )
      #     }),
      #     list(filterInput = function(values, name) {
      #       tags$select(
      #         # Set to undefined to clear the filter
      #         onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
      #         # "All" has an empty value to clear the filter, and is the default option
      #         tags$option(value = "", "All"),
      #         lapply(unique(values), tags$option),
      #         "aria-label" = sprintf("Filter %s", name),
      #         style = "width: 100%; height: 28px;"
      #       )
      #     }),
      #     list(filterInput = function(values, name) {
      #       tags$select(
      #         # Set to undefined to clear the filter
      #         onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
      #         # "All" has an empty value to clear the filter, and is the default option
      #         tags$option(value = "", "All"),
      #         lapply(unique(values), tags$option),
      #         "aria-label" = sprintf("Filter %s", name),
      #         style = "width: 100%; height: 28px;"
      #       )
      #     }),
      #     list(show = F),
      #     list(filterInput = function(values, name) {
      #       tags$select(
      #         # Set to undefined to clear the filter
      #         onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
      #         # "All" has an empty value to clear the filter, and is the default option
      #         tags$option(value = "", "All"),
      #         lapply(unique(values), tags$option),
      #         "aria-label" = sprintf("Filter %s", name),
      #         style = "width: 100%; height: 28px;"
      #       )
      #     }),
      #     list(filterInput = function(values, name) {
      #       tags$select(
      #         # Set to undefined to clear the filter
      #         onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
      #         # "All" has an empty value to clear the filter, and is the default option
      #         tags$option(value = "", "All"),
      #         lapply(unique(values), tags$option),
      #         "aria-label" = sprintf("Filter %s", name),
      #         style = "width: 100%; height: 28px;"
      #       )
      #     }),
      #     list(show = F),
      #     list(defaultSortOrder = "desc",
      #          filterInput = function(values, name) {
      #            tags$select(
      #              # Set to undefined to clear the filter
      #              onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
      #              # "All" has an empty value to clear the filter, and is the default option
      #              tags$option(value = "", "All"),
      #              lapply(unique(values), tags$option),
      #              "aria-label" = sprintf("Filter %s", name),
      #              style = "width: 100%; height: 28px;"
      #            )
      #          }
      #          ),
      #     list(show = F),
      #     list(defaultSortOrder = "asc",
      #          filterInput = function(values, name) {
      #            tags$select(
      #              # Set to undefined to clear the filter
      #              onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
      #              # "All" has an empty value to clear the filter, and is the default option
      #              tags$option(value = "", "All"),
      #              lapply(unique(values), tags$option),
      #              "aria-label" = sprintf("Filter %s", name),
      #              style = "width: 100%; height: 28px;"
      #            )
      #          }),
      #     list(defaultSortOrder = "desc",
      #          filterInput = function(values, name) {
      #            tags$select(
      #              # Set to undefined to clear the filter
      #              onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
      #              # "All" has an empty value to clear the filter, and is the default option
      #              tags$option(value = "", "All"),
      #              lapply(unique(values), tags$option),
      #              "aria-label" = sprintf("Filter %s", name),
      #              style = "width: 100%; height: 28px;"
      #            )
      #          }),
      #     list(NULL),
      #     list(NULL),
      #     list(NULL),
      #     list(NULL),
      #     list(NULL),
      #     list(NULL),
      #     list(NULL),
      #     list(NULL),
      #     list(NULL),
      #     list(defaultSortOrder = "desc",
      #          filterMethod = htmlwidgets::JS('filterMinValue'),
      #          filterInput = htmlwidgets::JS('rangeFilter')),
      #     list(filterInput = function(values, name) {
      #            tags$select(
      #              # Set to undefined to clear the filter
      #              onchange = sprintf("Reactable.setFilter('incidence-select', '%s', event.target.value || undefined)", name),
      #              # "All" has an empty value to clear the filter, and is the default option
      #              tags$option(value = "", "All"),
      #              lapply(unique(values), tags$option),
      #              "aria-label" = sprintf("Filter %s", name),
      #              style = "width: 100%; height: 28px;"
      #            )
      #          })
      #   )
      # )
      # 
      # # use the below as a guide to save named colDef list as JSON then read it back!
      #  ParallelLogger::saveSettingsToJson(customColDefs, "./inst/components-columnInformation/characterization-incidence-colDefs.json")
      #  loadTest <- ParallelLogger::loadSettingsFromJson("./inst/components-columnInformation/characterization-incidence-colDefs.json")
      
      
      
      incidenceColList <- ParallelLogger::loadSettingsFromJson(system.file("components-columnInformation",
                                                                           "characterization-incidence-colDefs.json",
                                                                           package = "OhdsiShinyModules")
      )
      
      resultTableServer(id = "incidenceRateTable",
                        df = allData,
                        selectedCols = c("cdmSourceAbbreviation", "targetName", "outcomeName",
                                         "ageGroupName", "genderName", "startYear", "tar", "outcomes",
                                         "incidenceProportionP100p", "incidenceRateP100py"),
                        sortedCols = c("ageGroupName", "genderName", "startYear", "incidenceRateP100py"),
                        elementId = "incidence-select",
                        colDefsInput = incidenceColList,
                        downloadedFileName = "incidenceRateTable-")
      
      return(invisible(NULL))
      
    })
}






























#allDataDownload(allData)

# do the plots reactively
#   output$incTable <- reactable::renderReactable(
#     {
#       reactable::reactable(
#         data = allData %>% 
#           dplyr::relocate("tar", .after = "cdmSourceAbbreviation") %>%
#           dplyr::relocate("personsAtRisk", .after = "tar") %>% 
#           dplyr::relocate("personDays", .after = "personsAtRisk") %>% 
#           dplyr::relocate("personOutcomes", .after = "personDays") %>% 
#           dplyr::relocate("incidenceProportionP100p", .after = "personOutcomes") %>% 
#           dplyr::relocate("incidenceRateP100py", .after = "incidenceProportionP100p") 
#           ,
#         filterable = TRUE,
#         showPageSizeOptions = TRUE,
#         pageSizeOptions = c(10, 50, 100,1000),
#         defaultPageSize = 50,
#         striped = TRUE,
#         highlight = TRUE,
#         elementId = "desc-incidence-select",
#         
#         columns = list(
#           cdmSourceAbbreviation = reactable::colDef( 
#             name = 'Database',
#             sticky = "left",
#             filterInput = function(values, name) {
#               shiny::tags$select(
#                 # Set to undefined to clear the filter
#                 onchange = sprintf("Reactable.setFilter('desc-incidence-select', '%s', event.target.value || undefined)", name),
#                 # "All" has an empty value to clear the filter, and is the default option
#                 shiny::tags$option(value = "", "All"),
#                 lapply(unique(values), shiny::tags$option),
#                 "aria-label" = sprintf("Filter %s", name),
#                 style = "width: 100%; height: 28px;"
#               )
#             }
#           ),
#           tar = reactable::colDef( 
#             filterInput = function(values, name) {
#               shiny::tags$select(
#                 # Set to undefined to clear the filter
#                 onchange = sprintf("Reactable.setFilter('desc-incidence-select', '%s', event.target.value || undefined)", name),
#                 # "All" has an empty value to clear the filter, and is the default option
#                 shiny::tags$option(value = "", "All"),
#                 lapply(unique(values), shiny::tags$option),
#                 "aria-label" = sprintf("Filter %s", name),
#                 style = "width: 100%; height: 28px;"
#               )
#             }
#           ),
#           refId = reactable::colDef(show = F),
#           databaseId = reactable::colDef(show = F),
#           sourceName = reactable::colDef(show = F),
#           targetCohortDefinitionId = reactable::colDef(show = F),
#           targetName = reactable::colDef(show = F),
#           outcomeId = reactable::colDef(show = F),
#           outcomeCohortDefinitionId = reactable::colDef(show = F),
#           outcomeName = reactable::colDef(show = F),
#           outcomeId = reactable::colDef(show = F),
#           ageId = reactable::colDef(show = F),
#           genderId = reactable::colDef(show = F),
#           subgroupId = reactable::colDef(show = F),
#           incidenceProportionP100p = reactable::colDef(
#             format = reactable::colFormat(digits = 4)
#           ),
#           incidenceRateP100py = reactable::colDef(
#             format = reactable::colFormat(digits = 4)
#           )
#         )
#         
#         
#         
#         
#       )
#     }
#   )
#   
# }
#       )
#       
#       # download
#       output$downloadInc <- shiny::downloadHandler(
#         filename = function() {
#           paste('incidence-data-', Sys.Date(), '.csv', sep='')
#         },
#         content = function(con) {
#           utils::write.csv(allDataDownload(), con)
#         }
#       )
#     
#       
#       return(invisible(NULL))
#       
#     }
#   )
# }

getIncidenceData <- function(
    targetIds,
    outcomeIds,
    connectionHandler,
    resultDatabaseSettings
){
  
  #shiny::withProgress(message = 'Getting incidence data', value = 0, {
  
  sql <- 'select d.cdm_source_abbreviation, i.* 
    from @result_schema.@incidence_table_prefixINCIDENCE_SUMMARY i
    inner join @result_schema.@database_table_name d
    on d.database_id = i.database_id
    where target_cohort_definition_id in (@target_ids)
    and outcome_cohort_definition_id in (@outcome_ids)
    ;'
  
  #shiny::incProgress(1/2, detail = paste("Created SQL - Extracting..."))
  
  resultTable <- connectionHandler$queryDb(
    sql = sql, 
    result_schema = resultDatabaseSettings$schema,
    incidence_table_prefix = resultDatabaseSettings$incidenceTablePrefix,
    target_ids = paste(as.double(targetIds), collapse = ','),
    outcome_ids = paste(as.double(outcomeIds), collapse = ','),
    database_table_name = resultDatabaseSettings$databaseTable
  )
  
  #shiny::incProgress(2/2, detail = paste("Done..."))
  
  #})
  
  # format the tar
  resultTable$tar <- paste0('(',resultTable$tarStartWith, " + ", resultTable$tarStartOffset, ') - (', resultTable$tarEndWith, " + ", resultTable$tarEndOffset, ')')
  resultTable <- resultTable %>% 
    dplyr::select(-c("tarStartWith","tarStartOffset","tarEndWith","tarEndOffset", "tarId", "subgroupName"))
  
  resultTable[is.na(resultTable)] <- 'All'
  resultTable <- unique(resultTable)
  
  return(resultTable)
}


getTargetOutcomes <- function(
    connectionHandler,
    resultDatabaseSettings
){
  
  shiny::withProgress(message = 'Getting incidence inputs', value = 0, {
    
    sql <- 'select distinct target_cohort_definition_id, target_name 
  from @result_schema.@incidence_table_prefixINCIDENCE_SUMMARY;'
    
    shiny::incProgress(1/3, detail = paste("Created SQL - Extracting targets"))
    
    targets <- connectionHandler$queryDb(
      sql = sql, 
      result_schema = resultDatabaseSettings$schema,
      incidence_table_prefix = resultDatabaseSettings$incidenceTablePrefix
    )
    targetIds <- targets$targetCohortDefinitionId
    names(targetIds) <- targets$targetName
    
    sql <- 'select distinct outcome_cohort_definition_id, outcome_name 
  from @result_schema.@incidence_table_prefixINCIDENCE_SUMMARY;'
    
    shiny::incProgress(2/3, detail = paste("Created SQL - Extracting outcomes"))
    
    outcomes <- connectionHandler$queryDb(
      sql = sql, 
      result_schema = resultDatabaseSettings$schema,
      incidence_table_prefix = resultDatabaseSettings$incidenceTablePrefix
    )
    
    outcomeIds <- outcomes$outcomeCohortDefinitionId
    names(outcomeIds) <- outcomes$outcomeName
    
    shiny::incProgress(3/3, detail = paste("Done"))
  })
  
  return(
    list(
      targetIds = targetIds,
      outcomeIds = outcomeIds
    )
  )
  
}
