# Table-based input selector module.
# Shows a button that opens a modal with a table; the user selects one or more
# rows, which are then displayed as compact tags below the button.
#
# Inputs:
#   table          - reactive data.frame of options
#   selectedRowId  - reactive integer vector of selected row indices
# Output:
#   selectedRowId is updated when the user confirms a selection.

tableSelectionViewer <- function(id = "input-selection") {
  ns <- shiny::NS(id)
  shiny::div(
    shiny::uiOutput(ns('selectionInput'))
  )
}


tableSelectionServer <- function(
    id,
    table,           # must be reactive
    selectedRowId,   # must be reactive
    helpText = 'Click the button to make your selection',
    selectMultiple = FALSE,
    inputColumns = NULL,
    displayColumns = inputColumns,
    elementId = NULL,
    selectButtonText = 'Select Option',
    tableReset = shiny::reactive(0),
    groupBy = NULL,
    columnGroups = NULL
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      selection <- ifelse(selectMultiple, 'multiple', 'single')

      # Track which chips are expanded (by their index in the current selection)
      expandedChipIndices <- shiny::reactiveVal(integer())
      
      # Reset when tableReset fires
      shiny::observeEvent(tableReset(), {
        expandedChipIndices(integer())
        tryCatch(selectedRowId(NULL), error = function(e) {})
      })

      # ---- helpers -----------------------------------------------------------

      # Pick the best column(s) to use as the chip label for a selected row.
      # Prefers columns whose name contains "name" or "label", then falls back
      # to the first visible display column.
      chipLabelForRow <- function(rowData) {
        cols <- colnames(rowData)
        if (is.null(cols) || length(cols) == 0) {
          return("(no data)")
        }
        nameCols <- cols[grepl("name|label", cols, ignore.case = TRUE)]
        labelCols <- if (length(nameCols) > 0) nameCols[seq_len(min(2, length(nameCols)))] else cols[1]

        parts <- vapply(labelCols, function(col) {
          if (!(col %in% colnames(rowData))) {
            return("")
          }
          humanLabel <- NULL
          if (!is.null(displayColumns) && !is.null(displayColumns[[col]])) {
            humanLabel <- displayColumns[[col]]$name
          }
          value <- as.character(rowData[[col]])
          if (!is.null(humanLabel) && nzchar(humanLabel)) {
            paste0(humanLabel, ": ", value)
          } else {
            value
          }
        }, character(1))

        result <- paste(parts, collapse = " \u00b7 ")
        if (nzchar(result)) result else "(no label)"
      }

      # Build the expanded tooltip text showing all display columns
      buildExpandedText <- function(rowData, displayCols) {
        if (is.null(displayCols) || length(displayCols) == 0) {
          return("")
        }

        parts <- character(0)
        for (colName in names(displayCols)) {
          if (colName %in% colnames(rowData)) {
            colDef <- displayCols[[colName]]
            humanLabel <- if (!is.null(colDef) && !is.null(colDef$name)) {
              colDef$name
            } else {
              colName
            }
            value <- as.character(rowData[[colName]])
            parts <- c(parts, paste0(humanLabel, ": ", value))
          }
        }

        paste(parts, collapse = " - ")
      }

      # Build one pill/chip tag for a selected row
      makeChip <- function(label, expandedText = "", chipIndex = NULL) {
        isExpanded <- !is.null(chipIndex) && chipIndex %in% expandedChipIndices()
        hasExpandableContent <- nzchar(expandedText)

        shiny::tags$span(
          style = paste0(
            "display: inline-flex; align-items: center; gap: 5px; ",
            "background-color: ", if (hasExpandableContent) "#dceee5" else "#eaf6ee", "; ",
            "color: #1a6632; ",
            "border: 1px solid ", if (hasExpandableContent) "#7eb99f" else "#a9d9b6", "; ",
            "border-radius: 999px; ",
            "padding: 3px 12px 3px 9px; margin: 3px 3px 3px 0; ",
            "font-size: 0.82em; font-weight: 500; line-height: 1.4; ",
            if (hasExpandableContent) "cursor: pointer;" else ""
          ),
          shiny::icon("check-circle"),
          if (isExpanded && hasExpandableContent) {
            # Show expanded text
            shiny::tags$a(
              href = "#",
              onclick = paste0(
                "Shiny.onInputChange('",
                session$ns("toggleChip"), "', Math.random()); ",
                "Shiny.setInputValue('", session$ns("chipIndex"), "', ", chipIndex, "); ",
                "return false;"
              ),
              style = "color: #1a6632; text-decoration: none; display: flex; align-items: center; gap: 4px;",
              expandedText,
              shiny::icon("chevron-up", style = "font-size: 0.75em;")
            )
          } else {
            # Show short label with click handler if expandedText available
            if (hasExpandableContent) {
              shiny::tags$a(
                href = "#",
                onclick = paste0(
                  "Shiny.onInputChange('",
                  session$ns("toggleChip"), "', Math.random()); ",
                  "Shiny.setInputValue('", session$ns("chipIndex"), "', ", chipIndex, "); ",
                  "return false;"
                ),
                style = "color: #1a6632; text-decoration: none; display: flex; align-items: center; gap: 4px;",
                label,
                shiny::icon("chevron-down", style = "font-size: 0.75em;")
              )
            } else {
              label
            }
          }
        )
      }

      # Render the chips (or a placeholder) for the currently selected rows
      selectedChips <- function(rowIds, tbl) {
        if (is.null(rowIds) || length(rowIds) == 0 || sum(rowIds) == 0) {
          return(
            shiny::div(
              style = paste0(
                "color: #888; font-size: 0.85em; font-style: italic; ",
                "margin-top: 6px; padding: 6px 10px; ",
                "border: 1px dashed #ccc; border-radius: 6px; ",
                "background: #fafafa; display: inline-block;"
              ),
              shiny::icon("arrow-pointer"),
              " No selection yet \u2014 click the button above to choose."
            )
          )
        }

        if (is.null(tbl)) {
          return(NULL)
        }
        tblData <- tryCatch(tbl(), error = function(e) {NULL})
        if (is.null(tblData)) {
          return(NULL)
        }
        if (nrow(tblData) == 0) {
          return(NULL)
        }
        selected <- tblData[rowIds, , drop = FALSE]
        if (nrow(selected) == 0) {
          return(NULL)
        }
        chips <- lapply(seq_len(nrow(selected)), function(i) {
          rowData <- selected[i, , drop = FALSE]
          label <- chipLabelForRow(rowData)
          expandedText <- buildExpandedText(rowData, displayColumns)
          makeChip(label, expandedText, chipIndex = i)
        })

        shiny::div(
          style = "margin-top: 6px;",
          shiny::div(
            style = paste0(
              "display: flex; align-items: center; gap: 8px; ",
              "margin-bottom: 4px;"
            ),
            shiny::div(
              style = paste0(
                "font-size: 0.75em; font-weight: 600; color: #555; ",
                "text-transform: uppercase; letter-spacing: 0.06em;"
              ),
              if (selectMultiple) "Selected items:" else "Selected:"
            ),
            shiny::span(
              style = paste0(
                "font-size: 0.7em; color: #888; font-style: italic; ",
                "padding: 2px 6px; background: #fafafa; border-radius: 4px;"
              ),
              shiny::icon("hand-pointer", style = "font-size: 0.8em;"),
              " Click to expand"
            )
          ),
          shiny::div(chips)
        )
      }

      # Build the full selection widget UI (button + chips area)
      buildSelectionUI <- function(hasSelection, rowIds, tbl) {
        btnStyle <- if (hasSelection) {
          paste0(
            "background-color: #27ae60; border-color: #219a52; color: #fff; ",
            "font-weight: 600;"
          )
        } else {
          paste0(
            "background-color: #2980b9; border-color: #2471a3; color: #fff; ",
            "font-weight: 600;"
          )
        }

        btnIcon  <- if (hasSelection) shiny::icon("pen-to-square") else shiny::icon("table-list")
        btnLabel <- if (hasSelection) paste("Change", selectButtonText) else selectButtonText

        shiny::div(
          style = "padding: 4px 0;",
          # Row: button + help text (only shown when nothing selected)
          shiny::div(
            style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
            shiny::actionButton(
              inputId = session$ns('openModal'),
              icon    = btnIcon,
              label   = btnLabel,
              style   = btnStyle
            ),
            if (!hasSelection) {
              shiny::span(
                style = "color: #666; font-size: 0.88em; font-style: italic;",
                helpText
              )
            }
          ),
          # Chips area below the button
          selectedChips(rowIds, tbl)
        )
      }

      # ---- main reactive UI --------------------------------------------------

      output$selectionInput <- shiny::renderUI({
        tblData <- tryCatch(table(), error = function(e) {NULL})
        if (is.null(tblData)) {
          return(NULL)
        }
        rowIds <- tryCatch(selectedRowId(), error = function(e) {NULL})
        if (is.null(rowIds)) rowIds <- integer()
        hasSelection <- !is.null(rowIds) && length(rowIds) > 0 && sum(rowIds) > 0
        buildSelectionUI(hasSelection, rowIds, table)
      })

      # Toggle expanded state when a chip is clicked
      shiny::observeEvent(input$toggleChip, {
        chipIdx <- input$chipIndex
        if (!is.null(chipIdx)) {
          current <- expandedChipIndices()
          if (chipIdx %in% current) {
            expandedChipIndices(setdiff(current, chipIdx))
          } else {
            expandedChipIndices(c(current, chipIdx))
          }
        }
      })

      # ---- modal -------------------------------------------------------------

      shiny::observeEvent(input$openModal, {

        instructionText <- if (selectMultiple) {
          paste0(
            "Click one or more rows to highlight them, then press ",
            "<strong>Confirm Selection</strong> to apply your choices."
          )
        } else {
          paste0(
            "Click a row to highlight it, then press ",
            "<strong>Confirm Selection</strong> to apply your choice."
          )
        }

        shiny::showModal(
          shiny::modalDialog(
            size      = 'l',
            easyClose = TRUE,
            title = shiny::div(
              style = "display: flex; align-items: center; gap: 8px;",
              shiny::icon("table-list"),
              selectButtonText
            ),
            shiny::div(
              class = "alert alert-info",
              style = "margin-bottom: 12px; display: flex; align-items: flex-start; gap: 8px;",
              shiny::icon("circle-info"),
              shiny::HTML(instructionText)
            ),
            resultTableViewer(
              id       = session$ns("input-table"),
              boxTitle = NULL
            ),
            footer = shiny::tagList(
              shiny::modalButton(
                shiny::tagList(shiny::icon("xmark"), "Cancel")
              ),
              shiny::actionButton(
                inputId = session$ns("confirmInput"),
                label   = shiny::tagList(shiny::icon("check"), "Confirm Selection"),
                style   = "background-color: #2980b9; border-color: #2471a3; color: #fff; font-weight: 600;"
              )
            )
          )
        )

        oldSetSelected <- setSelected()
        setSelected(oldSetSelected + 1)
      })

      # ---- table server ------------------------------------------------------

      setSelected <- shiny::reactiveVal(0)
      getSelected <- shiny::reactiveVal(0)

      resultTableServer(
        id                = "input-table",
        df                = table,
        colDefsInput      = inputColumns,
        columnGroups      = columnGroups,
        details           = data.frame(),
        selectedCols      = NULL,
        elementId         = elementId,
        addActions        = NULL,
        downloadedFileName = NULL,
        groupBy           = groupBy,
        selection         = selection,
        getSelected       = getSelected,
        selectedRowId     = selectedRowId,
        setSelected       = setSelected,
        showPageSizeOptions = TRUE,
        pageSizeOptions   = c(5, 25, 50, 500),
        defaultPageSize   = 5
      )

      # Close the modal and commit the selection
      shiny::observeEvent(input$confirmInput, {
        shiny::removeModal()
        oldCount <- getSelected()
        getSelected(oldCount + 1)
      })

    }
  )
}