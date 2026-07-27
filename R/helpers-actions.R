#' Create Action Button Definition
#'
#' Build an action button configuration for resultTableServer(addActions = ...).
#'
#' @param actionType string, action identifier emitted by the button click event.
#' @param buttonIcon string, Font Awesome icon name (for shiny::icon). Use "" to hide the icon.
#' @param hoverText string, button tooltip shown on hover.
#' @param buttonClass string, CSS classes applied to the button element.
#' @param buttonStyle string, inline CSS style applied to the button element.
#' @param buttonLabel string, button text shown to the user.
#'
#' @return A list with action button settings.
#' @export
createActionButton <- function(
    actionType,
    buttonIcon = "play",
    hoverText = NULL,
    buttonClass = "btn btn-default btn-xs",
    buttonStyle = "margin-right: 4px; margin-bottom: 2px; padding: 2px 8px; font-size: 11px; line-height: 1.2;",
    buttonLabel = NULL
) {
  if (is.null(actionType) || length(actionType) != 1 || !nzchar(actionType)) {
    stop("actionType must be a single non-empty string.")
  }

  if (is.null(hoverText)) {
    hoverText <- paste0("Run action: ", actionType)
  }

  if (is.null(buttonLabel)) {
    buttonLabel <- paste0("View ", actionType)
  }

  list(
    actionType = as.character(actionType),
    buttonIcon = as.character(buttonIcon),
    hoverText = as.character(hoverText),
    buttonClass = as.character(buttonClass),
    buttonStyle = as.character(buttonStyle),
    buttonLabel = as.character(buttonLabel)
  )
}

#' Action Button Style (Info)
#'
#' Shared styling for neutral/info action pills used in result tables.
#'
#' @return A CSS style string.
#' @export
actionButtonStyleInfo <- function() {
  paste0(
    "margin-right: 4px; margin-bottom: 2px; ",
    "padding: 3px 10px; border-radius: 999px; ",
    "border: 1px solid #86b9f4; background: #eef6ff; ",
    "color: #0f4c81; font-weight: 600; letter-spacing: 0.01em;"
  )
}

#' Action Button Style (Warning)
#'
#' Shared styling for warning/attention action pills used in result tables.
#'
#' @return A CSS style string.
#' @export
actionButtonStyleWarning <- function() {
  paste0(
    "margin-right: 4px; margin-bottom: 2px; ",
    "padding: 3px 10px; border-radius: 999px; ",
    "border: 1px solid #f0b27a; background: #fff5eb; ",
    "color: #a64200; font-weight: 600; letter-spacing: 0.01em;"
  )
}
