# Create Action Button Definition

Build an action button configuration for resultTableServer(addActions =
...).

## Usage

``` r
createActionButton(
  actionType,
  buttonIcon = "play",
  hoverText = NULL,
  buttonClass = "btn btn-default btn-xs",
  buttonStyle =
    "margin-right: 4px; margin-bottom: 2px; padding: 2px 8px; font-size: 11px; line-height: 1.2;",
  buttonLabel = NULL
)
```

## Arguments

- actionType:

  string, action identifier emitted by the button click event.

- buttonIcon:

  string, Font Awesome icon name (for shiny::icon). Use "" to hide the
  icon.

- hoverText:

  string, button tooltip shown on hover.

- buttonClass:

  string, CSS classes applied to the button element.

- buttonStyle:

  string, inline CSS style applied to the button element.

- buttonLabel:

  string, button text shown to the user.

## Value

A list with action button settings.

## See also

Other Utils: [`actionButtonStyleInfo()`](actionButtonStyleInfo.md),
[`actionButtonStyleWarning()`](actionButtonStyleWarning.md),
[`datasourcesHelperFile()`](datasourcesHelperFile.md),
[`datasourcesServer()`](datasourcesServer.md),
[`datasourcesViewer()`](datasourcesViewer.md),
[`getLogoImage()`](getLogoImage.md),
[`resultTableViewer()`](resultTableViewer.md)
