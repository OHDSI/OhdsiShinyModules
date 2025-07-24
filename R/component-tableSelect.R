# create a input selector 
# that shows the options as columns and 

# inputs: table to display
# input columns 
# output selected row id

tableSelectionViewer <- function(id = "input-selection") {
  ns <- shiny::NS(id)
  
  shiny::div(
    # UI for inputs - has a button that activates a model with the options as a table
    shiny::uiOutput(ns('selectionInput'))
    
  )
  
}


tableSelectionServer <- function(
    id, 
    table, # must be reactive
    selectedRowId, # must be reactive
    helpText = 'Click the button to make your selection',
    selectMultiple = FALSE,
    inputColumns = NULL,
    displayColumns = inputColumns,
    elementId = NULL,
    selectButtonText = 'Select Option',
    tableReset = shiny::reactive(0)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # defaults
      ##selectedRow <- shiny::reactiveVal(NULL)
      selection <- ifelse(selectMultiple, 'multiple','single')
      selectButtonText <- shiny::reactiveVal(selectButtonText)
      helpTextReactive <- shiny::reactiveVal(helpText)
      icon <- shiny::reactiveVal('plus')
      
      # reset if table changes - TODO remove this?
      shiny::observeEvent(tableReset(),{
        icon('plus')
        helpTextReactive(helpText)
        output$selectedRow <- NULL
      })
      
      # create the main UI
      output$selectionInput <- shiny::renderUI(
        shinydashboard::box(
          collapsible = TRUE,
          title = shiny::actionButton(
            inputId = session$ns('openModal'), 
            icon = shiny::icon(icon()),
            label = selectButtonText()
          ),
          width = "100%",
          shiny::helpText(helpTextReactive()),
          shiny::uiOutput(session$ns('selectedRow'))
        )
      )
      
      # when the selection button is pressed open a modal
      # with the table to select from
      shiny::observeEvent(
        eventExpr = input$openModal, {
          
          shiny::showModal(
            shiny::modalDialog(
              size = 'l',
            title = "Select row/s", 
            shiny::helpText('Select row/s of interest by clicking on the selector and then scroll down to the bottom of the table to hit the "Select" button and confirm your selection.  Hitting the "Select" button will close the modal.'),
            reactable::reactableOutput(session$ns("inputTable")),
            
            footer = shiny::actionButton(
              inputId = session$ns("confirmInput"), 
              label = 'Select'
              )
            )
          )
          
          # ?add code to set the row if selectedRow() is not NULL
          if(!is.null(selectedRowId())){
            if(sum(selectedRowId()) == 0){
              reactable::updateReactable(
                outputId = 'inputTable', 
                selected = NA
              )
            } else{
              reactable::updateReactable(
                outputId = 'inputTable', 
                selected = selectedRowId()
              )
            }
          }
        }
        )
      
  
      # display the table in the model
      # TODO how to get selected rows to persist?
       output$inputTable <- reactable::renderReactable(
         expr = if(!is.null(table())){
           return(reactable::reactable(
           data = table(), 
           columns = inputColumns,
           striped = TRUE, 
           pagination = TRUE,
           showPagination = TRUE,
           showPageInfo = TRUE,
           showPageSizeOptions = TRUE,
           pageSizeOptions = c(5,25,50,500),
           defaultPageSize = 5, 
           selection = selection, 
           highlight = TRUE, 
           filterable = TRUE, 
           compact = TRUE, 
           onClick = "select",
           elementId = elementId
           ))} else{
           return(NULL)
         }
      )
       
       # when modal button is clicked update the selected row and 
       # remove model
       shiny::observeEvent(input$confirmInput,{
         shiny::removeModal() # close the modal
         
         rowId <- reactable::getReactableState(outputId = 'inputTable', name = 'selected')
         if(!is.null(rowId)){
           selectedRowId(rowId)
         } else{
           selectedRowId(0)
         }
         
       }
         
       )
       
       
       # observe the row change to update the selected
       # need this for it to work across servers
       shiny::observeEvent(selectedRowId(), {
         
         if(sum(selectedRowId()) == 0){
           icon('plus')
           helpTextReactive(helpText)
           output$selectedRow <- NULL
         } else{
           if(nrow(table()[selectedRowId(),]) > 0){
             # update the icon if a row is selected
             icon('redo')
             helpTextReactive("")
             
             output$selectedRow <- shiny::renderUI(
               shiny::div(
                 #shiny::h4('Selected: '),
                 reactable::reactable(
                   data = table()[selectedRowId(),], 
                   columns = displayColumns,
                   sortable = FALSE,
                   filterable = FALSE, 
                   searchable = FALSE, 
                   compact = TRUE,
                   pagination = FALSE,
                   showPageInfo = FALSE
                 )
               )
             )
             
           } else{
             icon('plus')
             helpTextReactive(helpText)
             output$selectedRow <- NULL
           }
         }
       }
         
       )
       
      
    }
  )
}