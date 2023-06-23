inputSelectionViewer <- function(id = "input-selection") {
   ns <- shiny::NS(id)
  
   shiny::div(
     
   # UI for inputs
   # summary table
   shinydashboard::box(
     collapsible = TRUE,
     title = "Options",
     width = "100%",
     shiny::uiOutput(ns("inputs"))
   ),
   
   # displayed inputs
   shiny::conditionalPanel(
     condition = "input.generate != 0",
     ns = ns,
     
     shinydashboard::box(
       status = 'warning', 
       width = "100%",
       title = 'Selected: ',
       shiny::uiOutput(ns("inputsText"))
     )
   )
   
   )
   
}

createInputSetting <- function(
    rowNumber,                           
    columnWidth = 4,
    varName = '',
    uiFunction = 'shinyWidgets::pickerInput',
    uiInputs = list(
      label = 'Input: ',
      choices = list(),
      multiple = F,
      options = shinyWidgets::pickerOptions()
    )
    
    ){
  
  result <- list(
    rowNumber = rowNumber,
    columnWidth = columnWidth,
    varName = varName,
    uiFunction = uiFunction,
    uiInputs = uiInputs
  )
  
  class(result) <- 'inputSetting'
  return(
    result
  )
}

inputSelectionServer <- function(
    id, 
    inputSettingList
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
    if(inherits(inputSettingList, 'inputSetting')){
      inputSettingList <- list(inputSettingList)
    }
      
      rowNumbers <- unlist(lapply(inputSettingList, function(x){x$rowNumber}))
      inputNames <- unlist(lapply(inputSettingList, function(x){x$varName}))
      rows <- list()
      for(i in 1:max(rowNumbers)){
        rows[[i]] <- shiny::fluidRow(
          lapply(which(rowNumbers == i), function(x){
            
            inputs <- inputSettingList[[x]]$uiInputs
            inputs$inputId <- session$ns(paste0('input_',x))
            
            shiny::column(
              width = inputSettingList[[x]]$columnWidth,
              do.call(eval(parse(text = inputSettingList[[x]]$uiFunction)), inputs)
            )
          }
          )
        )
      }
      rows[[length(rows)+1]] <- shiny::actionButton(
        inputId = session$ns('generate'), 
        label = 'Generate Report'
      )
      
      output$inputs <- shiny::renderUI({
        shiny::fluidPage(rows)
        })
        
      selectedInput <- shiny::reactiveVal()
      selectedInputText <- shiny::reactiveVal()
      output$inputsText <- shiny::renderUI(selectedInputText())
      
      # when generate is pressed update the selected values and text
      shiny::observeEvent(
        eventExpr = input$generate,
        {
          
          # get the input values and store in reactiveval
          inputList <- lapply(
            1:length(inputNames), 
            function(x){
              input[[paste0('input_', x)]]
          }
          )
          names(inputList) <- inputNames
          selectedInput(inputList)
          
          # create the text output
          
          otext <- list()
          for(i in 1:max(rowNumbers)){
            otext[[i]] <- shiny::fluidRow(
              lapply(which(rowNumbers == i), function(x){
                shiny::column(
                  width = inputSettingList[[x]]$columnWidth,
                  shiny::tags$b(paste0(inputSettingList[[x]]$uiInputs$label)),
                  if(!is.null(inputSettingList[[x]]$uiInputs$choices)){
                    paste(names(inputSettingList[[x]]$uiInputs$choices)[inputSettingList[[x]]$uiInputs$choices %in% input[[paste0('input_',x)]]], collapse = ',')
                  } else{
                    paste(input[[paste0('input_',x)]], collapse = ',')
                  }
                )
              }
              )
            )
          }
          selectedInputText(shiny::div(otext))
        })
      
      return(selectedInput)
          
    }
  )
}