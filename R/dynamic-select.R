#' Dynamic Selectize Input 


#' Dynamic Select UI
#' 
#' @param id The input slot that will be used to access the value.
#'
#' @seealso \code{\link{dynamicselect}}
#'
#' @export
dynamicselectUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ds"))
}

#' Dynamic Select Server
#' 
#' @param label label text in the input
#' @param choices vector with possible values
#'  
#' @seealso \code{\link{dynamicselectUI}}
#'    
#' @export
dynamicselect <- function(input, output, session, label, choices) {
  output$ds <- renderUI({
    ns <- session$ns
    selectizeInput(
      ns('sel'),
      label = label(), 
      choices = choices()
    )
  })
  
  return(reactive({
    input$sel
  }))
}
