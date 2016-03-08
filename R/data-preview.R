# Data Preview Module


#' Data Preview UI
#' 
#' @param id The input slot that will be used to access the value.
#' @param title Title of the box (Default: "Preview")
#' @param width Wdth of the shinydashboard box. (Default: 12)
#'
#' @export
datapreviewUI <- function(id, title = "Preview", width = 12) {
  ns <- NS(id)
  shinydashboard::box(
    title = title, 
    width = width,
    status = "primary", 
    solidHeader = TRUE,
    collapsible = F,
    DT::dataTableOutput(ns("table"))
  )
}

#' Data Preview Server
#' 
#' @param data data.frame that you want to preview
#' @param n number of observations to show. (Default: 100)
#'
#' @export
datapreview <- function(input, output, session, data, n = reactive(100)) {
  observe({
    if(is.null(data())){
      return(NULL)
    }
    
    n <- reactive({
      min(nrow(data()), n())
    })
    
    data_preview <- reactive({
      data()[1:n(),]
    })
    
    output$table <- DT::renderDataTable(
      DT::datatable({
        data_preview()
      })
    )
    
  })
}
