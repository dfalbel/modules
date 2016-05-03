#' WordTree UI
#' 
#' Use googleCharts::googleWordtree to make a shiny dashboard box with the
#' Wordtree.
#' 
#' 
#' @param id The input slot that will be used to access the value.
#' @param title Title of the box (Default: "Preview")
#' @param width Width of the shinydashboard box. (Default: 12)
#'
#' @seealso \code{\link{wordtree}}
#'
#' @export
wordtreeUI <- function(id, title = "WordTree", width.box = 12, width.plot = "600px") {
  ns <- NS(id)
  shinydashboard::box(
    title = title, 
    width = width,
    status = "primary", 
    solidHeader = TRUE,
    collapsible = F,
    googleCharts::googleWordtree(ns("wordtree"), width = width.plot, height = "100%")
  )
}

#' Wordtree Server
#' 
#' @param data data.frame that you want to preview
#' @param n number of observations to show. (Default: 100)
#'
#' @seealso \code{\link{datapreviewUI}}
#'
#' @export
wordtree <- function(input, output, session, data, n = reactive(100)) {
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