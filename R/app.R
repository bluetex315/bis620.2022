#' Function to start the toy shiny app for homework 3
#'
#' @returns A shiny app object
#' @export

start_shiny = function(){
  ui <- fluidPage(
    "Hello, world!"
  )
  server <- function(input, output, session) {
  }
  shinyApp(ui, server)
}

