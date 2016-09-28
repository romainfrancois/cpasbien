library(shiny)

shinyUI(fluidPage(

  absolutePanel(
    bottom = "10px", right = "10px", width = "100px", height = "100px" , style = "opacity: .8",
    imageOutput("purrple" )
  ),
  sidebarPanel(
    htmlOutput( "movie_description")
  ),
  mainPanel(
    htmlOutput( "search_results" )
  )

))
