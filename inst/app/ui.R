library("shiny")
library("purrpleWidgets")

shinyUI(fluidPage(
  sidebarPanel(
    htmlOutput( "movie_description")
  ),
  mainPanel(
    htmlOutput( "search_results" )
  )

))
