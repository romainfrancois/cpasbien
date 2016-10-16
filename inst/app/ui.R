library("shiny")
library("purrpleWidgets")

shinyUI(fluidPage(

  purrple_logo(),

  sidebarPanel(
    htmlOutput( "movie_description")
  ),
  mainPanel(
    htmlOutput( "search_results" )
  )

))
