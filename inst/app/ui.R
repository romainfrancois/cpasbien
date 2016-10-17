library("shiny")
library("purrple")

shinyUI(fluidPage(
  sidebarPanel(
    htmlOutput( "movie_description")
  ),
  mainPanel(
    htmlOutput( "search_results" )
  )

))
