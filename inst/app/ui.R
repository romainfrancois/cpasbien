library(shiny)

shinyUI(fluidPage(

  purrpleCatOutput("meow"),

  sidebarPanel(
    htmlOutput( "movie_description")
  ),
  mainPanel(
    htmlOutput( "search_results" )
  )

))
