library("shiny")
library("purrple")

page <- purrplePage(fluidPage)
shinyUI(page(

  sidebarPanel(
    htmlOutput( "movie_description")
  ),
  mainPanel(
    htmlOutput( "search_results" )
  )

))
