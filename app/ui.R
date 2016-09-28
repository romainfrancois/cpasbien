library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

  sidebarPanel(
    htmlOutput( "movie_description")
  ),
  mainPanel(
    htmlOutput( "search_results" )
  )

))
