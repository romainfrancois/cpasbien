library(shiny)

shinyUI(fluidPage(

  absolutePanel(
    bottom = "10px", right = "10px", width = "150px", height = "150px" , style = "z-index: -2",
    img( src = "purrple.png" , width = "150px", height = "150px", id = "purrple",
         onmouseover = '{ var audio = new Audio("purr.mp3");  audio.play() ; }')
  ),
  sidebarPanel(
    htmlOutput( "movie_description")
  ),
  mainPanel(
    htmlOutput( "search_results" )
  )

))
