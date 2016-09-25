library(shiny)
library(cpasbien)
library(dplyr)
library(jsonlite)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

  sidebarPanel(
    textInput( "search", label = "" ),
    actionButton( "search_button" , label = "search movies"),
    htmlOutput( "movie_description")
  ),
  mainPanel(
    htmlOutput( "search_results" )
  )

))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {

  search_results <- reactive({
    input$search_button
    query <- isolate(input$search)
    data <- if( query == "" ) {
      NULL
    } else {
      data <- search_movies(query, .progress = progress_shiny(session) ) %>%
        group_by(title) %>%
        summarise(
          url = list(href),
          img = first(poster),
          torrent = list(torrent),
          quality = list(quality),
          lang = list(lang),
          size = list(size)
        )
      data
    }
    data
  })

  output$movie_description <- renderUI({
    index <- as.numeric(input$img_clicked)
    if( length(index) ){
      data <- search_results()

      if( !is.null(data) && nrow(data) > 0){
        data <- data[index, ]

        description <- extract_description(data$url[[1]][1])
        title <- data$title[1]
        img <- data$img[1]
        torrents <- unlist( data$torrent[[1]] )
        quality <- unlist(data$quality[[1]])
        lang <- unlist(data$lang[[1]])
        size <- unlist(data$size[[1]])

        links <- do.call( div, lapply( seq_along(torrents), function(i){
          tagList(
            tags$a( href = torrents[i], sprintf( "%s - %s %d Mb", quality[i], lang[i], size[i]) ),
            tags$br()
          )
        }))

        tagList(
          tags$br(),
          tags$h3(title),
          tags$p(description),
          tags$img( src = img ),
          tags$h4("links"),

          links
        )
      }

    }

  })

  output$search_results <- renderUI({
    data <- search_results()
    if( !is.null(data) ){
      res <- lapply( seq_len(nrow(data)), function(i) {
        tags$img(
          src=data$img[i], width = 100,
          onclick = sprintf( 'Shiny.onInputChange( "img_clicked", %d )', i )
        )
      } )
      do.call( div, res)
    }
  })

})

# Run the application
shinyApp(ui = ui, server = server)

