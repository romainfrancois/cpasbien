library(shiny)
library(cpasbien)
library(dplyr)
library(purrple)

shinyServer(function(input, output, session) {

  movies_results <- reactive({
    data <- movies %>%
      group_by(title) %>%
      summarise(
        img = first(poster),
        type = first(type),
        url = list(href),
        torrent = list(torrent),
        quality = list(quality),
        lang = list(lang),
        size = list(size),
        up = list(up),
        down = list(down)
      ) %>%
      mutate( index = row_number() )
    data
  })

  output$movie_description <- renderUI({
    index <- as.numeric(input$img_clicked)
    if( length(index) ){
      data <- movies_results()
      if( is.null(data)) return()

      if( !is.null(data) && nrow(data) > 0){
        data <- data[index, ]

        description <- extract_description(data$url[[1]][1])
        title <- data$title[1]
        img <- data$img[1]
        torrents <- unlist( data$torrent[[1]] )
        quality <- unlist(data$quality[[1]])
        lang <- unlist(data$lang[[1]])
        size <- unlist(data$size[[1]])

        links <- lapply( seq_along(torrents), function(i){
          tagList(
            tags$a( href = torrents[i], sprintf( "%s - %s %d Mb", quality[i], lang[i], size[i]) ),
            tags$br()
          )
        })

        tagList(
          tags$br(),
          tags$h3(title),
          tags$p(description),
          tags$img( src = img ),
          tags$h4("links"),

          div( links )
        )
      }
    }

  })

  output$search_results <- renderUI({
    data <- movies_results()
    if( is.null(data) ) return()

    categories <- unique(data$type)

    if( !is.null(data) ){

      res <- lapply( categories, function(categ){
        d <- filter( data, type == categ ) %>%
          group_by(title) %>%
          mutate( sum_up = sum(unlist(up)) ) %>%
          arrange( desc(sum_up) )

        x <- lapply( seq_len(nrow(d)), function(i) {
          tags$img(
            src=d$img[i], width = 80,
            onclick = sprintf( 'Shiny.onInputChange( "img_clicked", %d )', d$index[i] )
          )
        })
        tabPanel( categ, div(x) )
      })
      do.call( tabsetPanel, res )
    }
  })

})
