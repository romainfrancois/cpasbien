
search <- function(query, page = 0, where = "" ){
  query <- gsub( "[[:space:]]+", "-", query)
  scrap( sprintf( "http://www.cpasbien.cm/recherche%s/%s/page-%d", where, query, page) )
}

#' @export
search_movies <- function(...){
  search(..., where = "/films")
}

#' @export
search_episodes <- function(...){
  process_episodes( search(..., where = "/series") )
}

#' @export
search_all <- function(...){
  search(..., where = "")
}

recent <- function( page = 1, category ){
  scrap( sprintf( "http://www.cpasbien.cm/view_cat.php?categorie=%s&page=%d", category, page) )
}

process_episodes <- function( data ){
  rx <- "^(.*)S([[:digit:]]{2})E([[:digit:]]{2})(.*)$"
  res <- data  %>%
    filter( grepl(rx, title) ) %>%
    mutate(
      show = gsub( rx, "\\1", title),
      season = as.numeric(gsub( rx, "\\2", title)),
      episode = as.numeric(gsub( rx, "\\3", title)),
      VOSTFR = grepl( "VOSTFR", title, ignore.case = TRUE ),
      FRENCH = grepl( "FRENCH", title, ignore.case = TRUE ),
      HDTV = grepl( "HDTV", title, ignore.case = TRUE ),
      BluRay = grepl( "BluRay", title, ignore.case = TRUE )
    )
  res
}

#' @importFrom dplyr filter
#' @export
all_episodes <- function( page = 1){
  process_episodes( recent(page=page, category = "series") )
}

#' @export
all_movies <- function( page = 1 ){
  recent(page=page, category = "films")
}


#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr html_text
#' @importFrom magrittr %>%
#' @importFrom tibble data_frame
#' @importFrom plyr llply
scrap <- function(url){
  html <- read_html(url)
  data <- html %>% html_nodes( "#gauche a.titre" )
  href <- data %>% html_attr("href")
  titles <- data %>% html_text()

  size <- html %>% html_nodes("#gauche .poid") %>% html_text()

  data_frame( title = titles, href = href, size = size)
}

#' @importFrom dplyr bind_cols bind_rows
#' @importFrom plyr llply
#' @export
details <- function(data, .progress = "text", ... ){
  if( !nrow(data) ){
    shell <- data_frame( torrent = character(0), description = character(0), poster = character(0))
    bind_cols( data, shell)
  } else {
    res <- llply( data$href, function(link){
      html <- read_html(link)
      torrent <- html %>%
        html_nodes("a#telecharger") %>%
        html_attr("href")
      torrent <- sprintf( "http://www.cpasbien.cm%s", unlist(torrent) )

      description <- html %>%
        html_nodes("#textefiche p") %>%
        tail(1) %>%
        html_text

      poster <- html %>%
        html_nodes( "#bigcover img" ) %>%
        html_attr("src")

      data_frame( torrent = torrent, description = description, poster = poster)
    }, .progress = .progress, ... )

    bind_cols( data, bind_rows(res) )
  }

}
