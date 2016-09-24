#' @export
movies <- function( page = 1 ){
  scrap( sprintf( "http://www.cpasbien.cm/view_cat.php?categorie=films&page=%d", page) )
}

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
  search(..., where = "/series")
}

#' @export
search_all <- function(...){
  search(..., where = "")
}


#' @export
episodes <- function( page = 1){
  scrap( sprintf( "http://www.cpasbien.cm/view_cat.php?categorie=series&page=%d", page) )
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
  res <- llply( data$href, function(link){
    html <- read_html(link)
    torrent <- html %>%
      html_nodes("a#telecharger") %>%
      html_attr("href") %>%
      sprintf( "http://www.cpasbien.cm%s", unlist(.) )

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
