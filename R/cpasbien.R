#' @importFrom dplyr if_else
process_size <- function(size){
  rx   <- "^([^[:space:]]*)[[:space:]]*([^[:space:]]*)[[:space:]]*$"
  n    <- as.numeric(gsub(rx, "\\1", size ))
  unit <- gsub(rx, "\\2", size )
  round( ifelse( unit == "Mo", n, n*1024 ) )
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

  size <- html %>% html_nodes("#gauche .poid") %>% html_text() %>% process_size

  data_frame( title = titles, href = href, size = size)
}

get_list <- function( page = 1, category ){
  scrap( sprintf( "http://www.cpasbien.cm/view_cat.php?categorie=%s&page=%d", category, page) )
}

#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom magrittr %>%
npages <- function( category = "films" ){
  url <- sprintf( "http://www.cpasbien.cm/view_cat.php?categorie=%s&page=1", category)
  pages <- read_html(url) %>%
    html_nodes("#pagination a") %>%
    html_text()
  as.numeric( pages[ length(pages) - 1] )
}

#' @importFrom plyr llply
#' @importFrom dplyr bind_rows
scrap_all <- function(category = "films", pages = seq_len(n), n = npages(category), .progress = "text", ... ){
  res <- llply( pages, function(i){
    get_list( page = i, category = category )
  }, .progress = .progress, ...)
  bind_rows(res)
}


#' @importFrom dplyr mutate select
process_movies <- function(data){
  rx      <- "^([^/]+)/(.*)[-]([0-9]{4})([-][0-9]*)?$"
  lang_rx <- "^(.*)[-](vostfr|truefrench|french)(.*)$"

  data %>%
    mutate(
      base     = gsub( "^.*/dl-torrent/films/(.*)[.]html$", "\\1", href ),
      type     = gsub( rx, "\\1", base ),
      middle   = gsub( rx, "\\2", base),
      has_lang = grepl(lang_rx, middle),
      title    = if_else(has_lang, gsub(lang_rx, "\\1", middle ), middle),
      lang     = if_else(has_lang, gsub(lang_rx, "\\2", middle), NA_character_ ) ,
      quality  = if_else(has_lang, gsub( "^[-]", "", gsub(lang_rx, "\\3", middle) ), NA_character_ ) ,
      year     = gsub( rx, "\\3", base ),
      base     = gsub( "^.*/", "", base ),
      torrent  = sprintf( "http://www.cpasbien.cm/telechargement/%s.torrent", base )
      ) %>%
    select( type, title, year, lang, quality, size, torrent, href)

}

#' @export
get_all_movies <- function( pages = seq_len(n), n = npages("films"), .progress = "text", ... ){
  pages <- pages[ pages <= n]
  data <- scrap_all("films", pages = pages, .progress = .progress, ...)
  process_movies(data)
}

#' @importFrom rvest html_nodes
npages_search <- function(query, where = "" ){
  query <- gsub( "[[:space:]]+", "-", query)
  if( where != "" ) where <- sprintf( "/%s", where)

  url <- sprintf( "http://www.cpasbien.cm/recherche%s/%s.html", where, query)
  html <- read_html(url)
  pagination <- html %>% html_nodes("#pagination a") %>% html_text()
  if( length(pagination) ){
    as.numeric( pagination[ length(pagination) - 1] )
  } else 1

}

search <- function(query, page = 0, where = "" ){
  query <- gsub( "[[:space:]]+", "-", query)
  if( where != "" ) where <- sprintf( "/%s", where)
  scrap( sprintf( "http://www.cpasbien.cm/recherche%s/%s/page-%d", where, query, page) )
}

#' @importFrom dplyr bind_rows
#' @importFrom plyr llply
#' @export
search_movies <- function( query, pages = seq(0, n), n = npages_search(query, "films") , ...){
  pages <- pages[ pages <= n]
  llply( pages, function(.) search(query, where = "films", page = .) ) %>%
    bind_rows %>%
    process_movies
}

#' @export
search_episodes <- function(...){
  process_episodes( search(..., where = "/series") )
}

#' @export
search_all <- function(...){
  search(..., where = "")
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
get_episodes <- function( page = 1){
  process_episodes( get_list(page=page, category = "series") )
}

#' @export
get_movies <- function( page = 1 ){
  get_list(page=page, category = "films")
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
