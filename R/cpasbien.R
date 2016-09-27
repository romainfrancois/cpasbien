#' @importFrom dplyr if_else
process_size <- function(size){
  rx   <- "^([^[:space:]]*)[[:space:]]*([^[:space:]]*)[[:space:]]*$"
  n    <- as.numeric(gsub(rx, "\\1", size ))
  unit <- gsub(rx, "\\2", size )
  round( ifelse( unit == "Mo", n, n*1024 ) )
}

clean_title <- function(title){
  title %>%
    gsub( "FRENCH.*$", "", . ) %>%
    gsub( "VOSTFR.*$", "", . ) %>%
    gsub( "PROPER*$", "", . ) %>%
    gsub( "[[:space:]]+$", "", .)
}


#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr html_text
#' @importFrom magrittr %>%
#' @importFrom tibble data_frame
#' @importFrom plyr llply
scrap <- function(url){
  html    <- read_html(url)
  data   <- html %>% html_nodes( "#gauche a.titre" )
  href   <- data %>% html_attr("href")
  titles <- data %>% html_text() %>% clean_title

  size <- html %>% html_nodes("#gauche .poid") %>% html_text() %>% process_size

  up   <- html %>% html_nodes( ".seed_ok") %>% html_text() %>% as.numeric
  down <- html %>% html_nodes( ".down") %>% html_text() %>% as.numeric

  data_frame( title = titles, href = href, size = size, up = up, down = down )
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

#' @importFrom dplyr if_else
extract_language <- function(base){
  lang_rx  <- "^(.*)[-](vostfr|truefrench|french)(.*)$"
  has_lang <- grepl(lang_rx, base)
  if_else( has_lang, gsub(lang_rx, "\\2", base), NA_character_ )
}

#' @importFrom dplyr if_else
extract_quality <- function(base){
  lang_rx  <- "^(.*)[-](vostfr|truefrench|french)(.*)$"
  has_lang <- grepl(lang_rx, base)
  if_else(has_lang, gsub( "^[-]", "", gsub(lang_rx, "\\3", base) ), NA_character_ )
}

extract_torrent <- function(base){
  sprintf( "http://www.cpasbien.cm/telechargement/%s.torrent", base )
}

extract_poster <- function(base){
  sprintf( "http://www.cpasbien.cm/_pictures/%s.jpg", base )
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
      lang     = extract_language(middle) ,
      quality  = extract_quality(middle),
      year     = gsub( rx, "\\3", base ),
      base     = gsub( "^.*/", "", base ),
      torrent  = extract_torrent(base),
      poster   = extract_poster(base)
      ) %>%
    select( type, title, year, lang, quality, size, up, down, torrent, poster, href)

}

#' @importFrom dplyr filter
process_episodes <- function( data ){
  rx <- "^(.*)S([[:digit:]]{2})E([[:digit:]]{2})(.*)$"
  res <- data  %>%
    filter( grepl(rx, title) ) %>%
    mutate(
      base     = gsub( "^.*/dl-torrent/series/(.*)[.]html$", "\\1", href ),
      middle   = gsub( rx, "\\2", base),
      show     = gsub( rx, "\\1", title),
      season   = as.numeric(gsub( rx, "\\2", title)),
      episode  = as.numeric(gsub( rx, "\\3", title)),
      lang     = extract_language( middle ),
      quality  = extract_quality( middle ),
      torrent  = extract_torrent(base),
      poster   = extract_poster(base)
    ) %>%
    select( show, season, episode, lang, quality, size, up, down, torrent, poster, href )
  res
}

#' Extract movies information
#'
#' @param pages pages to retrieve
#' @param n maximum number of pages
#' @param .progress see \code{\link[plyr]{llply}}
#' @param \dots further arguments for \code{\link[plyr]{llply}}
#'
#' @export
get_all_movies <- function( pages = seq_len(n), n = npages("films"), .progress = "text", ... ){
  pages <- pages[ pages <= n]
  data <- scrap_all("films", pages = pages, .progress = .progress, ...)
  process_movies(data)
}

#' Extract series information
#'
#' @param pages pages to retrieve
#' @param n maximum number of pages
#' @param .progress see \code{\link[plyr]{llply}}
#' @param \dots further arguments for \code{\link[plyr]{llply}}
#'
#' @export
get_all_episodes <- function( pages = seq_len(n), n = npages("series"), .progress = "text", ... ){
  pages <- pages[ pages <= n]
  data <- scrap_all("series", pages = pages, .progress = .progress, ...)
  process_episodes(data)
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

#' Search for movies
#'
#' @param query what to search
#' @param pages which pages to retrieve
#' @param n total number of pages. The default values retrieves the total number of pages for this query
#' @param \dots further arguments to pass to \code{\link[plyr]{llply}}, e.g. \code{.progress}
#'
#' @importFrom dplyr bind_rows
#' @importFrom plyr llply
#' @export
search_movies <- function( query, pages = seq(0, n), n = npages_search(query, "films") , ...){
  pages <- pages[ pages <= n]
  llply( pages, function(.) search(query, where = "films", page = .), ... ) %>%
    bind_rows %>%
    process_movies
}

#' Search from series episodes
#'
#' @param query what to search
#' @param pages which pages to retrieve
#' @param n total number of pages. The default values retrieves the total number of pages for this query
#' @param \dots further arguments to pass to \code{\link[plyr]{llply}}, e.g. \code{.progress}
#'
#' @importFrom plyr llply
#' @importFrom dplyr bind_rows
#' @export
search_episodes <- function( query, pages = seq(0, n), n = npages_search(query, "series") , ...){
  pages <- pages[ pages <= n]
  llply( pages, function(.) search(query, where = "series", page = .), ... ) %>%
    bind_rows %>%
    process_episodes
}

#' Extract description
#'
#' @param url cpasbien url where to extract the description from
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom utils tail
#' @export
extract_description <- function(url){
  read_html(url) %>%
    html_nodes("#textefiche p") %>%
    tail(1) %>%
    html_text
}
