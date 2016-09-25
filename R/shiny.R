#' @importFrom shiny Progress
#' @export
progress_shiny <- function (session){
  p <- NULL
  list(
    init = function(n){
      p <<- Progress$new( session, min = 0, max = n)
      p$set(0)
    },
    step = function() {
      p$set( p$getValue() + 1 )
    },
    term = function(){
      p$close()
    }
  )
}
