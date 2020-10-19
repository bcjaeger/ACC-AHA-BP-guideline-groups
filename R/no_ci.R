##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param x
##' @return
##' @export

no_ci <- function(x){
  stringr::str_remove(x, pattern = ' \\(.+\\)')
}
