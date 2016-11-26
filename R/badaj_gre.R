#' @export
badaj_gre <- function(strategia){
  wyniki <- sapply(1:10000, function(x) gra(strategia))
  return(summary(wyniki))
}