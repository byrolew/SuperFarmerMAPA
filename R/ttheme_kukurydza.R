#' @title Wyglad tabeli
#'
#' @description Funkcja \code{ttheme_kukurydza} generuje ttheme do tabeli statystyk
#'
#' @return tt1 szablon wygladu
#'
#' @import gridExtra
#'
#' @export

ttheme_kukurydza <- function(){
  tt1 <- gridExtra::ttheme_default(
    base_size = 11,
    core = list(fg_params = list(col="white"),
                bg_params=list(fill=c(rgb(230, 90, 90, maxColorValue = 255),
                                      rgb(50, 50, 50, maxColorValue = 255),
                                      rgb(90, 230, 110, maxColorValue = 255)))),
    colhead = list(fg_params=list(col="black"),
                   bg_params=list(fill=rgb(220, 220, 220, maxColorValue = 255))),
    rowhead = list(fg_params=list(cex = 1.0)))
  return(tt1)
}
