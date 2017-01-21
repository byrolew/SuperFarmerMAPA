#' @title Wyglad tabeli
#'
#' @description Funkcja \code{ttheme_kukurydza} generuje ttheme do tabeli statystyk
#'
#'
#' @return tt1 szablon wygladu
#'
#' @import gridExtra
#'
#' @export
#'
ttheme_kukurydza <- function(){
  tt1 <- gridExtra::ttheme_default(
    core = list(fg_params = list(col="white"),
                bg_params=list(fill=c("firebrick1", "gray7", "green3"))),
    colhead = list(fg_params=list(col="white"),
                   bg_params=list(fill="cadetblue3")),
    rowhead = list(fg_params=list(cex = 1.0)))
  return(tt1)
}
