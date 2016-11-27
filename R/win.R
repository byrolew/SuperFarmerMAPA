#' @title Czy gracz wygral
#' 
#' @description Funkcja sprawdzajaca czy w danym momencie gracz ma odpowiednie zwierzeta, aby wygrac
#' 
#' @param stock_status Wektor aktualnego stanu stada gracza
#' 
#' @return Funkcja zwraca \code{TRUE} jesli gracz spelnia warunki zwyciestwa i \code{FALSE} w przeciwnym przypadku
#' 
#' @rdname win
#' 
#' @author Agnieszka Ciepielewska

win <- function(stock_status){
  min(stock_status[c(1:5)]) > 0
}