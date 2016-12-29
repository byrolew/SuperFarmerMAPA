#' @title Czy gracz wygral
#'
#' @description Funkcja sprawdzajaca czy w danym momencie gracz ma odpowiednie zwierzeta, aby wygrac
#'
#' @param stock_status Wektor aktualnego stanu stada gracza
#' @param warunek_zwycieztwa Jakie zwierzeta musi miec gracz aby wygrac
#'
#' @return Funkcja zwraca \code{TRUE} jesli gracz spelnia warunki zwyciestwa i \code{FALSE} w przeciwnym przypadku
#'
#' @rdname win
#'
#' @author Agnieszka Ciepielewska, Marek Wawreniuk, Pawel Gorniak

win <- function(stock_status, warunek_zwycieztwa){
  for(i in 1:7){
    if(warunek_zwycieztwa[i] > stock_status[i]){
      return(FALSE)
    }
  }
  return(TRUE)
}
