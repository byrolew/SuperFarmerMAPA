#' @title Aktualizacja stada
#'
#' @description
#' Funkcja pomocnicza wykorzystywana przez funkcje \code{reproduction()} do aktualizowania stanu stada gracza
#'
#' @param stock_status Wektor posiadanych przez gracza zwierzat
#' @param max_stock Wektor maksymalnej liczby zwierzat w stadzie
#' @param die1_result String, bedacy wynikiem rzutu pierwsza kostka (tej z wilkiem), domyslnie pusty string
#' @param die2_result String, bedacy wynikiem rzutu druga kostka (tej z lisem), domyslnie pusty string
#'
#' @return stock_status Zaktualizowany stan stada gracza
#'
#'
#' @rdname update_stocku
#'
#' @author Agnieszka Ciepielewska

update_stocku <- function(stock_status, max_stock, die1_result = "", die2_result = ""){
  if(die1_result != die2_result){
    if(die1_result != ""){
      stock_status[die1_result] <- min(floor(stock_status[die1_result] + (stock_status[die1_result]+1)/2), max_stock[die1_result])
    }
    if(die2_result != ""){
      stock_status[die2_result] <- min(floor(stock_status[die2_result] + (stock_status[die2_result]+1)/2), max_stock[die2_result])
    }
  } else {
    stock_status[die1_result] <- min(floor(stock_status[die1_result] + (stock_status[die1_result]+2)/2), max_stock[die1_result])
  }
  return(stock_status)
}
