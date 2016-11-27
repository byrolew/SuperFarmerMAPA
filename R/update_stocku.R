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
#' @return Funkcja zwraca zaktualizowany stan stada gracza
#' 
#' @examples 
#' stock_status <- update_stocku(stock_status=c(0,1,0,2,0,0,0), 
#' max_stock=c(60,24,20,12,6,2,4), die1_result="krowa")
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