#' @title Rozmnazanie zwierzat po rzucie
#'
#' @description Funkcja przeprowadzajaca zmiany w stadzie gracza po rzucie kostkami,
#' wykorzystuje funkcje \code{update_stocku}
#'
#' @param die1_result String, bedacy wynikiem rzutu pierwsza kostka (tej z wilkiem)
#' @param die2_result String, bedacy wynikiem rzutu druga kostka (tej z lisem)
#' @param stock_status Wektor posiadanych przez gracza zwierzat
#' @param max_stock Wektor maksymalnej liczby zwierzat w stadzie
#' @param zawsze_jeden_krolik Wartosc binarna oznaczajaca czy gramy w wersje zasad w ktorej
#' zawsze mamy co najmniej jednego krolika
#' @param co_zjada_wilk wektor zer i jedynek oznaczajacy czy wilk zjada konkretny rodzaj zwierzecia czy nie
#' (duzy pies zawsze powinien dostawac 0, bo jest to rozpatrywane w przypadku gdy nie mamy duzego psa)
#'
#'
#' @return stock_status Stan stada gracza po rozpatrzeniu rzutow kostkami
#'
#' @rdname reproduction
#'
#' @author Agnieszka Ciepielewska, Marek Wawreniuk, Pawel Gorniak

reproduction <- function(die1_result, die2_result, stock_status, max_stock, zawsze_jeden_krolik = FALSE, co_zjada_wilk = c(1, 1, 1, 1, 0, 0, 0)){
  if(die1_result != "wilk" && die2_result != "lis"){
    stock_status <- update_stocku(stock_status, max_stock, die1_result, die2_result)
  } else if(die1_result == "wilk" && die2_result != "lis"){
    if(stock_status[["duzy_pies"]] == 0){
      stock_status <- stock_status - (stock_status*co_zjada_wilk)
      if(zawsze_jeden_krolik && co_zjada_wilk[1] == 1){
        stock_status[["krolik"]] <- 1
      }
    } else {
      stock_status[["duzy_pies"]] <- stock_status[["duzy_pies"]] - 1
    }
    stock_status <- update_stocku(stock_status, max_stock, die2_result)
  } else if(die1_result != "wilk" && die2_result == "lis"){
    if(stock_status[["maly_pies"]] == 0){
      if(zawsze_jeden_krolik){
        stock_status[["krolik"]] <- 1
      }else{
        stock_status[["krolik"]] <- 0
      }
    } else {
      stock_status[["maly_pies"]] <- stock_status[["maly_pies"]] - 1
    }
    stock_status <- update_stocku(stock_status, max_stock, die1_result)
  } else {
    if(stock_status[["duzy_pies"]] == 0){
      stock_status <- stock_status - (stock_status*co_zjada_wilk)
      if(zawsze_jeden_krolik && co_zjada_wilk[1] == 1){
        stock_status[["krolik"]] <- 1
      }
    } else {
      stock_status[["duzy_pies"]] <- stock_status[["duzy_pies"]] - 1
    }
    if(stock_status[["maly_pies"]] == 0){
      if(zawsze_jeden_krolik){
        stock_status[["krolik"]] <- 1
      }else{
        stock_status[["krolik"]] <- 0
      }
    } else {
      stock_status[["maly_pies"]] <- stock_status[["maly_pies"]] - 1
    }
  }
  return(stock_status)
}
