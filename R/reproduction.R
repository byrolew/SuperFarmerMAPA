#' @title Rozmnazanie zwierzat po rzucie
#' 
#' @description Funkcja przeprowadzajaca zmiany w stadzie gracza po rzucie kostkami, 
#' wykorzystuje funkcje \code{update_stocku}
#' 
#' @param die1_result String, bedacy wynikiem rzutu pierwsza kostka (tej z wilkiem)
#' @param die2_result String, bedacy wynikiem rzutu druga kostka (tej z lisem)
#' @param stock_status Wektor posiadanych przez gracza zwierzat
#' @param max_stock Wektor maksymalnej liczby zwierzat w stadzie
#' 
#' @return stock_status Stan stada gracza po rozpatrzeniu rzutow kostkami
#' 
#' @examples 
#' stock_status <- reproduction(die1_result="krowa", die2_result="krolik", stock_status=c(0,1,0,2,0,0,0), 
#' max_stock=c(60,24,20,12,6,2,4))
#' 
#' @rdname reproduction
#' 
#' @author Agnieszka Ciepielewska

reproduction <- function(die1_result, die2_result, stock_status, max_stock){
  if(die1_result != "wilk" && die2_result != "lis"){
    stock_status <- update_stocku(stock_status, max_stock, die1_result, die2_result)
  } else if(die1_result == "wilk" && die2_result != "lis"){
    if(stock_status[["duzy_pies"]] == 0){
      stock_status[["krolik"]] <- 0
      stock_status[["owca"]] <- 0
      stock_status[["swinia"]] <- 0
      stock_status[["krowa"]] <- 0
    } else {
      stock_status[["duzy_pies"]] <- stock_status[["duzy_pies"]] - 1
    }
    stock_status <- update_stocku(stock_status, max_stock, die2_result)
  } else if(die1_result != "wilk" && die2_result == "lis"){
    if(stock_status[["maly_pies"]] == 0){
      stock_status[["krolik"]] <- 0
    } else {
      stock_status[["maly_pies"]] <- stock_status[["maly_pies"]] - 1
    }
    stock_status <- update_stocku(stock_status, max_stock, die1_result)
  } else {
    if(stock_status[["duzy_pies"]] == 0){
      stock_status[["krolik"]] <- 0
      stock_status[["owca"]] <- 0
      stock_status[["swinia"]] <- 0
      stock_status[["krowa"]] <- 0
    } else {
      stock_status[["duzy_pies"]] <- stock_status[["duzy_pies"]] - 1
    }
    if(stock_status[["maly_pies"]] == 0){
      stock_status[["krolik"]] <- 0
    } else {
      stock_status[["maly_pies"]] <- stock_status[["maly_pies"]] - 1
    }
  }
  return(stock_status)
}