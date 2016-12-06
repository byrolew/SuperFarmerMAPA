#' @title Generowanie mozliwych wymian zwierzecia na mniejsze
#' 
#' @description 
#' Funkcja \code{generate_exchanges()} generuje wszystkie mozliwe wymiany zwierzecia na mniejsze, wykorzystywana w 
#' funkcji \code{generate_change_lists()}
#' 
#' @param n_rabbits Cena zwierzecia, ktore chcemy rozbic (w krolikach)
#' @param prices_sorted Ceny zwierzat, posortowane malejaca
#' @param pre_moves Wymiana, ktora juz nastapila
#' @param moves To, co chcemy jeszcze wyznaczyc
#' 
#' @return moves Macierz wszystkich mozliwych wymian zadanego zwierzecia na tansze
#' 
#' @examples 
#' n_rabbits <- prices_sorted("kon")
#' mozliwe_wymiany <- generate_exchanges(n_rabbits, prices_sorted)
#' 
#' @rdname generate_exchanges
#' 
#' @author Agnieszka Ciepielewska

generate_exchanges <- function(n_rabbits, prices_sorted, pre_moves=c(), moves=c()){
  ratio <- floor(n_rabbits/prices_sorted[1])
  #Warunek koncowy
  if(length(prices_sorted) == 1){
    return(c(pre_moves, ratio))
  }
  for(i in 0:ratio){
    #Dopisywany jest tutaj kolejny wiersz macierzy ruchow
    moves <- rbind(moves, generate_exchanges(n_rabbits - i*prices_sorted[1],
                               prices_sorted[-1],
                               c(pre_moves, i),
                               moves))
  }
  return(unique(moves))
}