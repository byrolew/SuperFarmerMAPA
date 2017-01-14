#' @title Badanie gry przy zadanej strategii
#'
#' @description Funkcja wywolujaca funkcje \code{gra()} wielokrotnie przy zadanej strategii i zwracajaca wektor wynikow
#'
#' @param strategia Strategia, przy ktorej ma byc badana gra
#' @param ile Ile razy fukcja ma zagrac w SuperFarmera przy zadanej strategii, domyslnie 10000
#' @param ... Opcjonalne parametry do funkcji \code{gra}
#'
#' @return Funkcja zwraca wektor liczby ruchów w kolejnych grach.
#'
#' @examples
#' wyniki <- badaj_gre(strategia_rf, 10)
#'
#' @rdname badaj_gre
#'
#' @author Agnieszka Ciepielewska, Marek Wawreniuk, Pawel Gorniak
#'
#' @export

badaj_gre <- function(strategia, ile = 10000, ...){
  
  prices <- c(1, 6, 12, 36, 72, 6, 36)
  
  tabela <- matrix(nrow = 7, ncol = 7)
  wyniki <- list()
  wyniki <- lapply(1:ile, function(x) gra(strategia, ...))
  turns <- sapply(wyniki, `[`, 1)
  changes <- sapply(wyniki, `[`, 2)
  for(i in 1:ile){
    changes[[i]] <- t(t(changes[[i]])*prices)
  }
  #replicate(ile, )
  
  
  return(changes)
}
