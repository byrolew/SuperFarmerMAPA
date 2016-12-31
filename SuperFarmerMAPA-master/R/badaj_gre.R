#' @title Badanie gry przy zadanej strategii
#'
#' @description Funkcja wywolujaca funkcje \code{gra()} wielokrotnie przy zadanej strategii i zwracajaca wektor wynikow
#'
#' @param strategia Strategia, przy ktorej ma byc badana gra
#' @param ile Ile razy fukcja ma zagrac w SuperFarmera przy zadanej strategii, domyslnie 10000
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

badaj_gre <- function(strategia, ile = 10000){
  wyniki <- sapply(1:ile, function(x) gra(strategia))
  return(wyniki)
}
