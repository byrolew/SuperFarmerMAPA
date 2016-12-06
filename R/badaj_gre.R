#' @title Badanie gry przy zadanej strategii
#' 
#' @description Funkcja wywolujaca funkcje \code{gra()} 10000 razy przy zadanej strategii i zwracajaca podstawowe statystyki
#' 
#' @param strategia Strategia, przy ktorej ma byc badana gra
#' 
#' @return Funkcja zwraca nastepujace statystyki:
#' minimum, 1. kwartyl, mediana, srednia arytmetyczna, 3. kwartyl, maksimum
#' 
#' @examples 
#' wyniki <- badaj_gre(strategia)
#' 
#' @rdname badaj_gre
#' 
#' @author Agnieszka Ciepielewska
#' 
#' @export

badaj_gre <- function(strategia){
  wyniki <- sapply(1:10000, function(x) gra(strategia))
  return(summary(wyniki))
}