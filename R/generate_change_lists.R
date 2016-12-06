#' @title Generowanie listy mozliwych wymian zwierzecia na mniejsze i mniejszych na jedno duze
#' 
#' @description 
#' Funkcja \code{generate_change_lists()} generuje dwie listy: wszystkie mozliwe wymiany zwierzecia na mniejsze 
#' i wszytskie mozliwe wymiany malych zwierza na jedno duze, wykorzystuje
#' funkcje \code{generate_exchanges()}
#' 
#' @param prices_sorted Ceny zwierzat, posortowane malejaca
#' 
#' @return lista list macierzy wymian
#' 
#' @examples 
#' wymiany_na_wieksze <- generate_change_lists[[1]]
#' wymiany_na_mniejsze <- generate_change_lists[[2]]
#' 
#' @rdname generate_change_lists
#' 
#' @author Agnieszka Ciepielewska

generate_change_lists <- function(prices_sorted){
  animals <- c("kon", "krowa", "duzy_pies", "swinia", "owca", "maly_pies", "krolik")
  #lista mozliwych zamian malych zwierz?t na jedno wieksze
  to_big_animal <- list()
  #lista mozliwych zamian duzego zwierzecia na mniejsze
  to_small_animals <- list()
  for(i in animals){
    to_big_animal[[i]] <- -generate_exchanges(prices_sorted[i], prices_sorted)
    to_big_animal[[i]][, which(names(prices_sorted) == i)] <- 1
    #Wyrzucanie przypadkow, ze wymianiamy jedno zwierze na nic 
    #(byly takie gdy w funkcji generate_exchanges wychodzila np_ wymiana konia na konia)
    to_big_animal[[i]] <- to_big_animal[[i]][rowSums(to_big_animal[[i]]) != 1, , drop=F]
    to_small_animals[[i]] <- -to_big_animal[[i]]
  }
#to_big_animal wyglada teraz tak: zwierzeta, ktore oddajemy sa na minusie, a te, ktore otrzymujemy na plusie
  return(list(to_big_animal, to_small_animals))
}