#' @title Generowanie mozliwych rochow
#'
#' @description
#' Funkcja \code{moves_generation()} wszytkie mozliwe ruchy gracza przy danym stadzie.
#' Wykorzystuje funkcje \code{generate_change_lists()}
#'
#' @param stock_status Aktualny stan stada gracza
#'
#' @return possible_moves Macierz wszystkich mozliwych ruchow
#'
#' @examples
#' stan_stada <- c(2,4,1,0,0,0,0)
#' stan_stada <- moves_generation(stan_stada)
#'
#' @rdname moves_generation
#'
#' @author Agnieszka Ciepielewska

moves_generation <- function(stock_status){
  max_stock <- c(6, 12, 2, 20, 24, 4, 60)
  prices_sorted <- c(72, 36, 36, 12, 6, 6, 1)
  animals <- c("kon", "krowa", "duzy_pies", "swinia", "owca", "maly_pies", "krolik")
  names(prices_sorted) <- animals
  #chcemy wytworzych listy wymian tylko raz, bo to troche zajmuje
  if(FALSE){
    to_big_animal <<- generate_change_lists(prices_sorted)[[1]]
    to_small_animals <<- generate_change_lists(prices_sorted)[[2]]
    #env <- new.env()
    #assign("to_big_animal", generate_change_lists(prices_sorted)[[1]], envir = parent.env(parent.env(env)))
    #assign("to_small_animals", generate_change_lists(prices_sorted)[[2]], envir = parent.env(parent.env(env)))
  }
  n_rabbits <- sum(stock_status * prices_sorted)
  possible_moves <- c()
  for(i in 1:length(prices_sorted)){
    if(n_rabbits >= prices_sorted[i]){
      possible_moves <- rbind(possible_moves, to_big_animal[[names(prices_sorted[i])]])
    }
  }
  if(!is.null(dim(possible_moves)) && dim(possible_moves)[1] > 0){
    #Sprawdzanie czy mamy wystarczajaco duzo zwierzat w stocku, zeby dokonac wymiany_
    #Nie znalazlam lepszego sposobu na dodanie stock_status do kazdego wiersza
    #zamiast do kazdej kolumny niz transponowanie macierzy dwukrotnie
    enough_animals <- rowMeans(t(t(possible_moves) + stock_status) >= 0)
    #Wszystkie wymiany, na ktore nie mamy zwierzat sa odrzucane
    possible_moves <- possible_moves[enough_animals == 1, , drop=F]
  }
  for(i in names(stock_status)){
    if(stock_status[[i]] != 0){
      possible_moves <- rbind(possible_moves, to_small_animals[[i]])
    }
  }
  possible_moves <- unique(possible_moves)
  if(!is.null(dim(possible_moves)) && dim(possible_moves)[1] > 0){
    #Sprawdzenie czy ruch jest mozliwy ze wzgledu na liczbe zwierzat w calej grze
    future_states <- t(t(possible_moves) + stock_status)
    #Sprawdzamy czy srednia wyrazen logicznych w kazdym rzedzie jest rowna 1,
    #bo interesuja nas tylko te ruchy, na ktore starczy wszystkich zwierzat
    possible_states <- rowMeans(t(t(future_states) <= max_stock)) == 1
    #Bierzemy tylko te ruch, ktore sa mozliwe ze wzglegu na maksymalna liczbe zwierzat
    possible_moves <- as.matrix(possible_moves[possible_states, ])
    #Dorzucamy brak ruchu jako mozliwosc
    possible_moves <- rbind(possible_moves, c(0, 0, 0, 0, 0, 0, 0))
  }
  return(possible_moves)
}
