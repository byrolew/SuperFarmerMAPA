#' @title Strategia Expected Value
#' 
#' @description Strategia polega na wybraniu ruchu, dla ktorego wartosc
#'  oczekiwana liczona w krolikach jest najwieksza. Funkcja \code{moves_generation()} 
#'  generuje wszystkie mozliwe ruchy. Funkcja \code{get_the_best_state} zwraca ruch o 
#'  największej 
#'  
#' @param stock_status Stan stada gracza przed ruchem
#' @param max_stock Stan stada glownego na poczatku gry.
#' 
#' @return Stan stada gracza po ruchu
#' @param max_stock Stan stada glownego na poczatku gry.
#' 
#' @rdname strategia_EV
#' 
#' @author Marek Wawreniuk
#'
#' @export
strategia_EV <- function (stock_status, max_stock = c("krolik" = 60, "owca" = 24, "swinia" = 20,
                                                     "krowa" = 12, "kon" = 6, "maly_pies" = 4,
                                                     "duzy_pies" = 2)) {
  animals_in_necessary_order <- c("kon", "krowa", "duzy_pies", "swinia", "owca", "maly_pies", "krolik")
  stock_status <- stock_status[animals_in_necessary_order]
  max_stock <- max_stock[animals_in_necessary_order]
  possible_moves <- rbind(moves_generation(stock_status, max_stock), c(0, 0, 0, 0, 0, 0, 0))
  possible_states <- t(t(possible_moves) + stock_status)
  colnames(possible_states) <- animals_in_necessary_order
  animals_in_correct_order <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
  return (get_the_best_state(possible_states, max_stock)[animals_in_correct_order])
}