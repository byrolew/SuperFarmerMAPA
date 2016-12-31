#' @title Strategia One Last Chance
#' 
#' @description Strategia bedaca kolejnym rozszerzeniem strategii \code{strategia_EV} i \code{strategia_HEV}.
#'  Jesli mamy ponad dwa konie, to tak jak poprzednio, jesli nie to jesli stac nas na konia, to rowniez 
#'  jak poprzednio. W przeciwnym
#'   przypadku jesli liczba krolikow przekracza number_of_rabbits_to_protect to chronimy je (czyli jesli 
#'   nie ma malego psa, to go kupujemy, jesli jest, to kupujemy swinie, bo to obniza liczbe krolikow 
#'   (zauwazmy, ze nie ma co trzymac wiecej niz 40 krolikow, bo przy potencjalnym wyrzuceniu krolika, 
#'   nasz przyrost jest ograniczany przez maksymalna liczbe krolikow w stadzie glownym)), przy czym robimy 
#'   to w przypadku jesli nadal nas nie stac na wygranie gry. Jesli nas stac, to wracamy do strategii 
#'   poprzedniej (\code{strategia_HEV}).
#'
#' @param stock_status Stan stada gracza przed ruchem
#' @param max_stock Stan stada glownego na poczatku gry.
#' 
#' @return Stan stada gracza po ruchu
#' 
#' @rdname strategia_OLC
#' 
#' @author Marek Wawreniuk
#'
#' @export

strategia_OLC <- function (stock_status, max_stock = c("krolik" = 60, "owca" = 24, "swinia" = 20,
                                                     "krowa" = 12, "kon" = 6, "maly_pies" = 4,
                                                     "duzy_pies" = 2), number_of_rabbits_to_protect = 37) {
  #if mamy dwa konie -> rób wygraną
  #else jeśli stać nas na konia -> kupuj konia
  #else jeśli mamy więcej niż 40 królików -> jeśli nie mamy małego psa -> kupuj małego psa else kupuj świnię
  #else strategy_ev
  
  if (stock_status["kon"] >= 2) { return (change_horse_on_other_animals(stock_status))
  } else if (can_buy_a_horse(stock_status) ) { return (buy_the_animal(stock_status, "kon", max_stock))
  } else if (stock_status["krolik"] >= number_of_rabbits_to_protect && 
             stock_worth(stock_status) < 127) { return (protect_the_rabbits(stock_status, max_stock))
  } else { return (strategia_HEV(stock_status, max_stock))}
}