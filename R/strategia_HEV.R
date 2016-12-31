#' @title Strategia Horse Expected Value
#' 
#' @description Taktyka bedaca malym rozszerzeniem \code{strategia_EV()}. Uwzglednia ona to, ze poprzednia
#'  nie bierze pod uwage tego, ze wygrana partii nastepuje po tym jak zdobedziemy wszystkie zwierzeta, nie
#' po tym jak bedziemy mieli najwieksza wartosc stada w krolikach. Dlatego po pierwsze skupiamy sie na 
#' zakupie reszty zwierzat jesli mamy dwa konie. Potem na zakupie konia jesli nast tylko na to stac i
#' konia nie mamy, nastepnie jesli stac nas na wygranie gry, to kupujemy wszystkie zwierzeta po kolei, 
#'  ale w taki sposob zeby nie pozbywac sie innych (zeby nie wyzerowac jednego kosztem innego). Jesli 
#' zaden z powyzszych warunkow nie zachodzi, to wracamy do strategii \code{strategia_EV}.
#'
#' @param stock_status Stan stada gracza przed ruchem
#' @param max_stock Stan stada glownego na poczatku gry.
#' 
#' @return Stan stada gracza po ruchu
#' 
#' @rdname strategia_HEV
#' 
#' @author Marek Wawreniuk
#'
#' @export
strategia_HEV <- function(stock_status, max_stock = c("krolik" = 60, "owca" = 24, "swinia" = 20,
                                                     "krowa" = 12, "kon" = 6, "maly_pies" = 4,
                                                     "duzy_pies" = 2)) {
  if (stock_status["kon"] > 1) {
    return (change_horse_on_other_animals(stock_status))
  } else if (stock_status["kon"] == 0 && stock_worth(stock_status) >= 72) {
    return (buy_the_animal(stock_status, "kon"))
  } else if (stock_worth(stock_status) >= 127) {
    return (strategia_get_every_animal(stock_status))
  } else {
    return (strategia_EV(stock_status, max_stock))
  }
}