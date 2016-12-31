#' @title Strategia Get Every Animal
#' 
#' @description Strategia polega na zakupywaniu najwyzszego zwierzecia na jakie nas stac, ktorego nie mamy. 
#' Robione jest to w sposob optymalny, poniewaz korzystamy z \code{expected_value()}. Podczas zakupu brane jest
#' pod uwage to aby nie zerowac jednego zwierzecia kosztem innego.
#' 
#' @param stock_status Stan stada gracza przed ruchem
#' 
#' @return Stan stada gracza po ruchu
#' 
#' @rdname strategia_get_every_animal
#' 
#' @author Marek Wawreniuk
#'
#' @export
strategia_get_every_animal <- function(stock_status) {
  animals_without_dogs <-  c("kon", "krowa", "swinia", "owca", "krolik")
  for (animal_name in animals_without_dogs) {
    if (stock_status[animal_name] == 0) {
      return (buy_the_animal_without_zeroing_another(stock_status, animal_name))
    }
  }
}