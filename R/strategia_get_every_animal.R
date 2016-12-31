#' @export
strategia_get_every_animal <- function(stock_status) {
  animals_without_dogs <-  c("kon", "krowa", "swinia", "owca", "krolik")
  for (animal_name in animals_without_dogs) {
    if (stock_status[animal_name] == 0) {
      return (buy_the_animal_without_zeroing_another(stock_status, animal_name))
    }
  }
}