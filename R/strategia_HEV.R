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