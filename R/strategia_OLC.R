#' @export

strategia_OLC <- function (stock_status, max_stock = c("krolik" = 60, "owca" = 24, "swinia" = 20,
                                                     "krowa" = 12, "kon" = 6, "maly_pies" = 4,
                                                     "duzy_pies" = 2), number_of_rabbits_to_protect = 37) {
  #if mamy dwa konie -> rób wygraną
  #else jeśli stać nas na konia -> kupuj konia
  #else jeśli mamy więcej niż 40 królików -> jeśli nie mamy małego psa -> kupuj małego psa else kupuj świnię
  #else strategy_ev
  
  if (stock_status["kon"] >= 2) { return (change_horse_on_other_animals(stock_status))
  } else if (can_buy_a_horse(stock_status) ) { return (buy_the_animal(stock_status, "kon"))
  } else if (stock_status["krolik"] >= number_of_rabbits_to_protect && 
             stock_worth(stock_status) < 127) { return (protect_the_rabbits(stock_status))
  } else { return (strategia_HEV(stock_status, max_stock))}
}