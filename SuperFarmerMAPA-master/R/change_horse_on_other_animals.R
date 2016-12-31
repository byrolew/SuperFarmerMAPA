#zakładam, że tutaj mi wystarczy wszystkiego (czyli,
#że na przykład na pewno jest jeszcze w stadzie 6 królików)
change_horse_on_other_animals <- function(stock_status) {
  stock_status["kon"] <- stock_status["kon"] - 1
  stock_status["krowa"] <- stock_status["krowa"] + 1
  stock_status["swinia"] <- stock_status["swinia"] + 1
  stock_status["owca"] <- stock_status["owca"] + 3
  stock_status["krolik"] <- stock_status["krolik"] + 6
  return (stock_status)
}