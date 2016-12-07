strategy_HEV <- function(stock_status) {
  if (stock_status["kon"] > 1) {
    return (change_horse_on_other_animals(stock_status))
  } else {
    return (strategy_EV(stock_status))
  }
}