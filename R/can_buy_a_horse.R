can_buy_a_horse <- function (stock_status) {
  return (stock_worth(stock_status) - (stock_status["kon"] * 72) >= 72)
}