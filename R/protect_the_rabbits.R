protect_the_rabbits <- function (stock_status, max_stock) {
  if (stock_status["maly_pies"] > 0) {
    return (buy_the_animal(stock_status, "swinia", max_stock))
  } else {
    return (buy_the_animal(stock_status, "maly_pies", max_stock))
  }
}