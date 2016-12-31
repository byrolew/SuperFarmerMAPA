protect_the_rabbits <- function (stock_status) {
  if (stock_status["maly_pies"] > 0) {
    return (buy_the_animal(stock_status, "swinia"))
  } else {
    return (buy_the_animal(stock_status, "maly_pies"))
  }
}