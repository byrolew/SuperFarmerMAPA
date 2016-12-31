stock_worth <- function(stock_status) {
  animals_in_correct_order <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
  stock_status <- stock_status[animals_in_correct_order];
  animals_prices_in_rabbits <- c(1, 6, 12, 36, 72, 0, 0)
  return (sum(stock_status*animals_prices_in_rabbits))
}