expected_value <- function(stock_status) {
  #zakladamy, ze nie kupujemy psow
  R <- stock_status["krolik"]
  S <- stock_status["owca"]
  P <- stock_status["swinia"]
  C <- stock_status["krowa"]
  H <- stock_status["kon"]
  value <- 2/3 + 37*R/144 - floor(R/2)/6 + 17*S/12 - 5*floor(S/2)/3 + 3*P/2 -
    7*floor(P/2)/3 - 3*floor(C/2) + 6*floor((H+1)/2)
  return (value)
}