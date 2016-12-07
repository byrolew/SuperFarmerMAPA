strategia <- function(stock_status){
  stock_status <- stock_status + c(0, 0, 0, 1, 1, 0, 0)
  return(stock_status)
}