win <- function(stock_status){
  min(stock_status[c(1:5)]) > 0
}