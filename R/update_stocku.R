update_stocku <- function(stock_status, max_stock, die1_result = "", die2_result = ""){
  if(die1_result != die2_result){
    if(die1_result != ""){
      stock_status[die1_result] <- min(floor(stock_status[die1_result] + (stock_status[die1_result]+1)/2), max_stock[die1_result])
    }
    if(die2_result != ""){
      stock_status[die2_result] <- min(floor(stock_status[die2_result] + (stock_status[die2_result]+1)/2), max_stock[die2_result])
    }
  } else {
    stock_status[die1_result] <- min(floor(stock_status[die1_result] + (stock_status[die1_result]+2)/2), max_stock[die1_result])
  }
  return(stock_status)
}