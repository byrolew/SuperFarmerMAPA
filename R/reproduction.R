reproduction <- function(die1_result, die2_result, stock_status, max_stock){
  if(die1_result != "wilk" && die2_result != "lis"){
    stock_status <- update_stocku(stock_status, max_stock, die1_result, die2_result)
  } else if(die1_result == "wilk" && die2_result != "lis"){
    if(stock_status[["duzy_pies"]] == 0){
      stock_status[["krolik"]] <- 0
      stock_status[["owca"]] <- 0
      stock_status[["swinia"]] <- 0
      stock_status[["krowa"]] <- 0
    } else {
      stock_status[["duzy_pies"]] <- stock_status[["duzy_pies"]] - 1
    }
    stock_status <- update_stocku(stock_status, max_stock, die2_result)
  } else if(die1_result != "wilk" && die2_result == "lis"){
    if(stock_status[["maly_pies"]] == 0){
      stock_status[["krolik"]] <- 0
    } else {
      stock_status[["maly_pies"]] <- stock_status[["maly_pies"]] - 1
    }
    stock_status <- update_stocku(stock_status, max_stock, die1_result)
  } else {
    if(stock_status[["duzy_pies"]] == 0){
      stock_status[["krolik"]] <- 0
      stock_status[["owca"]] <- 0
      stock_status[["swinia"]] <- 0
      stock_status[["krowa"]] <- 0
    } else {
      stock_status[["duzy_pies"]] <- stock_status[["duzy_pies"]] - 1
    }
    if(stock_status[["maly_pies"]] == 0){
      stock_status[["krolik"]] <- 0
    } else {
      stock_status[["maly_pies"]] <- stock_status[["maly_pies"]] - 1
    }
  }
  return(stock_status)
}