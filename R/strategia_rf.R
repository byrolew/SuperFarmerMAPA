#' @export

strategia_rf <- function(stock_status){
  forest <- rf
  stock_status <- stock_status[c(5,4,7,3,2,6,1)]
  possible_moves <- moves_generation(stock_status)
  if(!is.null(dim(possible_moves)) && dim(possible_moves)[1] > 0){
    df1 <- matrix(stock_status, nrow = nrow(possible_moves), ncol = 7, byrow = T)
    df1 <- cbind(df1, possible_moves)
    
    
    n_types_after <- rowSums((df1[, 1:7] + df1[, 8:14])[, c(1, 2, 4, 5, 7)] > 0)
    n_types_before <- rowSums((df1[, 1:7])[, c(1, 2, 4, 5, 7)] > 0)
    #(Dorzucamy te same kolumny co w funkcji "forest_training_and_results", 
    #?eby random forest mia? tyle samo zmiennych)
    #?eby modelowi ?atwiej by?o si? uczy? oraz przewidywa? dorzucamy kolumny m?wi?ce 
    #ile rodzaj?w zwierz?t potrzebnych do wygranej mieli?my, 
    #ile b?dziemy mie? po ruchu i jaka jest r??nica
    df1 <- cbind(df1, n_types_before)
    df1 <- cbind(df1, n_types_after)
    df1 <- cbind(df1, (n_types_after - n_types_before))
    colnames(df1) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", 
                      "X10", "X11", "X12", "X13", "X14", "X16", "X17", "X18")
    pred <- predict(forest, df1)
    #Bierzemy ruch, kt?ry otrzyma? od naszego modelu najmniejsz? przewidywan? liczb? ruch?w do wygranej 
    which_move <- sample(which(pred == min(pred)), 1)
    #Jako, ?e czasami wyst?puj? dwa ruchy o tej samej predykcji, to bierzemy losowy z nich
    move <- possible_moves[which_move, ]
    stock_status <- stock_status + move
  }
  stock_status <- stock_status[c(7,5,4,2,1,6,3)]
  return(stock_status)
}