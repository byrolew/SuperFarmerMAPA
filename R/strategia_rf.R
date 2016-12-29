#' @title Strategia oparta na Random Forest
#'
#' @description
#' Funkcja \code{strategia_rf()} wybiera ruch gracza wedlug przewidywan modelu random forest wgranego do pakietu.
#' Model przewiduje, ktory ruch doprowadzi do najszybszego zwycieztwa. Wykorzystuje funkcje \code{moves_generation()},
#' zeby miec z czego wybierac ruch
#'
#' @param stock_status Stan stada gracza przed ruchem
#'
#' @return stock_status Stan stada gracza po ruchu
#'
#' @examples
#' stan_stada <- c(2,4,1,0,0,0,0)
#' stan_stada <- strategia_rf(stan_stada)
#'
#' @rdname strategia_rf
#'
#' @author Agnieszka Ciepielewska
#'
#' @export

strategia_rf <- function(stock_status){
  stock_status <- stock_status[c(5,4,7,3,2,6,1)]
  possible_moves <- moves_generation(stock_status)
  if(!is.null(dim(possible_moves)) && dim(possible_moves)[1] > 0){
    df1 <- matrix(stock_status, nrow = nrow(possible_moves), ncol = 7, byrow = T)
    df1 <- cbind(df1, possible_moves)

    n_types_after <- rowSums((df1[, 1:7] + df1[, 8:14])[, c(1, 2, 4, 5, 7)] > 0)
    n_types_before <- rowSums((df1[, 1:7])[, c(1, 2, 4, 5, 7)] > 0)
    #zeby modelowi latwiej bylo sie uczyc oraz przewidywac dorzucamy kolumny mowiace
    #ile rodzajow zwierzat potrzebnych do wygranej mielismy,
    #ile bedziemy miec po ruchu i jaka jest roznica
    df1 <- cbind(df1, n_types_before)
    df1 <- cbind(df1, n_types_after)
    df1 <- cbind(df1, (n_types_after - n_types_before))
    colnames(df1) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9",
                      "X10", "X11", "X12", "X13", "X14", "X16", "X17", "X18")
    pred <- randomForest:::predict.randomForest(forest, df1)
    #Bierzemy ruch, ktory otrzymal od naszego modelu najmniejsza przewidywana liczbe ruchow do wygranej
    which_move <- sample(which(pred == min(pred)), 1)
    #Jako, ze czasami wystepuja dwa ruchy o tej samej predykcji, to bierzemy losowy z nich
    move <- possible_moves[which_move, ]
    stock_status <- stock_status + move
  }
  stock_status <- stock_status[c(7,5,4,2,1,6,3)]
  return(stock_status)
}
