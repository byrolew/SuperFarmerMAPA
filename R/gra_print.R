#' @title Jedna gra w Super Farmera
#' 
#' @description 
#' Funkcja \code{gra()} rozgrywa jedna gre w Super Farmera przy zadanej strategii i zwraca liczbe tur, 
#' ktora byla potrzebna, aby gracz wygral. Wykorzystuje funkcje: \code{reproduction()} i \code{win()}.
#' Dodatkowo wypisuje stan stada gracza.
#' 
#' @param strategia Strategia, ktora ma byc wykorzystywana do grania
#' 
#' @return turns Liczba tur, ktora byla potrzebna na wygranie gry przy tej strategii
#' 
#' @examples 
#' wynik <- gra(strategia)
#' 
#' @rdname gra
#' 
#' @author Agnieszka Ciepielewska, Marek Wawreniuk
#'
#' @export

gra_print <- function(strategia){
  die1 <- c("krolik", "krolik", "krolik", "krolik", "krolik", "krolik", "owca", "owca", "owca", "swinia", "krowa", "wilk")
  die2 <- c("krolik", "krolik", "krolik", "krolik", "krolik", "krolik", "owca", "owca", "swinia", "swinia", "kon", "lis")
  animals <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
  stock_status <- c(0, 0, 0, 0, 0, 0, 0)
  names(stock_status) <- animals
  max_stock <- c(60, 24, 20, 12, 6, 4, 2)
  names(max_stock) <- animals
  turns <- 0
  while(!win(stock_status)){
    turns <- turns + 1
    #rzut kostkami
    die1_result <- sample(die1, 1)
    die2_result <- sample(die2, 1)
    #reprodukcja
    stock_status <- reproduction(die1_result, die2_result, stock_status, max_stock)
    # sprawdzamy czy wygralismy
    if(win(stock_status)){
      return(turns)
    }
    stock_status <- strategia(stock_status)
    print(stock_status)
  }
  return(turns)
}