#' @title Jedna gra w Super Farmera
#'
#' @description
#' Funkcja \code{gra()} rozgrywa jedna gre w Super Farmera przy zadanej strategii i zwraca liczbe tur,
#' ktora byla potrzebna, aby gracz wygral. Wykorzystuje funkcje: \code{reproduction()} i \code{win()}
#' Kolejnosc zwierzat we wszystkich wektorach: 1-krolik, 2-owca, 3-swinia, 4-krowa, 5-kon, 6-maly pies, 7-duzy pies
#'
#' @param strategia Strategia, ktora ma byc wykorzystywana do grania
#' @param zwierzeta_w_pudelku Wektor mowiacy ile zwierzat konkretnego typu znajduje sie w calej grze
#' - nie nalezy odejmowac zwierzat, ktore chcemy dac jako warunki poczatkowe, domyslnie c(60, 24, 20, 12, 6, 4, 2)
#' @param warunki_poczatkowe Wektor zwierzat, z ktorymi rozpoczynamy gre, domyslnie c(0, 0, 0, 0, 0, 0, 0)
#' @param warunek_zwycieztwa Wektor mowiacy ktore zwierzeta i w jakiej ilosci nalezy zdobyc aby wygrac,
#' domyslnie c(1, 1, 1, 1, 1, 0, 0)
#' @param zawsze_jeden_krolik Zmienna binarna mowiaca czy gramy w wariant gry, w ktorym zawsze
#' posiadamy co najmniej jednego krolika, domyslinie \code{FALSE} (Uwaga: jesli ustawiamy ten parametr na TRUE,
#' a w warunkach poczatkowych podalismy 0 krolikow, to automatycznie liczba krolikow zostanie zmieniona na 1)
#' @param co_zjada_wilk Wektor zmiennych binarnych, mowiacy ktore zwierzeta sa zjadane przez wilka, domyslnie
#' c(1, 1, 1, 1, 0, 0, 0) (Uwaga: na polu duzego psa mozna podac cokolwiek, poniewaz jest to rozpatrywane
#' dopiero w przypadku, gdy duzego psa nie ma w stadzie gracza. Z zalozenia zawsze duzy pies broni gracza przed wilkiem.)
#'
#' @return turns Liczba tur, ktora byla potrzebna na wygranie gry przy tej strategii
#'
#' @examples
#' wynik <- gra(strategia_rf, zwierzeta_w_pudelku = c(100, 100, 100, 100, 100, 100, 100),
#' warunek_zwycieztwa = c(42, 1, 1, 1, 1, 1, 1), zawsze_jeden_krolik = TRUE)
#'
#' @rdname gra
#'
#' @author Agnieszka Ciepielewska, Marek Wawreniuk, Pawel Gorniak
#'
#' @export

gra <- function(strategia,
                zwierzeta_w_pudelku = c(60, 24, 20, 12, 6, 4, 2),
                warunki_poczatkowe = c(0, 0, 0, 0, 0, 0, 0),
                warunek_zwycieztwa = c(1, 1, 1, 1, 1, 0, 0),
                zawsze_jeden_krolik = FALSE,
                co_zjada_wilk = c(1, 1, 1, 1, 0, 0, 0)){
  die1 <- c("krolik", "krolik", "krolik", "krolik", "krolik", "krolik", "owca", "owca", "owca", "swinia", "krowa", "wilk")
  die2 <- c("krolik", "krolik", "krolik", "krolik", "krolik", "krolik", "owca", "owca", "swinia", "swinia", "kon", "lis")
  animals <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
  changes_array <- matrix(nrow = 1000, ncol = 7, byrow = T)
  if(zawsze_jeden_krolik && warunki_poczatkowe[1] == 0){
    warunki_poczatkowe[1] <- 1
  }
  stock_status <- warunki_poczatkowe
  names(stock_status) <- animals
  max_stock <- zwierzeta_w_pudelku
  names(max_stock) <- animals
  turns <- 0
  i = 1
  while(!win(stock_status, warunek_zwycieztwa)){
    turns <- turns + 1
    #rzut kostkami
    die1_result <- sample(die1, 1)
    die2_result <- sample(die2, 1)
    #reprodukcja
    stock_status <- reproduction(die1_result, die2_result, stock_status, max_stock, zawsze_jeden_krolik, co_zjada_wilk)
    # sprawdzamy czy wygralismy
    if(win(stock_status, warunek_zwycieztwa)){
      return(turns)
    }
    stock_status_tmp <- stock_status
    if(environmentName(environment(strategia)) == "SuperFarmerMAPA"){
      stock_status <- strategia(stock_status, max_stock)
    }else{
      stock_status <- strategia(stock_status)
    }
    changes_array[i, ] <- (stock_status - stock_status_tmp)
    i = i+1
  }
  changes_array <- changes_array[complete.cases(changes_array), ]
  return(list(turns, changes_array))
}
