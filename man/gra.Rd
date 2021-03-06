% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gra.R
\name{gra}
\alias{gra}
\title{Jedna gra w Super Farmera}
\usage{
gra(strategia, zwierzeta_w_pudelku = c(60, 24, 20, 12, 6, 4, 2),
  warunki_poczatkowe = c(0, 0, 0, 0, 0, 0, 0), warunek_zwycieztwa = c(1, 1,
  1, 1, 1, 0, 0), zawsze_jeden_krolik = FALSE, co_zjada_wilk = c(1, 1, 1, 1,
  0, 0, 0))
}
\arguments{
\item{strategia}{Strategia, ktora ma byc wykorzystywana do grania}

\item{zwierzeta_w_pudelku}{Wektor mowiacy ile zwierzat konkretnego typu znajduje sie w calej grze
- nie nalezy odejmowac zwierzat, ktore chcemy dac jako warunki poczatkowe, domyslnie c(60, 24, 20, 12, 6, 4, 2)}

\item{warunki_poczatkowe}{Wektor zwierzat, z ktorymi rozpoczynamy gre, domyslnie c(0, 0, 0, 0, 0, 0, 0)}

\item{warunek_zwycieztwa}{Wektor mowiacy ktore zwierzeta i w jakiej ilosci nalezy zdobyc aby wygrac,
domyslnie c(1, 1, 1, 1, 1, 0, 0)}

\item{zawsze_jeden_krolik}{Zmienna binarna mowiaca czy gramy w wariant gry, w ktorym zawsze
posiadamy co najmniej jednego krolika, domyslinie \code{FALSE} (Uwaga: jesli ustawiamy ten parametr na TRUE,
a w warunkach poczatkowych podalismy 0 krolikow, to automatycznie liczba krolikow zostanie zmieniona na 1)}

\item{co_zjada_wilk}{Wektor zmiennych binarnych, mowiacy ktore zwierzeta sa zjadane przez wilka, domyslnie
c(1, 1, 1, 1, 0, 0, 0) (Uwaga: na polu duzego psa mozna podac cokolwiek, poniewaz jest to rozpatrywane
dopiero w przypadku, gdy duzego psa nie ma w stadzie gracza. Z zalozenia zawsze duzy pies broni gracza przed wilkiem.)}
}
\value{
turns Liczba tur, ktora byla potrzebna na wygranie gry przy tej strategii
}
\description{
Funkcja \code{gra()} rozgrywa jedna gre w Super Farmera przy zadanej strategii i zwraca liczbe tur,
ktora byla potrzebna, aby gracz wygral. Wykorzystuje funkcje: \code{reproduction()} i \code{win()}
Kolejnosc zwierzat we wszystkich wektorach: 1-krolik, 2-owca, 3-swinia, 4-krowa, 5-kon, 6-maly pies, 7-duzy pies
}
\examples{
wynik <- gra(strategia_rf, zwierzeta_w_pudelku = c(100, 100, 100, 100, 100, 100, 100),
warunek_zwycieztwa = c(42, 1, 1, 1, 1, 1, 1), zawsze_jeden_krolik = TRUE)

}
\author{
Agnieszka Ciepielewska, Marek Wawreniuk, Pawel Gorniak
}

