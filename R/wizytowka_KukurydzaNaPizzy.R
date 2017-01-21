#' @title Wizytowka
#'
#' @description Funkcja \code{wizytowka_KukurydzaNaPizzy} generuje wizytowke wybranej
#' strategii w formacie pdf
#'
#' @param strategia Strategia, dla ktorej na byc wygenerowana wizytowka
#' @param powtorz Liczba powtorzen gier
#'
#' @return Wizytowka.pdf Wizytowka, pokazujaca statystyki na temat strategii oraz jej dzialanie
#'
#' @rdname wizytowka_KukurydzaNaPizzy
#'
#' @import gridExtra tidyr grid
#'
#' @export

wizytowka_KukurydzaNaPizzy <- function(strategia,powtorz=10000){

  lista <- SuperFarmerMAPA::badaj_gre(strategia,powtorz)

  dane <- unlist(lista[[1]])
  m <- lista[[2]]

  lay <- rbind(c(1,1),
               c(4,5),
               c(4,2),
               c(3,2),
               c(3,2),
               c(3,2),
               c(7,6))

  tytul <- grid::textGrob(gsub("::","\n",deparse(substitute(strategia))), gp=grid::gpar(fontsize=30))
  p1 <-gridExtra::tableGrob(staty(dane, strsplit(deparse(substitute(strategia)),"::")[[1]][2]),theme = ttheme_kukurydza())
  p2 <- rysuj_gestosc(dane)
  #tutaj zapisujemy plik
  png('rplot.png',width = 1280, height = 1280, res = 200)
  kolowy(m)
  dev.off()
  # tu go wczytujemy
  p3_tytul <- grid::textGrob("Przepływ wartosci w krolikach w wymianach", gp=grid::gpar(fontsize=13), just = "center")
   p3 <- rasterGrob(readPNG('rplot.png'))
  #######TO zapisywanie i wczytywanie pliku jest baaaaaaaaaaaaardzo nieeleganckie!!!!!
  p3_opis <- grid::textGrob("Wykres wizualizuje wymiany dokonywane przez strategię.\n
Jednostką wykresu jest wartość jednego królika.\n
Długość wycinka odpowiadającego zwierzęciu pokazuje udział jego wartości w we wszystkich\n
wymianach (cały okrąg).\n
Przepływy między wycinkami pokazują, na co i jak wartościowe były wymiany zwierzęcia\n
o odpowiednim kolorze.",
                            gp=grid::gpar(fontsize=10, lineheight	= 0.5),
                            x = unit(0, "npc"),
                            y = unit(0.7, "npc"),
                            just = "left")
  p_podpis <- grid::textGrob("grupa KukurydzaNaPizzy", gp=grid::gpar(fontsize=8), just = "left")
  nazwa_pliku <- paste0("Wizytowka_",gsub('\"',"",gsub("::","_",gsub("\\.","",deparse(substitute(strategia))))),".pdf")
  pdf(nazwa_pliku,width=11.69, height=8.27)
  grid.arrange(tytul,p3,p2,p1,p3_tytul,p3_opis,p_podpis,layout_matrix = lay)
  dev.off()
}
