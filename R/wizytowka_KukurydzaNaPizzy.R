#' @title Wizytowka
#'
#' @description Funkcja \code{wizytowka_KukurydzaNaPizzy} generuje wizytowke wybranej
#' strategii w formacie pdf
#'
#' @param strategia Strategia, dla ktorej na byc wygenerowana wizytowka
#' @param powtorz Liczba powtorzen gier
#' @param sciezka Sciezka do pliku, gdzie ma sie zapisac wizytowka
#'
#' @return KukurydzaNaPizzy_Wizytowka_strategia.pdf Wizytowka, pokazujaca statystyki na
#' temat strategii oraz jej dzialanie. Zostaje ona zapisana w folderze \code{inst} wewnatrz
#' pakietu
#'
#' @rdname wizytowka_KukurydzaNaPizzy
#'
#' @author Hanna Kranas, Alicja Gosiewska, Agnieszka Ciepielewska
#'
#' @import gridExtra tidyr grid grDevices
#'
#' @export

wizytowka_KukurydzaNaPizzy <- function(strategia,powtorz=10000, sciezka){

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
  if(grepl("::",deparse(substitute(strategia)))){
    nazwa <- strsplit(deparse(substitute(strategia)),"::")[[1]][2]
  } else {
    nazwa <- deparse(substitute(strategia))
  }
  p1 <-gridExtra::tableGrob(staty(dane, nazwa),theme = ttheme_kukurydza())
  p2 <- rysuj_gestosc(dane)
  #tutaj zapisujemy plik
  png('rplot.png',width = 1280, height = 1280, res = 200)
  kolowy(m)
  dev.off()
  # tu go wczytujemy
  p3_tytul <- grid::textGrob("Przepływ wartości w królikach w wymianach", gp=grid::gpar(fontsize=13), just = "center")
   p3 <- rasterGrob(readPNG('rplot.png'))
  #######TO zapisywanie i wczytywanie pliku jest baaaaaaaaaaaaardzo nieeleganckie!!!!!
  p3_opis <- grid::textGrob("Wykres wizualizuje wymiany dokonywane przez strategię.\n
Jednostką wykresu jest wartość jednego krolika.\n
Długość wycinka odpowiadającego zwięrzeciu pokazuje udział jego wartości\n
we wszystkich wymianach (cały okrąg).\n
Przepływy między wycinkami pokazują, na co i jak wartościowe były wymiany zwierzęcia\n
o odpowiednim kolorze.",
                            gp=grid::gpar(fontsize=10, lineheight	= 0.5),
                            x = unit(0, "npc"),
                            y = unit(0.7, "npc"),
                            just = "left")
  p_podpis <- grid::textGrob("grupa KukurydzaNaPizzy: Agnieszka Ciepielewska, Alicja Gosiewska, Hanna Kranas", gp=grid::gpar(fontsize=10), just = "center")
  nazwa_pliku <- paste0("KukurydzaNaPizzy_Wizytowka_",gsub('\"',"",gsub("::","_",gsub("\\.","",deparse(substitute(strategia))))),".pdf")
  cairo_pdf(paste0("inst/",nazwa_pliku),width=11.69, height=8.27)
  grid.arrange(tytul,p3,p2,p1,p3_tytul,p3_opis,p_podpis,layout_matrix = lay)
  dev.off()
}
