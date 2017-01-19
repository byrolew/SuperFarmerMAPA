#' @title Wizytowka
#'
#' @description Funkcja \code{wizytowka_KukurydzaNaPizzy} generuje wizytowke wybranej
#' strategii w formacie pdf
#'
#' @param strategia Strategia, dla ktorej na byc wygenerowana wizytowka
#'
#' @return Wizytowka.pdf Wizytowka, pokazujaca statystyki na temat strategii oraz jej dzialanie
#'
#' @rdname wizytowka_KukurydzaNaPizzy
#'
#' @import gridExtra tidyr grid
#'
#' @export

wizytowka_KukurydzaNaPizzy <- function(strategia){

  tt1 <- gridExtra::ttheme_default(
    core = list(bg_params=list(fill=c("aliceblue", "pink"))),
    colhead = list(fg_params=list(col="white"),
                   bg_params=list(fill="cadetblue3"))
  )


  lista <- SuperFarmerMAPA::badaj_gre(strategia)
  dane <- unlist(lista[[1]])
  m <- lista[[2]]
  lay <- rbind(c(1,1),
               c(2,2),
               c(3,3),
               c(3,3),
               c(3,3),
               c(5,5),
               c(4,4),
               c(4,4),
               c(4,4),
               c(4,4),
               c(6,6))

  tytul <- grid::textGrob(gsub("::","\n",deparse(substitute(strategia))), gp=grid::gpar(fontsize=30))
  stat <-unclass(summary(dane))
  names(stat) <- c("Min.", "1szy kw.", "Mediana", "Srednia", "3ci kw.", "Max.")
  statystyki <- t(data.frame(stat, check.names = FALSE, stringsAsFactors = FALSE))
  rownames(statystyki) <- NULL
  p1 <-gridExtra::tableGrob(statystyki,theme = tt1)
  p2 <- rysuj_gestosc(dane)
  #tutaj zapisujemy plik
  png('rplot.png',width = 1280, height = 1280, res = 200)
  kolowy(m)
  dev.off()
  # tu go wczytujemy
  p3_tytul <- grid::textGrob("        Przeplyw wartosci w krolikach w wymianach", gp=grid::gpar(fontsize=13), just = "right")
  p3 <- rasterGrob(readPNG('rplot.png'))
  #######TO zapisywanie i wczytywanie pliku jest baaaaaaaaaaaaardzo nieeleganckie!!!!!

  p_podpis <- grid::textGrob("grupa KukurydzaNaPizzy", gp=grid::gpar(fontsize=8), just = "left")
  nazwa_pliku <- paste0("Wizytowka_",gsub('\"',"",gsub("::","_",gsub("\\.","",deparse(substitute("SuperFarmer.SuperDziewczyn::strategia_owce"))))),".pdf")
  pdf(nazwa_pliku,width=8.27, height=11.69)
  grid.arrange(tytul,p1,p2,p3,p3_tytul,p_podpis,layout_matrix = lay)
  dev.off()
}
