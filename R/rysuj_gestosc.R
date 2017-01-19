#' @title Rysuj gestosc
#'
#' @rdname rysyj_gestosc
#'
#' @import ggplot2

rysuj_gestosc <- function(dane){
  dane <- data.frame(dane, SuperFarmerMAPA::najszybsza, SuperFarmerMAPA::najwolniejsza)
  p<-ggplot2::ggplot(dane) +
    ggplot2::geom_density(ggplot2::aes(SuperFarmerMAPA::najszybsza, colour = "najszybsza"), fill = "red", alpha=0.1, show.legend=TRUE)+
    ggplot2::geom_density(ggplot2::aes(SuperFarmerMAPA::najwolniejsza, colour = "najwolniejsza"), fill = "green", alpha=0.1,show.legend=TRUE)+
    ggplot2::geom_density(ggplot2::aes(dane, colour="badana"), alpha=0, size = 1.25,show.legend=TRUE)+
    ggplot2::theme_bw()+
    ggplot2::xlab("Czas trwania gry")+
    ggplot2::xlim(c(0,150))+
    ggplot2::ylab("Gestosc")+
    ggplot2::labs(title="Porownanie gestosci czasu gry")+
    ggplot2::scale_colour_manual(name = 'Strategia', values =c('badana'='black','najszybsza'='red','najwolniejsza'='green'))
  return(p)
}
