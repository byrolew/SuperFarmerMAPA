#' @title Zrob statystyki
#'
#' @rdname staty

staty <- function(dane){
  statdane <-unclass(summary(dane))
  names(statdane) <- c("Min.", "1szy kw.", "Mediana", "Srednia", "3ci kw.", "Max.")
  statnajszyb <- unclass(summary(SuperFarmerMAPA::najszybsza))
  statnajwol <- unclass(summary(SuperFarmerMAPA::najwolniejsza))
  names(statnajszyb) <- NULL
  names(statnajwol) <- NULL
  statystyki <- t(data.frame(statnajszyb, statdane,  statnajwol, check.names = FALSE, stringsAsFactors = FALSE))
  rownames(statystyki) <- c("najszybsza","badana","najwolniejsza")
  # TODO tu można potem dodać zmieniane nazwy w zależności od pakietu
  return(statystyki)
}
