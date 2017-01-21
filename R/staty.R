#' @title Zrob statystyki
#'
#' @rdname staty

staty <- function(dane, nazwa){
  statdane <-unclass(summary(dane))
  names(statdane) <- c("Min.", "1szy kw.", "Med.", "Śred.", "3ci kw.", "Max.")
  statnajszyb <- unclass(summary(SuperFarmerMAPA::najszybsza))
  statnajwol <- unclass(summary(SuperFarmerMAPA::najwolniejsza))
  names(statnajszyb) <- NULL
  names(statnajwol) <- NULL
  statystyki <- t(data.frame(statnajszyb, statdane,  statnajwol, check.names = FALSE, stringsAsFactors = FALSE))
  rownames(statystyki) <- c("strategy_maxrabbit",nazwa,"strategia_anty_yolo")

  return(round(statystyki))
}
