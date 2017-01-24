#' @title Wykres kolowy, pokazujacy zamiany
#'
#' @description Generuje wykres kolowy wymian
#'
#' @rdname kolowy
#'
#' @author Hanna Kranas, Alicja Gosiewska, Agnieszka Ciepielewska
#'
#' @import png circlize dplyr

kolowy <- function(m){
  colnames(m) = c("króliki","owce","świnie","krowy", "konie", "małe psy", "duże psy")
  rownames(m) = c("króliki","owce","świnie","krowy", "konie", "małe psy", "duże psy")
  df1 <- data.frame(matrix(nrow = 7, ncol = 3))
  df1[1, ] <- c(1, "153, 255, 51", "króliki")
  df1[2,] <- c(2, "255, 255, 102", "owce")
  df1[3,] <- c(3, "255, 102, 204", "świnie")
  df1[4,] <- c(4, "255,100,0", "krowy")
  df1[5,] <- c(5, "123,52,21", "konie")
  df1[6,] <- c(6, "150,150,255", "małe psy")
  df1[7,] <- c(7, "50,50,255", "duże psy")
  colnames(df1) <- c("order", "rgb", "gatunek")

  #sort regions and create colours
  df1 <- df1 %>% arrange(order) %>% separate(rgb, c("r","g","b")) %>%
    mutate(col = rgb(r, g, b, max=255), max = rowSums(m)+colSums(m))

  #plot using chordDiagram
  circos.clear()
  par(mar = rep(0, 4), cex=0.9)
  circos.par(start.degree = 90, gap.degree = 2)
  chordDiagram(x = m, directional = 1, order = df1$region,
               grid.col = df1$col, annotationTrack = "grid",
               transparency = 0.25, annotationTrackHeight = c(0.05),
               diffHeight  = 0.00, preAllocateTracks = list(list(track.height = 0.05),
                                                            list(track.height = 0.05)))

  #add in labels and axis
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), 0.5, sector.index, facing = "bending", cex = 2, niceFacing = TRUE, adj = c(0.5, 0.5))
  }, bg.border = NA)
  circos.clear()
}
