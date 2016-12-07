split <- function(n_rabbits, prices_sorted, pre_moves=c(), moves=c()){
  ratio <- floor(n_rabbits/prices_sorted[1])
  #Warunek koncowy
  if(length(prices_sorted) == 1){
    return(c(pre_moves, ratio))
  }
  for(i in 0:ratio){
    #Dopisywany jest tutaj kolejny wiersz macierzy ruchow
    moves <- rbind(moves, split(n_rabbits - i*prices_sorted[1],
                               prices_sorted[-1],
                               c(pre_moves, i),
                               moves))
  }
  return(unique(moves))
}