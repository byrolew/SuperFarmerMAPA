expected_value <- function(stock_status, max_stock = c("krolik" = 60, "owca" = 24, "swinia" = 20,
                                                       "krowa" = 12, "kon" = 6, "maly_pies" = 4,
                                                       "duzy_pies" = 2)) {
  #zakladamy, ze nie kupujemy psow
  
  R <- stock_status["krolik"]
  S <- stock_status["owca"]
  P <- stock_status["swinia"]
  C <- stock_status["krowa"]
  H <- stock_status["kon"]
  rabbits_left <- max_stock["krolik"] - R
  sheeps_left <- max_stock["owca"] - S
  pigs_left <- max_stock["swinia"] - P
  cows_left <- max_stock["krowa"] - C
  horses_left <- max_stock["kon"] - H
  expected_rabbits_income <- min(floor((R+2)/2), rabbits_left)/4 + 
    min(floor((R+1)/2), rabbits_left)*(5/12) - 23*R/144
  expected_sheeps_income <- min(floor((S+2)/2), sheeps_left)/4 + 
    min(floor((S+1)/2), sheeps_left)*(23/12) - S/2
  expected_pigs_income <- min(floor((P+2)/2), pigs_left)/6 + 
    min(floor((P+1)/2), pigs_left)*(5/2) - P
  expected_cows_income <- min(floor((C+1)/2), cows_left)*3 - C*3
  expected_horses_income <- min(floor((H+1)/2), horses_left)*30
  value <- expected_rabbits_income + expected_sheeps_income + expected_pigs_income +
    expected_cows_income + expected_horses_income
  return (value)
}