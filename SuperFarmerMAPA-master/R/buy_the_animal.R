buy_the_animal <- function(stock_status, animal_name) {
  #print(class(stock_status))
  animals_in_necessary_order <- c("kon", "krowa", "duzy_pies", "swinia", "owca", "maly_pies", "krolik")
  stock_status <- stock_status[animals_in_necessary_order]
  possible_moves <- rbind(moves_generation(stock_status), c(0, 0, 0, 0, 0, 0, 0))
  possible_states <- t(t(possible_moves) + stock_status)
  colnames(possible_states) <- animals_in_necessary_order
  #print(as.data.frame(possible_states))
  possible_states <- dplyr::filter(as.data.frame(possible_states), animal_name > 0)
  animals_in_correct_order <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
  the_best_state <- get_the_best_state(possible_states)[animals_in_correct_order]
  the_best_state <- c("krolik" = as.numeric(the_best_state[1]), 
                      "owca" = as.numeric(the_best_state[2]), 
                      "swinia" = as.numeric(the_best_state[3]), 
                      "krowa" = as.numeric(the_best_state[4]), 
                      "kon" = as.numeric(the_best_state[5]), 
                      "maly_pies" = as.numeric(the_best_state[6]), 
                      "duzy_pies" = as.numeric(the_best_state[7]))
  #print(the_best_state)
  return (the_best_state)
}