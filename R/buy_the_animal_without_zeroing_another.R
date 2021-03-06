buy_the_animal_without_zeroing_another <- function (stock_status, animal_name, max_stock = c("krolik" = 60, "owca" = 24, "swinia" = 20,
                                                                                             "krowa" = 12, "kon" = 6, "maly_pies" = 4,
                                                                                             "duzy_pies" = 2)) {
  #print(class(stock_status))
  animals_in_necessary_order <- c("kon", "krowa", "duzy_pies", "swinia", "owca", "maly_pies", "krolik")
  stock_status <- stock_status[animals_in_necessary_order]
  max_stock <- max_stock[animals_in_necessary_order]
  possible_moves <- rbind(moves_generation(stock_status, max_stock), c(0, 0, 0, 0, 0, 0, 0))
  possible_states <- t(t(possible_moves) + stock_status)
  colnames(possible_states) <- animals_in_necessary_order
  #print(as.data.frame(possible_states))
  #cat("ANIMAL NAME", animal_name, "\n")
  #possible_states <- dplyr::filter(as.data.frame(possible_states), animal_name > 0)
  #possible_states <- as.data.frame(possible_states)
  #possible_states <- possible_states[possible_states[, animal_name] > 0, ]
  for (animal in c("kon", "krowa", "swinia", "owca", "krolik")) {
    if (stock_status[animal] > 0) {
      #cat("WSZEDŁEM Z", animal, "\n")
      possible_states <- possible_states[possible_states[, animal] > 0, ]
    }
  }
  # print("DIOasPDASPODAOPSDPASDOASKD")
  # print(stock_status)
  # print(possible_states)
  animals_in_correct_order <- c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
  if(!is.null(dim(possible_states)) && dim(possible_states)[1] > 0){
    the_best_state <- get_the_best_state(possible_states)[animals_in_correct_order]
  }else{
    the_best_state <- stock_status
  }
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