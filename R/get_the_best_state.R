get_the_best_state <- function(possible_states) {
  ev_vector <- apply(possible_states, 1, expected_value)
  return (possible_states[which.max(ev_vector),])
}#TODO żeby zwracało wektor o najlepszej wartości oczekiwanej