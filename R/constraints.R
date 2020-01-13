#' Block Players Constraint
#'
#' @param model Model object
#' @param block_vector Vector where 1 indicates 'block'
#'
#' @keywords internal
add_block_constraint <- function(model, block_vector) {
  blocks <- which(block_vector == 1)

  if (sum(block_vector) == 0) {
    model <- model
  } else {
    model <- model %>%
      ompr::add_constraint(players[i] == 0, i = blocks)
  }

  return(model)
}


#' Lock Players Constraint
#'
#' @param model Model object
#' @param lock_vector Vector where 1 indicates 'block'
#'
#' @keywords internal
add_lock_constraint <- function(model, lock_vector) {
  locks <- which(lock_vector == 1)

  if (sum(lock_vector) == 0) {
    model <- model
  } else {
    model <- model %>%
      ompr::add_constraint(players[i] == 1, i = locks)
  }

  return(model)
}
