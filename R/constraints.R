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


#' Opposite Positions Constraint
#'
#' @param model Model object
#' @param pos1 Positions for set one
#' @param pos2 Positions for set two
#' @param players List of Player objects
#'
#' @details The parameters \code{pos1} and \code{pos2} refer to the two sets of positions to keep
#' unmatched.
#'
#' @keywords internal
.add_opposing_position_constraint <- function(model, pos1, pos2, players) {

  # NOTE -- the current solution isn't really great anyway because
  # it DEFAULTS to omitting the pos2 mismatch, when in fact,
  # it might make more sense to
  num_players   <- get_model_length(model, 'players')
  player_n      <- get_model_length(model, 'players')

  # Team Vectors
  p1_opponents <- sapply(players, get_opposing_team)
  p2_teams     <- sapply(players, team)
  num_teams   <- length(unique(p2_teams))

  #
  pos1_ind <- which(sapply(players, position) %in% pos1)
  pos2_ind <- which(sapply(players, position) %in% pos2)

  #browser()
  model <- model %>%
    # Opponent Variable Add
    ompr::add_variable(player_opps[i,j], i = 1:player_n, j = 1:num_teams, type = 'integer') %>% # j is the number of unique animals.
    # THIS DOESN'T WORK BECAUSE IT JUST ALIGNS EXACTLY THE SAME AS
    ompr::add_constraint(player_opps[i,j] == players[i] * mask_func(j, p1_opponents), i = 1:player_n, j = 1:num_teams) %>%
    # Position 1 Add
    ompr::add_variable(teams_pos1[i], i = 1:num_teams) %>%
    ompr::add_constraint(teams_pos1[j] == sum_expr(player_teams[i,j], i = pos1_ind), j = 1:num_teams) %>%
    # Position 2 Add
    ompr::add_variable(opps_pos2[i], i = 1:num_teams) %>%
    ompr::add_constraint(opps_pos2[j] == sum_expr(player_opps[i,j], i = pos2_ind), j = 1:num_teams)


  # vector functions
  pos_fn <- function(i, pos) as.numeric(sapply(players, position)[i] %in% pos)
  team_check <- function(i, t, teamvec) {
    as.integer(teamvec[i] == t)
  }

  penalty <- 1000
  for (j in unique(p1_opponents)) {
    model <- model %>%
      ompr::add_constraint(sum_expr(colwise(team_check(i, j, p1_opponents) * pos_fn(i, pos2) * penalty +
                                        team_check(i, j, p2_teams) * pos_fn(i, pos1)) * players[i], i = 1:num_players) <= penalty)
  }

  return(model)
}
