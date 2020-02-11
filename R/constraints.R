#' @include class-player.R

#' @title Block Players Constraint
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
#'
#' @details The parameters \code{pos1} and \code{pos2} refer to the two sets of positions to keep
#' unmatched.
#'
#' @keywords internal
.restrict_opposing_position <- function(model, pos1, pos2) {

  # Get players from the model
  players <- model@players

  # Model Data
  num_players   <- get_model_length(model, 'players')

  # Team Vectors
  p1_opponents <- sapply(players, get_opposing_team)
  p2_teams     <- sapply(players, team)
  num_teams   <- length(unique(p2_teams))

  #
  pos1_ind <- which(sapply(players, position) %in% pos1)
  pos2_ind <- which(sapply(players, position) %in% pos2)

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

#' Adds Same-Team stacks
#'
#' @param model Model object
#' @param positions Positions for that should be stacked within a single team
#'
#' @keywords internal
.add_team_stack <- function(model, positions) {

  # Get players from the model
  players <- model@players

  # Some info about the model
  num_players   <- get_model_length(model, 'players')
  num_teams     <- get_model_length(model, 'teams')
  teamvec       <- sapply(players, team)
  curr_teams    <- unique(teamvec)
  num_positions <- length(positions)

  # Add a team vector
  model <- model %>%
    ompr::add_variable(pos_team_stack[i,j], i = 1:num_teams, j = 1:length(positions), type = 'binary') %>%
    ompr::add_variable(team_stack[i], i = 1:num_teams, type = 'binary')

  # Make Positional constraint function
  pos_fn <- function(i, pos) as.numeric(sapply(players, position)[i] %in% pos)
  tpc_fun <- function(i, t, p, teamvec) {
    # Gets this is a mask that is the position and team
    mask <- as.integer(teamvec[i] == t) * pos_fn(i, positions[p])
    return(mask)
  }

  # This loop goes through each team and position and adds constraints
  # to flag, as a binary, the positions-by-team that have at least N
  # amount of players (where N is defined by the number of times that
  # position is repeated -- e.g., if positions = c('W','W','C'), then
  # N('W') = 2, but N('C') = 1)
  for (TMS in curr_teams) {
    for (POS in 1:length(positions)){
      posnum <- POS
      tmsnum <- which(curr_teams == TMS)
      poscount <- sum(positions[posnum] == positions)

        model <- model %>%
          ompr::add_constraint((poscount - (1 - pos_team_stack[i, j=posnum])) +
                                 (pos_team_stack[i, j=posnum] * 200) >=
                                 sum_expr(players[k] *
                                            colwise(tpc_fun(k, TMS, p=POS, teamvec)),
                                          k = 1:num_players), i = tmsnum) %>%
          ompr::add_constraint(pos_team_stack[i, j=posnum] * poscount <=
                                 sum_expr(players[k] *
                                            colwise(tpc_fun(k, TMS, p=POS, teamvec)),
                                          k = 1:num_players), i = tmsnum)
      # }
    }
  }

  # Last condition says count the values where the team_stack > num_positions and cast to boolean,
  # Then constrain the model to ensure at least one of those values == 1
  model <- model %>%
    ompr::add_constraint(sum_expr(pos_team_stack[i, j], j = 1:num_positions) >= team_stack[i] * num_positions, i = 1:num_teams) %>%
    ompr::add_constraint(sum_expr(team_stack[i], i = 1:num_teams) >= 1)

  return(model)
}
