# ALL CONSTRAINTS SHOULD TAKE
# 1 - the OMPR Model object
# 2 - the list of player objects
# 3 - Any other relevant arguments.
# ---------------------------------------
# Internal Functions for constraint logic
# ---------------------------------------

#' @include class-player.R

#' @title Block Players Constraint
#'
#' @param model Model object
#' @param players List of player objects
#'
#' @keywords internal
add_block_constraint <- function(model, players) {
  blocks <- which(sapply(players, blocked) == 1)

  if (length(blocks) == 0) {
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
#' @param players List of player objects
#'
#' @keywords internal
add_lock_constraint <- function(model, players) {
  locks <- which(sapply(players, locked) == 1)

  if (length(locks) == 0) {
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
#' @param players List of player objects
#' @param pos1 Positions for set one
#' @param pos2 Positions for set two
#'
#' @details The parameters \code{pos1} and \code{pos2} refer to the two sets of positions to keep
#' unmatched.
#'
#' @keywords internal
constr_restrict_opposing_position <- function(model, players, pos1, pos2) {

  # Model Data
  num_players   <- length(players)

  # Team Vectors
  p1_opponents <- sapply(players, get_opposing_team)
  p2_teams     <- sapply(players, team)
  num_teams   <- length(unique(p2_teams))

  # vector functions
  pos_fn <- function(i, pos) as.numeric(make_position_indicator(sapply(players,position)[i], pos, which_or_ind = 'ind'))

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
#' @param players List of player objects
#' @param positions Positions for that should be stacked within a single team
#' @param opt_positions Optional positions -- used to add OR-level positions (e.g., stack of QB, WR & (TE | RB))
#' @param nstacks Number of stacks to try to include (Default is 1)
#'
#' @keywords internal
constr_team_stack <- function(model, players, positions, opt_positions = NULL, nstacks = 1) {

  # Convert positions to a list
  positions <- as.list(positions)

  if (!is.null(opt_positions)) {
    # For now, do nothing...
    # positions <- c(positions, list(opt_positions))
    warning('opt_positions not yet functional')
  }

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

  # Make Positional constraint functions
  pos_fn <- function(i, pos) {
    as.numeric(make_position_indicator(sapply(players,position)[i], pos, which_or_ind = 'ind'))
  }

  tpc_fun <- function(i, t, p, teamvec) {
    # Gets this is a mask that is the position and team
    cpos <- unique(unlist(positions[p]))
    mask <- as.integer(teamvec[i] == t) * pos_fn(i, cpos)
    return(mask)
  }

  # This loop goes through each team and position and adds constraints
  # to flag, as a binary, the positions-by-team that have at least N
  # amount of players (where N is defined by the number of times that
  # position is repeated -- e.g., if positions = c('W','W','C'), then
  # N('W') = 2, but N('C') = 1)
  for (TMS in curr_teams) {
    tmsnum <- which(curr_teams == TMS)
    pos_vector <- seq_along(positions)
    # This constraint is meant to require the minimum number of players be represented
    # in the case of sports with multiple positions. Effectively, we want to say that in order
    # for the team_flag to be 1, we also need a minimum player count of num_positions
    model <- model %>%
      ompr::add_constraint(team_stack[i] * num_positions <= sum_expr(
        players[k] * colwise(tpc_fun(k, TMS, p=pos_vector, teamvec)), k = 1:num_players
      ), i = tmsnum)

    for (posnum in 1:length(positions)){
      # Get number of times this position repeats
      poscount <- sum(sapply(positions, identical, positions[[posnum]]))
      print(poscount)

      model <- model %>%
        ompr::add_constraint((poscount - (1 - pos_team_stack[i, j=posnum])) +
                               (pos_team_stack[i, j=posnum] * 200) >=
                               sum_expr(players[k] *
                                          colwise(tpc_fun(k, TMS, p=posnum, teamvec)),
                                        k = 1:num_players), i = tmsnum) %>%
        ompr::add_constraint(pos_team_stack[i, j=posnum] * poscount <=
                               sum_expr(players[k] *
                                          colwise(tpc_fun(k, TMS, p=posnum, teamvec)),
                                        k = 1:num_players), i = tmsnum)
    }
  }

  # This condition says count the values where the team_stack > num_positions and cast to boolean,
  # Then constrain the model to ensure at least one of those values == 1
  model <- model %>%
    ompr::add_constraint(sum_expr(pos_team_stack[i, j], j = 1:num_positions) >= team_stack[i] * num_positions, i = 1:num_teams) %>%
    ompr::add_constraint(sum_expr(team_stack[i], i = 1:num_teams) >= nstacks)

  return(model)
}


#' Force Opposing Position constraint
#'
#' @param model Model object
#' @param players List of player objects
#' @param pos1 Position for team
#' @param pos2 Position for opposing team
#'
#' @details Accepts a single value for pos1 and pos2
#'
#' @keywords internal
constr_force_opposing <- function(model, players, pos1, pos2) {
  # Assertions (for now)
  if (length(pos1) > 1 | length(pos2) > 1) stop("Both pos1 and pos2 must be of length 1")
  combpos <- c(pos1, pos2)

  # Data from players
  teams <- sapply(players, team)
  opps  <- sapply(players, get_opposing_team)
  posit <- sapply(players, position)
  team_opponent_df <- unique(data.frame(team = teams, opp = opps, stringsAsFactors = FALSE))

  # Count Data
  nump  <- length(players)
  numpos <- 2 # Currently hardcoded.
  numtms <- data.table::uniqueN(teams)

  # Add team and opponent variables
  model <- model %>%
    ompr::add_variable(force_team[i, j], i = 1:numtms, j = 1:numpos, type = 'binary', lb = 0) %>%
#    ompr::add_variable(force_opp[i], i = 1:numtms, type = 'binary', lb = 0) %>%
    ompr::add_variable(force_flag[i], i = 1:numtms, type = 'integer')

  # Add Functions
  pos_fn <- function(i, pos) as.numeric(make_position_indicator(posit[i], pos, which_or_ind = 'ind'))

  fo_fun <- function(i, t, p, teamvec=teams) {
    # Gets this is a mask that is the position and team
    mask <- as.integer(teamvec[i] == t) * pos_fn(i, p)
    return(mask)
  }

  # Set variables to the count of each item
  for (i in 1:nrow(team_opponent_df)) {
    # Current team/opponent pair
    cpair <- team_opponent_df[i,]

    # option: count all the possible force_team gets pos1, force_opps gets pos2
    # Those are the sum of that team/position, but constrained to 1 if greater than 0
    # Flag is the sum of those two, constrained to 1 if equal to 2
    # Final constraint is that sum(force_flag) >= 1
    # NOTE -- this simple version will meet the constraint if and when ANY pos1 or
    # pos2 is inclued (i.e., there is no way to say "constraint tm1 QB with tm2 WR *AND* TE)
    for (P in 1:numpos) {
      poscount <- 1

      # Add constraints
      model <- model %>%
        ompr::add_constraint((poscount - (1 - force_team[i, j=P])) +
                               (force_team[i, j=P] * 200) >=
                               sum_expr(players[k] *
                                          colwise(fo_fun(k, as.character(cpair)[P], p=combpos[P])),
                                        k = 1:nump), i = i) %>%
        ompr::add_constraint(force_team[i, j=P] * poscount <=
                               sum_expr(players[k] *
                                          colwise(fo_fun(k, as.character(cpair[P]), p=combpos[P])),
                                        k = 1:nump), i = i)
    }

  }

  # Constrain so that at least one pair must be one
  model <- model %>%
    ompr::add_constraint(sum_expr(force_team[i, j], j = 1:numpos) >= force_flag[i] * numpos, i = 1:numtms) %>%
    ompr::add_constraint(sum_expr(force_flag[i], i = 1:numtms) >= 1)


  return(model)
}
