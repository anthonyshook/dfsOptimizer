# CONSTRAINTS DEFINED HERE SHOULD TAKE THE OPTIMIZER OBJECT, NOT THE OMPR MODEL ITSELF.
#   LOCK AND BLOCK BREAK THIS RULE, BUT THEY CAN BE FIXED LATER


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
#' @param optObj Optimizer object
#' @param pos1 Positions for set one
#' @param pos2 Positions for set two
#'
#' @details The parameters \code{pos1} and \code{pos2} refer to the two sets of positions to keep
#' unmatched.
#'
#' @keywords internal
.restrict_opposing_position <- function(optObj, pos1, pos2) {

  # Get players from the model
  players <- optObj@players

  # Pull out model
  model <- optObj@model@mod

  # Model Data
  num_players   <- get_model_length(model, 'players')

  # Team Vectors
  p1_opponents <- sapply(players, get_opposing_team)
  p2_teams     <- sapply(players, team)
  num_teams   <- length(unique(p2_teams))

  # vector functions
  pos_fn <- function(i, pos) as.numeric(make_position_indicator(sapply(players,position)[i], pos, which_or_ind = 'ind'))
  # pos_fn <- function(i, pos) as.numeric(sapply(players, position)[i] %in% pos)

  team_check <- function(i, t, teamvec) {
    as.integer(teamvec[i] == t)
  }

  # browser()
  penalty <- 1000
  for (j in unique(p1_opponents)) {
    model <- model %>%
      ompr::add_constraint(sum_expr(colwise(team_check(i, j, p1_opponents) * pos_fn(i, pos2) * penalty +
                                              team_check(i, j, p2_teams) * pos_fn(i, pos1)) * players[i], i = 1:num_players) <= penalty)
  }

  optObj@model@mod <- model

  return(optObj)
}

#' Adds Same-Team stacks
#'
#' @param optObj Optimizer object
#' @param positions Positions for that should be stacked within a single team
#' @param nstacks Number of stacks to try to include (Default is 1)
#'
#' @keywords internal
.add_team_stack <- function(optObj, positions, nstacks = 1) {

  # Get players from the model
  players <- optObj@players

  # Pull model
  model <- optObj@model@mod

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
  # pos_fn <- function(i, pos) as.numeric(sapply(players, position)[i] %in% pos)
  pos_fn <- function(i, pos) as.numeric(make_position_indicator(sapply(players,position)[i], pos, which_or_ind = 'ind'))

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
      poscount <- sum(positions[posnum] == positions)

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

  # Put the model back and return the updated object
  optObj@model@mod <- model
  return(optObj)
}




# THIS WAS A TEST THAT ONLY KIND OF WORKED
# team_stack_2 <- function(optObj, positions) {
#
#   # This is just my attempt to test a quick thing
#   lineup_groups <- link_players_on_same_team(optObj@players, positions = positions)
#
#   model <- optObj@model@mod
#
#   tmpfn <- function(indx) {
#     tmp <- rep(0, times = 309)
#     tmp[as.numeric(lineup_groups[indx,])] <- 1
#     return(tmp)
#   }
#
#   # indvector <- lapply(1:nrow(lineup_groups), function(a) tmpfn(1:309, indx=a))
#   # npossibile <- length(TST)
#   indeces <- lapply(1:nrow(lineup_groups), function(a) as.numeric(lineup_groups[a,]))
#   indfun <- function(k) unlist(lineup_groups)[k]
#
#   # browser()
#   print(nrow(lineup_groups))
#   model <- model %>%
#     ######## sum for every grouping
#     # ompr::add_variable(tstgroups[i], i = 1:nrow(lineup_groups), type = 'integer') %>%
#     # ompr::add_constraint(
#     #   sum_expr(players[i]) == tstgroups[j], i = 1:309, j = 1:nrow(lineup_groups)) %>%
#     ######## one sum
#     ompr::add_variable(tst, type = 'integer') %>%
#     ompr::add_constraint(tst == sum_expr(
#       players[i], i = unlist(indeces)
#     ))
#
#   # Loop test...
#   # browser()
#   # for (i in 1:length(indeces)) {
#   #   cind <- tmpfn(i)
#   #   model <- model %>%
#   #     ompr::add_constraint(sum_expr(players[j] * colwise(cind), j = 1:309) == tstgroups[i])
#   # }
#
#   optObj@model@mod <- model
#
#   return(optObj)
#
# }
#
