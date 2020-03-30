# This currently builds a base CLASSIC model

#' Build Base Model
#'
#' @param size number of players
#' @param team_vector vector of teams
#' @param pts vector of length 'size' containing points to use in objective function
#' @param maximize Whether to maximize the objective (if FALSE, the objective is minimized)
#'
#' @keywords internal
build_classic_model <- function(size, team_vector, pts, maximize = TRUE) {

  # Lengths (unique teams and positions)
  num_teams <- length(unique(team_vector))
  #
  # Model with all appropriate variables
  base_model <- ompr::MILPModel() %>%
    ompr::add_variable(players[i], i = 1:size, type = "binary") %>%
    # Team related variables
    ompr::add_variable(player_teams[i,j], i = 1:size, j = 1:num_teams, type = 'integer') %>%
    ompr::add_variable(teams[i], i = 1:num_teams, type = 'integer') %>%
    ompr::add_variable(teams_binary[i], i = 1:num_teams, type = 'binary') %>%
    # Teams alignment
    ompr::add_constraint(player_teams[i,j] == players[i] * mask_func(j, team_vector), i = 1:size, j = 1:num_teams) %>%
    ompr::add_constraint(teams[j] == sum_expr(player_teams[i,j], i = 1:size), j = 1:num_teams) %>%
    ompr::add_constraint(teams_binary[j] <= teams[j], j = 1:num_teams) %>%
    ompr::add_constraint(teams[j] <= teams_binary[j] * 100, j = 1:num_teams)

  # Add Objective
  base_model <- add_classic_objective(base_model, maximize = maximize, pts = pts)

  return(base_model)
}


##### CLASSIC objective #####
add_classic_objective  <- function(model, maximize = TRUE, pts) {
  N <- get_model_length(model, 'players')
  objdir <- ifelse(maximize, 'max', 'min')
  model <- ompr::set_objective(model,
                               sum_expr(colwise(pts[i]) * players[i], i = 1:N),
                               sense = objdir)
  return(model)
}


#' Build Single Game / Showdown / Captain model
#'
#' @param size number of players
#' @param team_vector vector of teams
#' @param pts vector of length 'size' containing points to use in objective function
#' @param cpt_mode Logical. Determines whether captain mode is used.
#' @param maximize Whether to maximize the objective (if FALSE, the objective is minimized)
#'
#' @keywords internal
build_singlegame_model <- function(size, team_vector, pts, cpt_mode = TRUE, maximize = TRUE) {
  # Lengths (unique teams and positions)
  num_teams <- length(unique(team_vector))

  # Model with all appropriate variables
  base_model <- ompr::MILPModel() %>%
    ompr::add_variable(players[i], i = 1:size, type = "binary") %>%
    # Team related variables
    ompr::add_variable(player_teams[i,j], i = 1:size, j = 1:num_teams, type = 'integer') %>%
    ompr::add_variable(teams[i], i = 1:num_teams, type = 'integer') %>%
    ompr::add_variable(teams_binary[i], i = 1:num_teams, type = 'binary') %>%
    # Teams alignment
    ompr::add_constraint(player_teams[i,j] == players[i] * mask_func(j, team_vector), i = 1:size, j = 1:num_teams) %>%
    ompr::add_constraint(teams[j] == sum_expr(player_teams[i,j], i = 1:size), j = 1:num_teams) %>%
    ompr::add_constraint(teams_binary[j] <= teams[j], j = 1:num_teams) %>%
    ompr::add_constraint(teams[j] <= teams_binary[j] * 100, j = 1:num_teams)

  if (cpt_mode) {
    # Add caption variable, constrain it to 1, and ensure it's a subset of players
    base_model <- base_model %>%
      # Variables
      ompr::add_variable(captain[i], i = 1:size, type = 'binary') %>%
      ompr::add_variable(capflag[i], i = 1:size, type = 'binary') %>%
      # Constraints
      ompr::add_constraint(captain[i] + players[i] >= capflag[i] * 2, i = 1:size) %>%
      ompr::add_constraint(sum_expr(capflag[i], i = 1:size) == 1)
  }

  # Add Objective
  base_model <- add_singlegame_objective(base_model, maximize = maximize, cpt_mode = cpt_mode, pts = pts)

  return(base_model)
}

add_singlegame_objective  <- function(model, maximize = TRUE, cpt_mode = TRUE, pts) {
  N <- get_model_length(model, 'players')
  objdir <- ifelse(maximize, 'max', 'min')
  if (cpt_mode) {
    model <- ompr::set_objective(model,
                                 sum_expr((colwise(pts[i]) * players[i]) +
                                            (colwise(pts[i]) * .5 * capflag[i]), i = 1:N),
                                 sense = objdir)
  } else {
    model <- ompr::set_objective(model,
                                 sum_expr(colwise(pts[i]) * players[i], i = 1:N),
                                 sense = objdir)
  }
  return(model)
}


##### Base Constraints #####
# Roster Size Constraint
add_roster_size_constraint <- function(model, players, roster_limit) {
  N <- length(players)
  model <- ompr::add_constraint(.model = model,
                                .constraint_expr = sum_expr(players[i], i = 1:N) == roster_limit)
  return(model)
}


# Budget Constraint
add_budget_constraint <- function(model, players, budget, min_budget, cpt_mode = FALSE) {
  N <- length(players)
  player_salaries <- sapply(players, salary)

  ## If CPT MODE, we need to also add in the extra .5 salary
  if (cpt_mode) {
    # Max budget Constraint
    model <- ompr::add_constraint(.model = model,
                                  .constraint_expr = sum_expr(colwise((player_salaries[i]) * players[i]) +
                                                                colwise((player_salaries[i]) * capflag[i] * .5), i = 1:N) <= budget)
    # Min Budget Constraint
    model <- ompr::add_constraint(.model = model,
                                  .constraint_expr = sum_expr(colwise((player_salaries[i]) * players[i]) +
                                                                colwise((player_salaries[i]) * capflag[i] * .5), i = 1:N) >= min_budget)
  } else {
    # Max budget Constraint
    model <- ompr::add_constraint(.model = model,
                                  .constraint_expr = sum_expr(colwise(player_salaries[i]) * players[i], i = 1:N) <= budget)
    # Min Budget Constraint
    model <- ompr::add_constraint(.model = model,
                                  .constraint_expr = sum_expr(colwise(player_salaries[i]) * players[i], i = 1:N) >= min_budget)
  }
  return(model)
}


# Teams Constraints (max players per team, minimum number of teams)
add_team_number_constraints <- function(model, players, min_team_number, max_players_per_team) {
  N <- length(players)
  G <- length(unique(sapply(players, team)))

  # Add constraint
  new_model <- model %>%
    # Minimum Team Number
    ompr::add_constraint(sum_expr(teams_binary[j], j=1:G) >= min_team_number) %>%
    # Maximum Per Team
    ompr::add_constraint(teams[j] <= max_players_per_team, j = 1:G)

  return(new_model)

}


#' Position constraint
#'
#' @param model OMPR Model object
#' @param players List of player objects
#' @param roster_key List containing
#'
#' @keywords internal
add_position_constraint <- function(model, players, roster_key) {

  # Position vector
  position_vector <- sapply(players, position)

  # Parse the roster key
  # This takes UTILS into account
  parsed_roster <- parse_roster_key(roster_key)

  # Get position masks
  pos_masks <- lapply(parsed_roster$pos, function(CP){
    split_pos <- strsplit(x = position_vector, split = '/')
    return(as.numeric(sapply(split_pos, function(z) any(z %in% CP))))
  })
  names(pos_masks) <- parsed_roster$pos

  # Model length
  num_players   <- get_model_length(model, 'players')
  num_positions <- nrow(parsed_roster)

  model <- model %>%
    # Position related variables
    ompr::add_variable(player_positions[i,j], i = 1:num_players, j = 1:num_positions, type = 'integer') %>%
    ompr::add_variable(positions[i], i = 1:num_positions, type = 'integer')

  for (J in 1:num_positions) {
    mask_vec <- pos_masks[[J]]
    curr_min <- parsed_roster[J,]$min
    curr_max <- parsed_roster[J,]$max

    model <- model %>%
      # Position Alignment
      ompr::add_constraint(player_positions[i, J] == players[i] * mask_vec, i = 1:num_players, j = J) %>%
      ompr::add_constraint(positions[J] == sum_expr(player_positions[i,J], i = 1:num_players), j = J) #%>%

    # Just do a min/max check for each position
    model <- model %>%
      ompr::add_constraint(positions[J] >= curr_min) %>%
      ompr::add_constraint(positions[J] <= curr_max)
  }

  return(model)

}


#### These below are internal and
#### Do not need the 'players' object

#' Max Share Across Lineups
#'
#' @param model The model to further constrain
#' @param roster_indx the index of players to constrain
#' @param max_share Number indicating how many players are allowed to be shared across lineups
#'
#' @keywords internal
add_max_share_constraint <- function(model, roster_indx, max_share) {

  model <- model %>%
    ompr::add_constraint(sum_expr(players[i], i = roster_indx) <= max_share)

  return(model)
}


#' Unique Lineup Constraint
#'
#' @param model The model to further constrain
#' @param roster_indx the index of players to constrain
#'
#' @keywords internal
block_one_lineup <- function(model, roster_indx) {
  model <- add_max_share_constraint(model = model,
                                    roster_indx = roster_indx,
                                    max_share = length(roster_indx) - 1)
  return(model)
}


#' Unique Lineup Constraint
#'
#' @param model The model to further constrain
#' @param roster_indx_list the index of players to constrain
#'
#' @keywords internal
add_unique_lineup_constraint <- function(model, roster_indx_list) {

  if (length(roster_indx_list) == 0) {
    return(model)
  } else {
    for (roster_indx in roster_indx_list){
      model <- block_one_lineup(model = model,
                                roster_indx = which(roster_indx == 1))
    }
  }
  return(model)
}


#' Max Overlap constraint
#'
#' @param model The model to further constrain
#' @param roster_indx_list the index of players to constrain
#' @param max_overlap the Maximum player overlap across lineups
#'
#' @keywords internal
add_max_overlap_constraint <- function(model, roster_indx_list, max_overlap) {

  if (length(roster_indx_list) == 0) {
    return(model)
  } else {
    for (roster_indx in roster_indx_list){
      model <- add_max_share_constraint(model, which(roster_indx == 1), max_overlap)
    }
  }
  return(model)
}



#' Unique ID constraint
#'
#' On sites with multi-position eligibility, players will show up once for every
#' position they are eligible. We want to ensure a player is not selected more than
#' once on the same lineup
#'
#' @keywords internal
add_unique_id_constraint <- function(model, players) {
  ids <- sapply(players, id)
  id_cnt     <- table(ids)
  repeat_ids <- names(id_cnt)[id_cnt > 1]

  for (id in repeat_ids) {
    indx <- which(ids == id)
    model <- ompr::add_constraint(model, sum_expr(players[i], i = indx) <= 1)
  }

  return(model)
}
