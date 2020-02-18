
#' Build Base Model
#'
#' @param size number of units
#' @param team_vector vector of teams
#' @param pts vector of length 'size' containing points to use in objective function
#' @param maximize Whether to maximize the objective (if FALSE, the objective is minimized)
#'
build_base_model <- function(size, team_vector, pts, maximize = TRUE) {

  # Lengths (unique teams and positions)
  num_teams <- length(unique(team_vector))
#
  # Model with all appropriate variables
  base_model <- ompr::MILPModel() %>%
    ompr::add_variable(players[i], i = 1:size, type = "binary") %>%
    # Team related variables
    ompr::add_variable(player_teams[i,j], i = 1:size, j = 1:num_teams, type = 'integer') %>% # j is the number of unique animals.
    ompr::add_variable(teams[i], i = 1:num_teams, type = 'integer') %>%
    ompr::add_variable(teams_binary[i], i = 1:num_teams, type = 'binary') %>%
    # Teams alignment
    ompr::add_constraint(player_teams[i,j] == players[i] * mask_func(j, team_vector), i = 1:size, j = 1:num_teams) %>%
    ompr::add_constraint(teams[j] == sum_expr(player_teams[i,j], i = 1:size), j = 1:num_teams) %>%
    ompr::add_constraint(teams_binary[j] <= teams[j], j = 1:num_teams) %>%
    ompr::add_constraint(teams[j] <= teams_binary[j] * 100, j = 1:num_teams)

  # Add Objective
  base_model <- add_objective(base_model, maximize = maximize, pts = pts)

  return(base_model)
}


##### Base objective #####
add_objective  <- function(model, maximize = TRUE, pts) {
  N <- get_model_length(model, 'players')
  objdir <- ifelse(maximize, 'max', 'min')
  model <- ompr::set_objective(model,
                               sum_expr(colwise(pts[i]) * players[i], i = 1:N),
                               sense = objdir)
  return(model)
}

##### Base Constraints #####
# Roster Size Constraint
add_roster_size_constraint <- function(model, roster_limit) {
  N <- get_model_length(model, 'players')
  model <- ompr::add_constraint(.model = model,
                                .constraint_expr = sum_expr(players[i], i = 1:N) == roster_limit)
  return(model)
}

# Budget Constraint
add_budget_constraint <- function(model, player_salaries, budget) {
  N <- get_model_length(model, 'players')
  model <- ompr::add_constraint(.model = model,
                                .constraint_expr = sum_expr(colwise(player_salaries[i]) * players[i], i = 1:N) <= budget)
  return(model)
}

# Teams Constraints (max players per team, minimum number of teams)
add_team_number_constraints <- function(model, min_team_number, max_players_per_team) {
  N <- get_model_length(model, 'players')
  G <- get_model_length(model, 'teams')

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
#' @param position_vector Vector of positions to consider
#' @param roster_key List containing
#' @param flex_positions Positions to be considered for flex
#'
#' @keywords internal
add_position_constraint <- function(model, position_vector, roster_key, flex_positions) {

  pos_masks <- lapply(roster_key, function(V){
    current_positions <- V$positions
    return(as.numeric(position_vector %in% current_positions))
  })

  # Model length
  num_players   <- get_model_length(model, 'players')
  num_positions <- length(roster_key)

  model <- model %>%
    # Position related variables
    ompr::add_variable(player_positions[i,j], i = 1:num_players, j = 1:num_positions, type = 'integer') %>%
    ompr::add_variable(positions[i], i = 1:num_positions, type = 'integer')

  for (J in 1:num_positions) {
    mask_vec   <- pos_masks[[J]]
    curr_limit <- roster_key[[J]]$num
    model <- model %>%
      # Position Alignment
      ompr::add_constraint(player_positions[i, J] == players[i] * mask_vec, i = 1:num_players, j = J) %>%
      ompr::add_constraint(positions[J] == sum_expr(player_positions[i,J], i = 1:num_players), j = J) #%>%

    # TODO: This is where we'd want to update the process
    # Instead of using just flex positions, we COULD use the key to reason about mins/maxs for each position type.
    if (all(roster_key[[J]]$positions %in% flex_positions)) {
      model <- model %>%
        ompr::add_constraint(positions[J] >= curr_limit) %>%
        ompr::add_constraint(positions[J] <= curr_limit+1)
    } else {
      model <- model %>%
        ompr::add_constraint(positions[J] == curr_limit)
    }
  }

  return(model)

}

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
#' @param roster_indx the index of players to constrain
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


#' Unique ID constraint
#'
#' On sites with multi-position eligibility, players will show up once for every
#' position they are eligible. We want to ensure a player is not selected more than
#' once on the same lineup
#' @keywords internal
add_unique_id_constraint <- function(model, ids) {
  id_cnt     <- table(ids)
  repeat_ids <- names(id_cnt)[id_cnt > 1]

  for (id in repeat_ids) {
    indx <- which(ids == id)
    model <- ompr::add_constraint(model, sum_expr(players[i], i = indx) <= 1)
  }

  return(model)
}
