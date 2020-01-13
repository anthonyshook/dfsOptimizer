# Base model
#library(ROI.plugin.glpk)
# n = 10
# l = 2
# v = runif(n, 0, 1)
# base_model <- ompr::MILPModel() %>%
#   ompr::add_variable(players[i], i = 1:n, type = "binary") %>%
#   ompr::set_objective(sum_expr(colwise(v[i]) * players[i], i = 1:n), 'max') %>%
# #  ompr::set_objective(ompr::sum_expr(colwise(v[i]) * players[i], i = 1:n)) %>%
#   ompr::add_constraint(sum_expr(players[i], i = 1:n) == l) %>%
#   ompr::solve_model(ompr.roi::with_ROI(solver = "glpk")) %>%
#   ompr::get_solution(players[i])
#
#
# max_capacity <- 5
# #n <- 10
# weights <- runif(n, max = max_capacity)
# MIPModel() %>%
#   add_variable(x[i], i = 1:n, type = "binary") %>%
#   set_objective(sum_expr(weights[i] * x[i], i = 1:n), "max") %>%
#   add_constraint(sum_expr(weights[i] * x[i], i = 1:n) <= max_capacity) %>%
#   solve_model(with_ROI(solver = "glpk")) %>%
#   get_solution(x[i])
#

#' Build Base Model
#'
#' @param size number of units
#' @param team_vector vector of teams
#' @param position_vector Vector of positions to consider
#' @param pts vector of length 'size' containing points to use in objective function
#'
#' @export
build_base_model <- function(size, team_vector, position_vector, pts) {

  # Lengths (unique teams and positions)
  num_teams <- length(unique(team_vector))
  num_positions <- length(unique(position_vector))

  # Model with all appropriate variables
  base_model <- ompr::MILPModel() %>%
    ompr::add_variable(players[i], i = 1:size, type = "binary") %>%
    # Team related variables
    ompr::add_variable(player_teams[i,j], i = 1:size, j = 1:num_teams, type = 'integer') %>% # j is the number of unique animals.
    ompr::add_variable(teams[i], i = 1:num_teams, type = 'integer') %>%
    ompr::add_variable(teams_binary[i], i = 1:num_teams, type = 'binary') %>%
    # Position related variables
    ompr::add_variable(player_positions[i,j], i = 1:size, j = 1:num_positions, type = 'integer') %>%
    ompr::add_variable(positions[i], i = 1:num_positions, type = 'integer') %>%
    ## Basic alignment of variables (making teams == player_teams, etc.) via constraint
    # Teams alignment
    ompr::add_constraint(player_teams[i,j] == players[i] * mask_func(j, team_vector), i = 1:size, j = 1:num_teams) %>%
    ompr::add_constraint(teams[j] == sum_expr(player_teams[i,j], i = 1:size), j = 1:num_teams) %>%
    ompr::add_constraint(teams_binary[j] <= teams[j], j = 1:num_teams) %>%
    ompr::add_constraint(teams[j] <= teams_binary[j] * 10000, j = 1:num_teams) %>%
    # Positions alignment
    ompr::add_constraint(player_positions[i, j] == players[i] * mask_func(j, position_vector), i = 1:size, j = 1:num_positions) %>%
    ompr::add_constraint(positions[j] == sum_expr(player_positions[i,j], i = 1:size), j = 1:num_positions)


  # Here we can also add constraints
  base_model <- add_objective(base_model, maximize = TRUE, pts = pts)

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


# Other constraints
