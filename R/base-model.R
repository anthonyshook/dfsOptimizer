# Base model
#library(ROI.plugin.glpk)
# n = 10
# l = 2
# v = runif(n, 0, 1)
# base_model <- ompr::MILPModel() %>%
#   ompr::add_variable(include_flag[i], i = 1:n, type = "binary") %>%
#   ompr::set_objective(sum_expr(colwise(v[i]) * include_flag[i], i = 1:n), 'max') %>%
# #  ompr::set_objective(ompr::sum_expr(colwise(v[i]) * include_flag[i], i = 1:n)) %>%
#   ompr::add_constraint(sum_expr(include_flag[i], i = 1:n) == l) %>%
#   ompr::solve_model(ompr.roi::with_ROI(solver = "glpk")) %>%
#   ompr::get_solution(include_flag[i])
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
#' @param pts vector of length 'size' containing points to use in objective function
#'
#' @export
build_base_model <- function(size, pts) {
  base_model <- ompr::MILPModel() %>%
    ompr::add_variable(include_flag[i], i = 1:size, type = "binary")

  base_model <- add_objective(base_model, maximize = TRUE, pts = pts)
#    ompr::add_constraint(sum_expr(include_flag[i], i = 1:n) == l)

  return(base_model)
}


##### Base objective #####
add_objective  <- function(model, maximize = TRUE, pts) {
  N <- get_model_length(model, 'include_flag')
  objdir <- ifelse(maximize, 'max', 'min')
  model <- ompr::set_objective(model,
                               sum_expr(colwise(pts[i]) * include_flag[i], i = 1:N),
                               sense = objdir)
  return(model)
}

##### Base Constraints #####
# Roster Size Constraint
add_roster_size_constraint <- function(model, roster_limit) {
  N <- get_model_length(model, 'include_flag')
  model <- ompr::add_constraint(.model = model,
                                .constraint_expr = sum_expr(include_flag[i], i = 1:N) == roster_limit)
  return(model)
}

# Budget Constraint
add_budget_constraint <- function(model, player_salaries, budget) {
  N <- get_model_length(model, 'include_flag')
  model <- ompr::add_constraint(.model = model,
                                .constraint_expr = sum_expr(colwise(player_salaries[i]) * include_flag[i], i = 1:N) <= budget)
  return(model)
}


