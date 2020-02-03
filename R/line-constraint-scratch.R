# ### Line-constraint (NHL) scratch
#
# ## below is code that could be adjusted to define lines, then ensure at least one is present
#
# # TODO:
# # Add both 'full' and 'partial' values?
# #
# .add_same_line_constraint <- function(model, positions, players) {
#
#   # Some info about the model
#   num_players   <- get_model_length(model, 'players')
#   num_teams     <- get_model_length(model, 'teams')
#   num_positions <- length(positions)
#
#   # NEEDS
#   # the positions, and the number of players
#   # the teams (probably need to add one set of constraints PER team.)
#
#   # Replace this with a way of getting the total combination
#   # OK, once -this- is done, it should work.
#   player_sets <- link_players_on_same_team(players, positions)
#
#   # Add Variables
#   model <- model %>%
#     ompr::add_variable(stack_count[i], i = 1:nrow(player_sets), type = 'integer') %>% # This doesn't actually work.
#     ompr::add_variable(stack_flag[i], i = 1:nrow(player_sets), type = 'binary')
#
#   # Loop and add combination constraints
#   # OOF at scale this can take 30 full seconds... (with 3 positions included)
#   stackfun <- function(i, j) {
#     browser()
#     init <- rep(0, times = length(i))
#     init[as.numeric(player_sets[i,])] <- 1
#     return(init)
#   }
#
#   # ####
#   for (combination in 1:nrow(player_sets)){
#     currIndex <- as.integer(player_sets[combination, ])
#     model <- model %>%
#       ompr::add_constraint(stack_count[i] == sum_expr(players[j], j = currIndex), i = combination)
#   }
#
#   # Add one more constraint to ensure AT LEAST one of those values meets the criteria.
#   model <- model %>%
#     # The following constraint limits the function to require a 0 when stack count is anything
#     # less than the number of expected positions
#     ompr::add_constraint(stack_count[j] >= num_positions * stack_flag[j], j = 1:nrow(player_sets)) %>%
#     # In combination with above, this constraint requires that at least one flag value is 1
#     # The only way that can be true is if at least 1 stack_count == num_positions.
#     ompr::add_constraint(sum_expr(stack_flag[j], j = 1:nrow(player_sets)) >= 1)
#
#   return(model)
# }
