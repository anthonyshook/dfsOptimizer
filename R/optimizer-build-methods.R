########## Running Models
setGeneric('construct_model', function(object, maximize = TRUE) standardGeneric('construct_model'))
#' Method for constructing the optimization model
#'
#' @param object an S4 object of class Optimizer
#' @param maximize Whether to maximize or minimize the objective function
#'
#' @aliases construct_model
#'
#' @export
setMethod('construct_model',
          signature = 'optimizer',
          definition = function(object, maximize = TRUE) {

            # Get config
            config <- object@config

            # Build base model
            object <- build_base_model(object, maximize = maximize)

            # Updating exposure where it isn't set
            object@players <- lapply(object@players, function(P) {
              # If NA, use global, else, use primary
              if (is.na(max_exposure(P))) {
                P <- set_player_max_exposure(P, max_exposure(config))
              }
              return(P)
            })

            # Updating variance where it isn't set
            object <- apply_global_variance(object, varpct = variance(object@config))

            # Adding roster limit
            object@model <- add_roster_size_constraint(object@model, object@players, roster_limit = roster_size(config))

            # Adding budget constraint
            mlt_mode <- ifelse('multiplier_mode' %in% slotNames(object@config), object@config@multiplier_mode, FALSE)
            object@model <- add_budget_constraint(object@model,
                                                  players = object@players,
                                                  budget = budget(config),
                                                  min_budget = min_budget(config),
                                                  mlt_mode = mlt_mode)

            # Add team size constraints
            object@model <- add_team_number_constraints(model = object@model,
                                                        players = object@players,
                                                        min_team_number = min_team_req(config),
                                                        max_players_per_team = max_players_per_team(config))

            # Add positional constraint
            object@model <- add_position_constraint(model = object@model,
                                                    players = object@players,
                                                    roster_key = roster_key(config))

            # Add unique ID constraint
            object@model <- add_unique_id_constraint(model = object@model,
                                                     players = object@players)

            # Add additional constraints that are specific to the current model
            object <- add_additional_constraints(object)

            return(object)

          })


setGeneric('build_lineups', function(object, num_lineups = 1, solver = 'glpk', maximize = TRUE, verbose = TRUE) standardGeneric('build_lineups'))
#' Function to Generate lineups
#'
#' @param object an S4 object of class Optimizer
#' @param num_lineups Number of lineups to generate
#' @param solver The solver method (defaults to 'glpk').
#' @param maximize Whether the model is intended to maximize (the default) or minimize the objective function
#' @param verbose Whether to show a progress bar when building models. Defaults to TRUE.
#'
#' @aliases build_lineups
#'
#' @export
setMethod('build_lineups',
          signature = 'optimizer',
          definition = function(object, num_lineups = 1, solver = 'glpk', maximize = TRUE, verbose = TRUE) {

            # Construct Model
            # Necessary to do this now so we do just-in-time construction
            M <- construct_model(object, maximize = maximize)

            # Build a player data set
            # We can then filter from this below, where we need the relevant rows the optimizer solved for
            solution_vectors <- list()
            lineups <- new_lineup_object(object, num_lineups = num_lineups)

            # Block Players
            M@model <- add_block_constraint(M@model,
                                            players = M@players)

            # Lock Players
            M@model <- add_lock_constraint(M@model,
                                           players = M@players)

            # Generate Lineups
            if (verbose) pb <- utils::txtProgressBar(min = 0, max = num_lineups, initial = 0, char = '#', style = 3)
            for (i in 1:num_lineups) {
              if (verbose) utils::setTxtProgressBar(pb, i)

              # Reset the variance of the model
              current_opt <- apply_variance(M)
              current_opt <- update_objective(current_opt, fpts = extract_player_fpts(current_opt))

              # Temporary Model
              current_model <- current_opt@model

              # Add unique roster constraint
              current_model <- add_unique_lineup_constraint(current_model, solution_vectors)

              # Add max overlap constraint (Note: Could be rolled into the unique roster constraint)
              current_model <- add_max_overlap_constraint(current_model, solution_vectors, max_overlap(M@config))

              # If any player is currently above their exposure rate, block them
              # But only check IF the lowest possible value of exposure is less than the max_exposure rate
              if ( 1/(length(solution_vectors) + 1) < max_exposure(M@config)) {
                current_exposures <- calculate_exposure(solution_vectors)
                over_exposed <- which(current_exposures > sapply(M@players, max_exposure))

                # Ignore Locked and blocked
                over_exposed <- setdiff(over_exposed,
                                        c(which(sapply(M@players, locked) == 1),
                                          which(sapply(M@players, blocked) == 1)))

                # Add exposure constraint
                if (length(over_exposed) > 0) {
                  current_model <- current_model %>%
                    ompr::add_constraint(players[i] == 0, i = over_exposed)
                }
              }

              # Solve the model
              fit_model <- ompr::solve_model(current_model,
                                             solver = ompr.roi::with_ROI(solver))

              # Break if not optimal
              if (fit_model$status != 'optimal') {
                stop('Model could not reach a solution.')
              }

              # Get solution index
              solution_index <- ompr::get_solution(fit_model, players[i])

              # Add to existing rosters
              solution_vectors[[i]] <- solution_index$value

              # TO DO -- get only relevant rows (not the index, but the table containing players' data)
              # Returns the *original* FPTS, not those influenced by variance
              cols <- c('id','fullname','team','position','salary','fpts')
              crlineup <- get_player_data(object)[which(solution_vectors[[i]]==1), ..cols]
              crlineup <- format_lineup(object, crlineup, fit_model = fit_model)
              lineups@lineups[[i]] <- crlineup

            }

            return(lineups)

          })

##### Methods for Building Models #####
setGeneric('build_base_model', function(object, maximize=TRUE) standardGeneric('build_base_model'))
setMethod('build_base_model', 'ClassicOptim',
          function(object, maximize=TRUE) {
            # Checking for players
            if (length(object@players) == 0) {
              stop('No players found, cannot construct a model!')
            }

            # Start constructing the model
            object@model <- build_classic_model(
              size = length(object@players),
              team_vector = sapply(object@players, team),
              pts  = extract_player_fpts(object),
              maximize = maximize
            )

            return(object)
          })

setMethod('build_base_model', 'SingleGameOptim',
          function(object, maximize=TRUE) {
            # Checking for players
            if (length(object@players) == 0) {
              stop('No players found, cannot construct a model!')
            }

            # Start constructing the model
            object@model <- build_singlegame_model(
              size = length(object@players),
              team_vector = sapply(object@players, team),
              position_vector = sapply(object@players, position),
              pts  = extract_player_fpts(object),
              config = object@config,
              maximize = maximize
            )

            return(object)

          })


# Updates objective based on necessary inputs
# Since it's internal, it can remain generic
setGeneric('update_objective', function(object, ...) standardGeneric('update_objective'))
setMethod('update_objective', 'ClassicOptim',
          function(object, ...){

            # Check for req values
            req_vals  <- c('fpts')
            sub_vals  <- list(...)
            val_check <- req_vals %in% names(sub_vals)

            if (!all(val_check)) {
              stop(paste0('Missing required argument(s): ', paste(req_vals[!val_check], collapse = ', ')))
            }

            # Update the model
            object@model <- add_classic_objective(object@model, pts = sub_vals$fpts)

            return(object)
          })

setMethod('update_objective', 'SingleGameOptim',
          function(object, ...){

            # Check for req values
            req_vals  <- c('fpts')
            sub_vals  <- list(...)
            val_check <- req_vals %in% names(sub_vals)

            if (!all(val_check)) {
              stop(paste0('Missing required argument(s): ', paste(req_vals[!val_check], collapse = ', ')))
            }

            # Update the model
            object@model <- add_singlegame_objective(object@model, pts = sub_vals$fpts, mlt_mode = object@config@multiplier_mode)

            return(object)
          })
