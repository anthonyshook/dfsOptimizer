#' S4 Class For Optimizer
#'
#' @slot site The site being used for optimization
#' @slot sport The sport being optimized
#' @slot contest_type The type of contest; determines base constraints (e.g., Classic, Showdown/Single-Game)
#' @slot players List of players to build lineups from
#' @slot model The optimization model
#' @slot maximize Logical, whether to maximize or minimize the objective function
#'
.optimizer <- setClass('optimizer',
                       slots = list(
                         site = 'character',
                         sport = 'character',
                         contest_type = 'character',
                         players = 'list',
                         model = 'optim_model',
                         maximize = 'logical'
                       ),
                       prototype = list(
                         maximize = TRUE
                       ))

#' Initialization Function For Optimizer Class
#'
#' @param site The site being used for optimization
#' @param sport The sport being optimized
#' @param contest_type The type of contest; determines base constraints (e.g., Classic, Showdown/Single-Game). Default: CLASSIC
#' @param players List of players to build lineups from (defaults to empty list)
#' @param maximize Logical, whether to maximize or minimize the objective function (Defaults to TRUE)
#'
#' @export
optimizer <- function(site,
                      sport,
                      contest_type = 'CLASSIC',
                      players = list(),
                      maximize = TRUE) {

  site         <- toupper(site)
  sport        <- toupper(sport)
  contest_type <- toupper(contest_type)

  # Get base config
  cfg <- base_settings[[site]][[sport]][[contest_type]]

  if (length(cfg) == 0 || is.null(cfg)) {
    stop('Configuration for Site + Sport + Contest Type not implemented!')
  }

  # Making model with flex position
  model <- optim_model(flex_positions = cfg$flex_positions)

  o <- .optimizer(site = site,
                  sport = sport,
                  contest_type = contest_type,
                  players = players,
                  model = model,
                  maximize = maximize)
  return(o)
}

# Base Methods
setMethod('show', 'optimizer', function(object) {
  cat(
    paste0('Optimizer Object (S4 class)\n',
           'Site: ', object@site, '\n',
           'Sport: ', object@sport, '\n',
           'Contest Type: ', object@contest_type, '\n',
           'Number of Players: ', length(object@players)
    )
  )
})


#### Methods
## Extraction
setGeneric("extract_player_fpts", function(object) standardGeneric("extract_player_fpts"))
#' Extracting Fantasy Points from Player objects
#'
#' @param object an optimizer object
#'
setMethod('extract_player_fpts',
          signature = 'optimizer',
          definition = function(object) {
            if (length(object@players) == 0) {
              message('No Players in optimizer!')
              out <- NULL
            } else {
              out <- sapply(object@players, fpts)
            }
            return(out)
          })


setMethod('get_player_data', 'optimizer',
          function(object){

            players <- lapply(object@players, get_player_data)
#            all     <- do.call('rbind', players)
            return(data.table::rbindlist(players))

          })

setGeneric('get_player_id', function(object, name, team, position) standardGeneric('get_player_id'))
#' Get a Player Id
#'
#' @param object Object of class Optimizer
#' @param name Full name of player
#' @param team team abbreviation of player (Not required)
#' @param position position of player (Not required)
#'
#' @details \code{team} and \code{position} can be included to differentiate between two players with the same name, but who play for different teams and/or at different positions.
#'
#' @export
setMethod(f = 'get_player_id',
          signature = 'optimizer',
          definition = function(object, name, team, position) {

            pdata <- get_player_data(object)

            # Go by name
            player <- pdata[fullname == name]

            if (!missing(team)) {
              TM <- team
              player <- player[team == TM]
            }

            if (!missing(position)) {
              POS <- position
              player <- player[position == POS]
            }

            if (nrow(player) == 0) {
              message('Player not found!')
              return(invisible(NULL))
            } else {
              return(as.character(player$id))
            }

          })

## Updating
setGeneric("add_player", function(object, pl) standardGeneric("add_player"))
#' Method for adding a player to Optimizer object
#'
#' @param object an S4 optimizer object
#' @param pl an object of class Player
#'
#' @export
setMethod('add_player',
          signature = 'optimizer',
          definition = function(object, pl) {

            # Check that if player exists, they aren't added again
            if (any(sapply(object@players, identical, pl))) {
              warning('Player already exists')
            } else {
              object@players <- c(object@players, pl)
            }

            return(object)
          })


setGeneric("remove_player", function(object, id) standardGeneric("remove_player"))
#' Method for removing a player from Optimizer object
#'
#' @param object an S4 optimizer object
#' @param id ID of player to remove
#'
#' @export
setMethod('remove_player',
          signature = 'optimizer',
          definition = function(object, id) {
            index_to_remove <- which(sapply(object@players, id) == id)

            if (length(index_to_remove) == 0) {
              stop("Could not find player with ID equal to ", id)
            } else if (length(index_to_remove) > 1) {
              stop("Found more than one player with ID of ", id)
            } else {
              object@players[[index_to_remove]] <- NULL
            }
            return(object)
          })

setGeneric("update_fpts", function(object, fpts_data) standardGeneric('update_fpts'))
#' Method for updating fantasy points in an object
#'
#' @param object An object of class Optimizer
#' @param fpts_data a data.frame containing players and points. See details.
#'
#' @details The data.frame passed in fpts_data should contain two columns - \code{id} and \code{fpts}.
#'
#' @export
setMethod('update_fpts',
          signature = 'optimizer',
          definition = function(object, fpts_data){

            # Check for columns
            if (!all(c('fpts','id') %in% colnames(fpts_data))) { stop('fpts_data must have columns `fpts` and `id`')}

            # Update the data
            for (i in 1:nrow(fpts_data)) {

              # just for code clarity
              ID  <- fpts_data$id[i]
              PTS <- fpts_data$fpts[i]

              # check for existence
              if (is.null(object@players[[ID]])) {
                next
              } else {
                object@players[[ID]] <- set_fpts(object@players[[ID]], PTS)
              }

            }
            return(object)
          })


setGeneric('block_players_by_id', function(object, player_ids) standardGeneric('block_players_by_id'))
#' Function to block players by ID
#'
#' @param object an S4 object of class Optimizer
#' @param player_ids IDs of players to block
#'
#' @export
setMethod('block_players_by_id', 'optimizer',
          function(object, player_ids) {
            # Find player
            for (pid in player_ids){
              indx <- which(sapply(object@players, id) == pid)
              for (i in indx) {
                object@players[[i]] <- block_player(object@players[[i]])
              }
            }
            return(object)
          })


setGeneric('lock_players_by_id', function(object, player_ids) standardGeneric('lock_players_by_id'))
#' Function to block players by ID
#'
#' @param object an S4 object of class Optimizer
#' @param player_ids IDs of players to block
#'
#' @export
setMethod('lock_players_by_id', 'optimizer',
          function(object, player_ids) {
            # Find player
            for (pid in player_ids){
              indx <- which(sapply(object@players, id) == pid)
              for (i in indx) {
                object@players[[i]] <- lock_player(object@players[[i]])
              }
            }
            return(object)
          })


########## Running Models
setGeneric('construct_model', function(object) standardGeneric('construct_model'))
#' Method for constructing the optimization model
#'
#' @param object an S4 object of class Optimizer
#'
#' @export
setMethod('construct_model',
          signature = 'optimizer',
          definition = function(object) {

            # Get base config
            base_config <- base_settings[[object@site]][[object@sport]][[object@contest_type]]

            # Checking for players
            if (length(object@players) == 0) {
              stop('No players found, cannot construct a model!')
            }

            # Start constructing the model
            object@model@mod <- build_base_model(
              size = length(object@players),
              team_vector = sapply(object@players, team),
              # position_vector = sapply(object@players, position),
              # roster_key = base_config$roster_key,
              pts  = extract_player_fpts(object),
              maximize = object@maximize
            )

            # Adding roster limit
            object@model@mod <- add_roster_size_constraint(object@model@mod, roster_limit = base_config$roster)

            # Adding budget constraint
            salaries <- sapply(object@players, function(p){p@salary})
            object@model@mod <- add_budget_constraint(object@model@mod,
                                                      player_salaries = salaries,
                                                      budget = base_config$budget)

            # Add team size constraints
            object@model@mod <- add_team_number_constraints(model = object@model@mod,
                                                            min_team_number = base_config$min_team_req,
                                                            max_players_per_team = base_config$max_players_per_team)

            # Add positional constraint
            object@model@mod <- add_position_constraint(model = object@model@mod,
                                                        position_vector = sapply(object@players, position),
                                                        roster_key = base_config$roster_key,
                                                        flex_positions = object@model@flex_positions)

            # Add unique ID constraint
            object@model@mod <- add_unique_id_constraint(model = object@model@mod,
                                                         ids = sapply(object@players, id))

            return(object)

          })

setGeneric('optimize', function(object, num_lineups = 1) standardGeneric('optimize'))
#' Function to Generate lineups
#'
#' @param object an S4 object of class Optimizer
#' @param num_lineups Number of lineups to generate
#'
#' @export
setMethod('optimize',
          signature = 'optimizer',
          definition = function(object, num_lineups = 1) {

            # Construct Model
            # Necessary to do this now so we do just-in-time construction
            M <- construct_model(object)

            # Build a player data set
            # We can then filter from this below, where we need the relevant rows the optimizer solved for
            solution_vectors <- list()
            lineups <- vector(mode = 'list', length = num_lineups)

            # Block Players
            M@model@mod <- add_block_constraint(M@model@mod,
                                                block_vector = sapply(object@players, blocked))

            # Lock Players
            M@model@mod <- add_lock_constraint(M@model@mod,
                                               lock_vector = sapply(object@players, locked))

            # Generate Lineups
            for (i in 1:num_lineups) {

              # Temporary Model
              current_model <- M@model@mod

              # Add unique roster constraint
              current_model <- add_unique_lineup_constraint(current_model, solution_vectors)

              # If any player is currently above their exposure rate, block them
              if (length(solution_vectors) > 0) {
                current_exposures <- calculate_exposure(solution_vectors)
                over_exposed <- which(current_exposures > sapply(object@players, max_exposure))

                # Ignore Locked and blocked
                over_exposed <- setdiff(over_exposed,
                                        c(which(sapply(object@players, locked) == 1),
                                          which(sapply(object@players, blocked) == 1)))

                # Add exposure constraint
                if (length(over_exposed) > 0) {
                  current_model <- current_model %>%
                    ompr::add_constraint(players[i] == 0, i = over_exposed)
                }
              }

              # Solve the model
              fit_model <- ompr::solve_model(current_model,
                                             solver = ompr.roi::with_ROI(M@model@solver))

              # Break if not optimal
              if (fit_model$status != 'optimal') {
                stop('Model could not reach a solution.')
              }

              # Get solution index
              solution_index <- ompr::get_solution(fit_model, players[i])

              # Add to existing rosters
              solution_vectors[[i]] <- solution_index$value

              # TO DO -- get only relevant rows (not the index, but the table containing players' data)
              lineups[[i]] <- get_player_data(object)[which(solution_vectors[[i]]==1),]

            }

            return(lineups)

          })


