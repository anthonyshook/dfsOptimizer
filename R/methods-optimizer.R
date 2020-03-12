#' @include class-optimizer.R


setGeneric('add_additional_constraints', function(object) standardGeneric('add_additional_constraints'))
#' Add additional constraints
#'
#' @param object Optimizer model object
#'
#' @details Adds constraints from object configuration.
#'
#' @return The Optimizer object with an updated model.
#' @export
setMethod('add_additional_constraints', signature = 'optimizer',
          definition = function(object) {

            if (length(object@constraints) > 0) {
              for (CON in object@constraints){
                object@model <- apply_constraint(CON, model = object@model, players = object@players)
              }
            }
            return(object)
          })


setGeneric('include_constraint', function(x, constraint_object) standardGeneric('include_constraint'))
setMethod('include_constraint', 'optimizer',
          function(x, constraint_object) {
            # This will add to the list if the field doesn't exist, but
            # replace it if it does. Makes it less likely to add various definitions
            # of the same constraint
            x@constraints[[constraint_object@constraint_name]] <- constraint_object
            return(x)
          })


setGeneric('add_players_from_csv', function(object, filepath, custom = FALSE) standardGeneric('add_players_from_csv'))
#' Add players to optimizer from CSV
#'
#' @param object An optimizer model object
#' @param filepath Filepath location of the CSV
#' @param custom Set to TRUE to use user-generated CSVs with player data; the data will be
#'     parsed using \code{get_players_from_date_frame}
#'
#' @return Optimizer model object with slot \code{players} filled
#'
#' @examples
#' \dontrun{
#' opt <- create_optimizer(site = 'DRAFTKINGs', sport = 'HOCKEY', contest_type = 'CLASSIC')
#' opt <- add_players_from_csv(object = opt, filepath = '/Path/to/file.csv')
#' }
#'
#' @export
setMethod(f = 'add_players_from_csv',
          signature = 'optimizer',
          definition = function(object, filepath, custom = FALSE) {

            # Read in the players and put them in the players slot
            if (custom) {
              dat <- data.table::fread(filepath, stringsAsFactors = FALSE)
              object@players <- get_players_from_data_frame(dat)
            } else {
              object@players <- get_players_from_csv(object, path = filepath)
            }
            return(object)
          })


setGeneric('add_players_from_df', function(object, df) standardGeneric('add_players_from_df'))
#' Add players to optimizer from a data.frame
#'
#' @param object An optimizer model object
#' @param df a data.frame of players to add to the model
#'
#' @return Optimizer model object with slot \code{players} filled
#'
#' @examples
#' \dontrun{
#' opt <- create_optimizer(site = 'DRAFTKINGs', sport = 'HOCKEY', contest_type = 'CLASSIC')
#' dat <- data.frame(first_name = c('Sidney','Alex'),
#'                  last_name = c('Crosby','Ovechkin'),
#'                  fpts = c(18,20),
#'                  team = c('PIT','WAS'),
#'                  position = c('C','W'),
#'                  salary = c(8000,9000),
#'                  game_info = c('this will be','ignored anyway'),
#'                  max_exposure = c(0.3, 0.6))
#' opt <- add_players_from_df(object = opt, df = dat)
#' }
#'
#' @export
setMethod(f = 'add_players_from_df',
          signature = 'optimizer',
          definition = function(object, df) {
            # Read in the players and put them in the players slot
            object@players <- get_players_from_data_frame(df = df)
            return(object)
          })



setGeneric('add_team_stack', function(object, positions, nstacks = 1) standardGeneric('add_team_stack'))
#' Add a Team Stack
#'
#' @param object An optimizer model object
#' @param positions Positions to stack within same team
#' @param nstacks Number of stacks to include (Default is 1)
#'
#' @return Updated optimizer object
#'
#' @examples
#' \dontrun{
#' opt <- create_optimizer(site = 'DRAFTKINGs', sport = 'HOCKEY', contest_type = 'CLASSIC')
#' opt <- add_players_from_csv(object = opt, filepath = '/Path/to/file.csv')
#'
#' # Add team stack, requiring a Center and Two Wingers from the same team
#' opt <- add_team_stack(object = opt, positions = c('C','W','W'))
#' }
#'
#'@export
setMethod(f = 'add_team_stack',
          signature = 'optimizer',
          definition = function(object, positions, nstacks = 1) {

            # Create constraint
            CON <- .constraintClass(constraint_name = "Team Stack Constraint",
                                    fnc = .add_team_stack,
                                    args = list(positions = positions, nstacks = nstacks))

            # Add it to the config object
            object@config <- include_constraint(object@config, CON)

            return(object)

          })


setGeneric('restrict_opposing_positions', function(object, pos1, pos2) standardGeneric('restrict_opposing_positions'))
#' Restrict Opposing Positions
#'
#' @param object An optimizer model object
#' @param pos1 Positions for set one
#' @param pos2 Positions for set two
#'
#' @return Updated optimizer object
#'
#' @details Prevents positions from opposing teams from being included in lineups.
#'
#' @examples
#' \dontrun{
#' opt <- create_optimizer(site = 'DRAFTKINGs', sport = 'HOCKEY', contest_type = 'CLASSIC')
#' opt <- add_players_from_csv(object = opt, filepath = '/Path/to/file.csv')
#'
#' # Restrict lineup from having Skaters and Goalies from opposite teams
#' opt <- restrict_opposing_positions(object = opt, pos1 = c('C','W','D'), pos2 = 'G')
#' }
#'
#'@export
setMethod(f = 'restrict_opposing_positions',
          signature = 'optimizer',
          definition = function(object, pos1, pos2) {

            # Create constraint
            CON <- .constraintClass(constraint_name = "Opposing Positions Restriction",
                                    fnc = .restrict_opposing_position,
                                    args = list(pos1 = pos1, pos2 = pos2))

            # Add it to the config object
            object@config <- include_constraint(object@config, CON)

            return(object)

          })


setGeneric('set_max_exposure', function(object, exposure) standardGeneric('set_max_exposure'))
#' @title Set the Global Max Exposure
#'
#' @param object An optimizer object
#' @param exposure Value to set the roster size (subject to validity checks).
#'
#' @rdname set_max_exposure
#' @description Method to set the global max exposure of an optimizer model.
#'
#' @return Updated Optimizer object
#'
#' @export
setMethod(f = 'set_max_exposure',
          signature = 'optimizer',
          definition = function(object, exposure) {
            max_exposure(object@config) <- exposure
            return(object)
          })


setGeneric('set_flex_positions', function(object, positions) standardGeneric('set_flex_positions'))
#' @title Set the FLEX/UTIL position list
#'
#' @param object An optimizer object
#' @param positions Value to set the roster size (subject to validity checks).
#'
#' @rdname set_flex_positions
#' @description Method for setting FLEX/UTIL positions. Can be used to limit FLEX/UTIL to a user-specified
#'    set of possible positions (see examples).
#'
#' @return Updated Optimizer object
#'
#' @examples
#' \dontrun{
#' opt <- create_optimizer(site = 'DRAFTKINGs', sport = 'HOCKEY')
#' opt <- add_players_from_csv(object = opt, filepath = '/Path/to/file.csv')
#'
#' # Default FLEX is c('C','W','D') -- here we'll remove D
#' opt <- set_flex_positions(object = opt, positions = c('C','W'))
#' }
#'
#' @export
setMethod(f = 'set_flex_positions',
          signature = 'optimizer',
          definition = function(object, positions) {
            flxind <- which(names(roster_key(object@config)) == flex_position(object@config))
            for (i in flxind) {
              object@config@roster_key[[i]]$positions <- positions
            }
            return(object)
          })

setGeneric('apply_variance', function(object, varpct) standardGeneric('apply_variance'))
setMethod(f = 'apply_variance',
          signature = 'optimizer',
          definition = function(object, varpct) {
            object@players <- lapply(object@players, 'apply_variance', varpct = varpct)
            return(object)
          })



#### Methods
## Extraction
#' Extracting Fantasy Points from Player objects
#'
#' @param object an optimizer object
#'
setGeneric("extract_player_fpts", function(object) standardGeneric("extract_player_fpts"))
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

setGeneric('get_player_data', function(object) standardGeneric('get_player_data'))
setMethod('get_player_data', 'optimizer',
          function(object){

            players <- lapply(object@players, get_player_data)
                       # all     <- do.call('rbind', players)
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
                object <- set_fpts_by_id(object, id = ID, fpts = PTS)
              }

            }
            return(object)
          })


setGeneric("set_fpts_by_id", function(object, id, fpts) standardGeneric('set_fpts_by_id'))
#' Method for updating fantasy points in an object
#'
#' @param object An object of class Optimizer
#' @param id A Player ID to update
#' @param fpts Value for slot \code{fpts} of Player object
#'
#' @export
setMethod('set_fpts_by_id',
          signature = 'optimizer',
          definition = function(object, id, fpts){

            # Find the player by ID
            if (is.null(object@players[[as.character(id)]])) {
              stop('ID not found in slot `players`')
            }

            object@players[[id]] <- set_fpts(object@players[[as.character(id)]], fpts)

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


