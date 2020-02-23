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
            if (length(object@config@constraints) > 0) {
              for (CON in object@config@constraints){
                object <- apply_constraint(CON, optObj = object)
              }
            }
            return(object)
          })

setGeneric('add_players_from_csv', function(object, filepath, site) standardGeneric('add_players_from_csv'))
#' Add players to optimizer from CSV
#'
#' @param object An optimizer model object
#' @param filepath Filepath location of the CSV
#' @param site The site from which the CSV was obtained. If not provided, the site is inferred from
#'    the model object. For user-generated CSVs with player data, one can set this argument to 'CUSTOM',
#'    and the data will be parsed by \code{get_players_from_date_frame}
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
          definition = function(object, filepath, site) {

            if (missing('site')) {
              site = object@site
            }

            # Read in the players and put them in the players slot
            object@players <- get_players_from_csv(path = filepath, site = toupper(site))
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
#' Add a Team Stack
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


setMethod(f = 'apply_variance',
          signature = 'optimizer',
          definition = function(object, varpct) {
            object@players <- lapply(object@players, 'apply_variance', varpct = varpct)
            return(object)
          })
