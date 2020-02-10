#' @include class-optimizer.R

setGeneric('add_players_from_csv', function(object, filepath) standardGeneric('add_players_from_csv'))
#' Add players to optimizer from CSV
#'
#' @param object An optimizer model object
#' @param filepath Filepath location of the CSV
#'
#' @return Optimizer model object with slot \code{players} filled
#'
#' @examples
#' \dontrun{
#' opt <- optimizer(site = 'DRAFTKINGs', sport = 'HOCKEY', contest_type = 'CLASSIC')
#' opt <- add_players_from_csv(object = opt, filepath = '/Path/to/file.csv')
#' }
#'
#' @export
setMethod(f = 'add_players_from_csv',
          signature = 'optimizer',
          definition = function(object, filepath) {
            # Read in the players and put them in the players slot
            object@players <- get_players_from_csv(path = filepath, site = object@site)
            return(object)
          })


setGeneric('add_team_stack', function(object, positions) standardGeneric('add_team_stack'))
#' Add a Team Stack
#'
#' @param object An optimizer model object
#' @param positions Positions to stack within same team
#'
#' @return Updated optimizer object
#'
#' @examples
#' \dontrun{
#' opt <- optimizer(site = 'DRAFTKINGs', sport = 'HOCKEY', contest_type = 'CLASSIC')
#' opt <- add_players_from_csv(object = opt, filepath = '/Path/to/file.csv')
#'
#' # Add team stack, requiring a Center and Two Wingers from the same team
#' opt <- add_team_stack(object = opt, positions = c('C','W','W'))
#' }
#'
#'@export
setMethod(f = 'add_team_stack',
          signature = 'optimizer',
          definition = function(object, positions) {

            # Create constraint
            CON <- constraintClass(constraint_name = "Team Stack Constraint",
                                   fnc = .add_team_stack,
                                   args = list(positions = positions))

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
#' opt <- optimizer(site = 'DRAFTKINGs', sport = 'HOCKEY', contest_type = 'CLASSIC')
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
            CON <- constraintClass(constraint_name = "Opposing Positions Restriction",
                                   fnc = .restrict_opposing_position,
                                   args = list(pos1 = pos1, pos2 = pos2))

            # Add it to the config object
            object@config <- include_constraint(object@config, CON)

            return(object)

          })



setGeneric('set_roster_size', function(object, value) standardGeneric('set_roster_size'))
#' @title Set the Roster Size
#'
#' @param object An optimizer object
#' @param value Value to set the roster size (subject to validity checks).
#'
#' @rdname set_roster_size
#' @description Method to set the roster size requirement of an optimizer model.
#'
#' @return Updated Optimizer object
#'
#' @export
setMethod(f = 'set_roster_size',
          signature = 'optimizer',
          definition = function(object, value) {
            roster_size(object@config) <- as.integer(value)
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
#' opt <- optimizer(site = 'DRAFTKINGs', sport = 'HOCKEY')
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
            flex_positions(object@config) <- positions
            return(object)
          })



setMethod(f = 'set_max_exposure',
          signature = 'optimizer',
          definition = function(object, exposure) {
            max_exposure(object@config) <- exposure
            return(object)
          })
