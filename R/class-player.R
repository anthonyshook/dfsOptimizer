#' S4 Player Class
#'
#' @slot id player ID
#' @slot first_name first name
#' @slot last_name last name
#' @slot fullname full name
#' @slot team team
#' @slot position position
#' @slot depth Position on depth chart or roster order (like batting order)
#' @slot salary salary
#' @slot fpts fantasy points
#' @slot locked Whether the player should be locked to all lineups
#' @slot blocked whether to omit player from all lineups
#' @slot is_injured injury flag
#' @slot min_exposure Single player minimum exposure
#' @slot max_exposure Single player maximum exposure
#' @slot game_info information about the game
#'
#' @keywords internal
.player <- setClass('player_object',
                   slots = list(
                     id = 'character',
                     first_name = 'character',
                     last_name = 'character',
                     fullname = 'character',
                     team = 'character',
                     position = 'character',
                     depth = 'integer',
                     salary = 'integer',
                     fpts = 'numeric',
                     locked = 'logical',
                     blocked = 'logical',
                     is_injured = 'logical',
                     min_exposure = 'numeric',
                     max_exposure = 'numeric',
                     game_info = 'gameInfo'
                   ),
                   prototype = list(
                     min_exposure = 0,
                     max_exposure = 1,
                     is_injured = FALSE,
                     locked = FALSE
                   ))

#' Initialization Function for Player class
#'
#' @param id player ID
#' @param first_name first name
#' @param last_name last name
#' @param team team (Required)
#' @param position position (Required)
#' @param depth Position on depth chart or roster order (like batting order) (Default 1)
#' @param salary salary
#' @param fpts fantasy points
#' @param locked Whether the player should be locked to all lineups (Default FALSE)
#' @param blocked Whether to omit player from all lineups (Default FALSE)
#' @param is_injured injury flag (Default FALSE)
#' @param min_exposure Single player minimum exposure (Default 0)
#' @param max_exposure Single player maximum exposure (Default 1)
#' @param game_info information about the game (Default is an empty game_info object)
#'
#' @details Function that initializes a Player object.
#'
#' @export
player <- function(id,
                   first_name,
                   last_name,
                   team,
                   position,
                   depth = 1L,
                   salary,
                   fpts,
                   locked = FALSE,
                   blocked = FALSE,
                   is_injured = FALSE,
                   min_exposure = 0,
                   max_exposure = 1,
                   game_info) {

  if (missing(game_info)){
    game_info <- new('gameInfo')
  }

  # Create the object
  p <- .player(id = as.character(id),
               first_name = first_name,
               last_name = last_name,
               fullname = paste(first_name, last_name),
               team = team,
               position = position,
               depth = depth,
               salary = as.integer(salary),
               fpts = fpts,
               locked = locked,
               blocked = blocked,
               is_injured = is_injured,
               min_exposure = min_exposure,
               max_exposure = max_exposure,
               game_info = game_info)

  return(p)

}

# Show method
setMethod('show', signature = 'player_object',
          definition = function(object){
            print(paste0(object@fullname, " (", object@position, ")"))
          })

# Accessor functions
setGeneric("fpts", function(x) standardGeneric("fpts"))
setMethod("fpts", "player_object", function(x) x@fpts)

setGeneric("id", function(x) standardGeneric("id"))
setMethod("id", "player_object", function(x) x@id)

setGeneric("fullname", function(x) standardGeneric("fullname"))
setMethod("fullname", "player_object", function(x) x@fullname)

setGeneric("team", function(x) standardGeneric("team"))
setMethod("team", "player_object", function(x) x@team)

setGeneric("position", function(x) standardGeneric("position"))
setMethod("position", "player_object", function(x) x@position)

setGeneric("salary", function(x) standardGeneric("salary"))
setMethod("salary", "player_object", function(x) x@salary)

setGeneric("max_exposure", function(x) standardGeneric("max_exposure"))
setMethod("max_exposure", "player_object", function(x) x@max_exposure)

setGeneric("blocked", function(x) standardGeneric("blocked"))
setMethod("blocked", "player_object", function(x) as.numeric(x@blocked))

setGeneric("locked", function(x) standardGeneric("locked"))
setMethod("locked", "player_object", function(x) as.numeric(x@locked))



# Formatter method
setGeneric('get_player_data', function(object) standardGeneric('get_player_data'))
setMethod('get_player_data', 'player_object',
          function(object) {

            # Get all the object names and values
            object_names <- c('id','fullname','team','position','salary','fpts')
            vals <- lapply(object_names, function(obn) slot(object, obn))

            names(vals) <- object_names

            return(data.frame(vals, row.names = FALSE))

          })

setGeneric('lock_player', function(object) standardGeneric('lock_player'))
setMethod('lock_player', 'player_object',
           function(object) {
             object@locked  <- TRUE
             object@blocked <- FALSE
             return(object)
           })

setGeneric('block_player', function(object) standardGeneric('block_player'))
setMethod('block_player', 'player_object',
           function(object) {
             object@locked  <- FALSE
             object@blocked <- TRUE
             return(object)
           })

setGeneric('set_min_exposure', function(object, exposure) standardGeneric('set_min_exposure'))
setMethod('set_min_exposure', 'player_object',
          function(object, exposure){
            if (exposure > 1 || exposure < 0) {
              stop('Minimum Exposure must be between 0 and 1')
            }
            object@min_exposure <- exposure
            return(object)
          })

setGeneric('set_max_exposure', function(object, exposure) standardGeneric('set_max_exposure'))
setMethod('set_max_exposure', 'player_object',
          function(object, exposure){
            if (exposure > 1 || exposure < 0) {
              stop('Maximum Exposure must be between 0 and 1')
            }
            object@max_exposure <- exposure
            return(object)
          })

setGeneric('set_as_injured', function(object, exposure) standardGeneric('set_as_injured'))
setMethod('set_as_injured', 'player_object',
          function(object){
            object@is_injured <- TRUE
            return(object)
          })

setGeneric('set_as_active', function(object, exposure) standardGeneric('set_as_active'))
setMethod('set_as_active', 'player_object',
          function(object){
            object@is_injured <- FALSE
            return(object)
          })


setGeneric('set_fpts', function(object, pts) standardGeneric('set_fpts'))
setMethod('set_fpts', 'player_object',
          function(object, pts) {
            object@fpts <- pts
            return(object)
          })
