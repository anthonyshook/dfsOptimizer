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



# #' Testing
# #' @include utils.R
# playerr6 <- R6::R6Class('player',
#                         list(
#                           sum = 0,
#                           add = add,
#                           add_shortname = add_shortname,
#                           s4 = player()
#                         ))
