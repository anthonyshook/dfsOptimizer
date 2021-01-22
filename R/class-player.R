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
#' @slot variance Single player variance
#' @slot game_info information about the game
#'
#' @keywords internal
setClass('player_object',
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
           variance = 'numeric',
           game_info = 'gameInfo'
         ),
         prototype = list(
           min_exposure = NA_real_,
           max_exposure = NA_real_,
           is_injured = FALSE,
           variance = NA_real_,
           locked = FALSE,
           blocked = FALSE,
           depth = 1L
         ))

#' Constructor Function for Player class
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
#' @param min_exposure Minimum exposure across lineups
#' @param max_exposure Maximum exposure across lineups
#' @param variance Amount of variance to apply to fpts during optimization (percentage)
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
                   min_exposure = NA_real_,
                   max_exposure = NA_real_,
                   variance = NA_real_,
                   game_info) {

  if (missing(game_info)){
    game_info <- new('gameInfo')
  }

  # Create the object
  p <- new('player_object',
           id = as.character(id),
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
           game_info = game_info)

  return(p)

}


# Show method
setMethod('show', signature = 'player_object',
          definition = function(object){
            print(paste0(object@fullname, " (", object@position, ")"))
          })

