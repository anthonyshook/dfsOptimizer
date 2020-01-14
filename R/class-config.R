#' S4 Class optimConfig
#'
#' @slot budget Lineup budget
#' @slot roster_size Roster limitation (How many players allowed)
#' @slot min_team_req Number of teams required to be represented in the lineup
#' @slot max_players_per_team Maximum number of players from any one team
#' @slot roster_key List containing roster positions and the number of each position required
#' @slot flex_positions Positions that are eligible for FLEX/UTIL slots
#' @slot max_exposure Maximum exposure for individual players (global)
#' @slot variance Percentage variance for fantasy points -- used to add randomness to the model.
#' @slot constraints A \code{list} containing additional constraint objects
#'
.optimConfig <- setClass('optimConfig',
                         slots = list(
                           budget = 'numeric',
                           roster_size = 'integer',
                           min_team_req = 'integer',
                           max_players_per_team = 'integer',
                           roster_key = 'list',
                           flex_positions = 'character',
                           max_exposure = 'numeric',
                           variance = 'numeric',
                           constraints = 'list'
                         ),
                         prototype = list(
                           flex_positions = NA_character_
                         )
)

setMethod('show', signature = 'optimConfig', definition = function(object) {
  cat('An S4 object of class', class(object))
})

# Sub-classes?
# HOCKEY
# GOLF
