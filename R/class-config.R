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
                           flex_positions = NA_character_,
                           max_exposure = 1
                         )
)

setMethod('show', signature = 'optimConfig', definition = function(object) {
  cat('An S4 object of class', class(object))
})

## Accessor Methods
setGeneric("budget", function(x) standardGeneric("budget"))
setMethod("budget", "optimConfig", function(x) x@budget)

setGeneric("roster_size", function(x) standardGeneric("roster_size"))
setMethod("roster_size", "optimConfig", function(x) x@roster_size)

setGeneric("min_team_req", function(x) standardGeneric("min_team_req"))
setMethod("min_team_req", "optimConfig", function(x) x@min_team_req)

setGeneric("max_players_per_team", function(x) standardGeneric("max_players_per_team"))
setMethod("max_players_per_team", "optimConfig", function(x) x@max_players_per_team)

setGeneric("roster_key", function(x) standardGeneric("roster_key"))
setMethod("roster_key", "optimConfig", function(x) x@roster_key)

setGeneric("flex_positions", function(x) standardGeneric("flex_positions"))
setMethod("flex_positions", "optimConfig", function(x) x@flex_positions)

setGeneric("max_exposure", function(x) standardGeneric("max_exposure"))
setMethod("max_exposure", "optimConfig", function(x) x@max_exposure)

setGeneric("variance", function(x) standardGeneric("variance"))
setMethod("variance", "optimConfig", function(x) x@variance)

# Sub-classes?
# HOCKEY
# GOLF
