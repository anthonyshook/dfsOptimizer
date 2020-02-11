#' S4 Class optimConfig
#'
#' @slot budget Lineup budget
#' @slot min_budget the minimum budget (Default: 0)
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
                           min_budget = 'numeric',
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
                           min_budget = 0,
                           flex_positions = NA_character_,
                           max_exposure = 1,
                           variance = 0
                         )
)

setMethod('show', signature = 'optimConfig', definition = function(object) {
  cat('An S4 object of class', class(object))
})

setValidity('optimConfig', method = function(object) {

  # Set controls
  validcheck <- TRUE
  # msg <- c()

  msg <- c(if (object@max_players_per_team > object@roster_size) "Cannot have more players per team than total roster size",
           if (object@min_team_req < 1) "Minimum team requirment must be at least 1",
           if (object@max_exposure > 1 |
               object@max_exposure < 0) "max exposure must be between 0 and 1",
           if (object@variance > 1 |
               object@variance < 0) "max exposure must be between 0 and 1",
           if (length(object@constraints) > 0 &&
               !all(sapply(object@constraints, class) == 'constraintClass')) "Invalid object(s) found in constraints slot! List may only contain constraintClass objects"
  )
  if (!is.null(msg)) {
    validcheck <- FALSE
    message(paste0(msg, collapse = '\n'))
  }
  validcheck
})

## Accessor Methods
setGeneric("budget", function(x) standardGeneric("budget"))
setMethod("budget", "optimConfig", function(x) x@budget)

setGeneric("min_budget", function(x) standardGeneric("min_budget"))
setMethod("min_budget", "optimConfig", function(x) x@min_budget)

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

### Setter Methods
setGeneric('set_budget<-', function(x, value) standardGeneric('set_budget<-'))
setMethod('set_budget<-', 'optimConfig', function(x, value) {
  x@budget <- value
  stopifnot(validObject(x))
  return(x)
})

setGeneric('set_min_budget<-', function(x, value) standardGeneric('set_min_budget<-'))
setMethod('set_min_budget<-', 'optimConfig', function(x, value) {
  x@min_budget <- value
  stopifnot(validObject(x))
  return(x)
})

setGeneric('roster_size<-', function(x, value) standardGeneric('roster_size<-'))
setMethod('roster_size<-', 'optimConfig', function(x, value) {
  x@roster_size <- value
  stopifnot(validObject(x))
  return(x)
})

setGeneric('set_min_team_req<-', function(x, value) standardGeneric('set_min_team_req<-'))
setMethod('set_min_team_req<-', 'optimConfig', function(x, value) {
  x@min_team_req <- value
  stopifnot(validObject(x))
  return(x)
})

setGeneric('set_max_players_per_team<-', function(x, value) standardGeneric('set_max_players_per_team<-'))
setMethod('set_max_players_per_team<-', 'optimConfig', function(x, value) {
  x@max_players_per_team <- value
  stopifnot(validObject(x))
  return(x)
})

setGeneric('flex_positions<-', function(x, value) standardGeneric('flex_positions<-'))
setMethod('flex_positions<-', 'optimConfig', function(x, value) {
  x@flex_positions <- value
  stopifnot(validObject(x))
  return(x)
})

setGeneric('max_exposure<-', function(x, value) standardGeneric('max_exposure<-'))
setMethod('max_exposure<-', 'optimConfig', function(x, value) {
  x@max_exposure <- value
  stopifnot(validObject(x))
  return(x)
})

setGeneric('variance<-', function(x, value) standardGeneric('variance<-'))
setMethod('variance<-', 'optimConfig', function(x, value) {
  x@variance <- value
  stopifnot(validObject(x))
  return(x)
})

# Sub-classes?
# HOCKEY
# GOLF

setGeneric('include_constraint', function(x, constraint_object) standardGeneric('include_constraint'))
setMethod('include_constraint', 'optimConfig',
          function(x, constraint_object) {
            # This will add to the list if the field doesn't exist, but
            # replace it if it does. Makes it less likely to add various definitions
            # of the same constraint
            x@constraints[[constraint_object$constraint_name]] <- constraint_object
            return(x)
          })

