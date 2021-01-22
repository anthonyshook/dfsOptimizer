
##### Accessor Methods #####
setGeneric("budget", function(x) standardGeneric("budget"))
#' @keywords internal
setMethod("budget", "optimConfig", function(x) x@budget)

setGeneric("min_budget", function(x) standardGeneric("min_budget"))
#' @keywords internal
setMethod("min_budget", "optimConfig", function(x) x@min_budget)

setGeneric("roster_size", function(x) standardGeneric("roster_size"))
#' @keywords internal
setMethod("roster_size", "optimConfig", function(x) x@roster_size)

setGeneric("min_team_req", function(x) standardGeneric("min_team_req"))
#' @keywords internal
setMethod("min_team_req", "optimConfig", function(x) x@min_team_req)

setGeneric("max_team_req", function(x) standardGeneric("max_team_req"))
#' @keywords internal
setMethod("max_team_req", "optimConfig", function(x) x@max_team_req)

setGeneric("max_players_per_team", function(x) standardGeneric("max_players_per_team"))
#' @keywords internal
setMethod("max_players_per_team", "optimConfig", function(x) x@max_players_per_team)

setGeneric("max_overlap", function(x) standardGeneric("max_overlap"))
#' @keywords internal
setMethod("max_overlap", "optimConfig", function(x) x@max_overlap)

#' @keywords internal
setGeneric("roster_key", function(x) standardGeneric("roster_key"))
setMethod("roster_key", "optimConfig", function(x) x@roster_key)

#' @keywords internal
setGeneric("flex_position", function(x) standardGeneric("flex_position"))
setMethod("flex_position", "optimConfig", function(x) x@flex_position)

#' @keywords internal
setGeneric("max_exposure", function(x) standardGeneric("max_exposure"))
setMethod("max_exposure", "optimConfig", function(x) x@max_exposure)

#' @keywords internal
setGeneric("variance", function(x) standardGeneric("variance"))
setMethod("variance", "optimConfig", function(x) x@variance)

#' @keywords internal
setGeneric('multiplier_mode', function(x) standardGeneric('multiplier_mode'))
setMethod('multiplier_mode', 'optimSingleGameConfig', function(x) x@multiplier_mode)

#' @keywords internal
setGeneric('multiplier_name', function(x) standardGeneric('multiplier_name'))
setMethod('multiplier_name', 'optimSingleGameConfig', function(x) x@multiplier_name)

### Setter Methods
## All setters have the 'set_' prefix, and those generics are referenced in methods-optimizer
setGeneric('set_max_budget<-', function(x, value) standardGeneric('set_max_budget<-'))
setMethod('set_max_budget<-', 'optimConfig', function(x, value) {
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

setGeneric('set_roster_size<-', function(x, value) standardGeneric('set_roster_size<-'))
setMethod('set_roster_size<-', 'optimConfig', function(x, value) {
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

setGeneric('set_max_team_req<-', function(x, value) standardGeneric('set_max_team_req<-'))
setMethod('set_max_team_req<-', 'optimConfig', function(x, value) {
  x@max_team_req <- value
  stopifnot(validObject(x))
  return(x)
})

setGeneric('set_max_players_per_team<-', function(x, value) standardGeneric('set_max_players_per_team<-'))
setMethod('set_max_players_per_team<-', 'optimConfig', function(x, value) {
  x@max_players_per_team <- value
  stopifnot(validObject(x))
  return(x)
})

setGeneric('set_max_overlap<-', function(x, value) standardGeneric('set_max_overlap<-'))
setMethod('set_max_overlap<-', 'optimConfig', function(x, value) {
  x@max_overlap <- value
  stopifnot(validObject(x))
  return(x)
})

setGeneric('set_max_exposure<-', function(x, value) standardGeneric('set_max_exposure<-'))
setMethod('set_max_exposure<-', 'optimConfig', function(x, value) {
  x@max_exposure <- value
  stopifnot(validObject(x))
  return(x)
})

setGeneric('set_variance<-', function(x, value) standardGeneric('set_variance<-'))
setMethod('set_variance<-', 'optimConfig', function(x, value) {
  x@variance <- value
  stopifnot(validObject(x))
  return(x)
})

# Sub-classes?
# HOCKEY
# GOLF


##### Constraint and Util methods #####
setGeneric('get_roster_order', function(x) standardGeneric('get_roster_order'))
setMethod('get_roster_order', 'optimConfig', function(x) {
  o <- tryCatch({
    as.character(
      unlist(
        sapply(names(x@roster_key),
               function(Z) rep(Z, x@roster_key[[Z]]$num)
        )
      )
    )
  }, error = function(e) NULL)
  return(o)
})
