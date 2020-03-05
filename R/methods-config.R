
##### Accessor Methods #####
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

setGeneric("flex_position", function(x) standardGeneric("flex_position"))
setMethod("flex_position", "optimConfig", function(x) x@flex_position)

setGeneric("max_exposure", function(x) standardGeneric("max_exposure"))
setMethod("max_exposure", "optimConfig", function(x) x@max_exposure)

setGeneric("variance", function(x) standardGeneric("variance"))
setMethod("variance", "optimConfig", function(x) x@variance)

setGeneric("min_budget", function(x) standardGeneric("min_budget"))
setMethod("min_budget", "optimConfig", function(x) x@min_budget)

### Setter Methods
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

setGeneric('flex_position<-', function(x, value) standardGeneric('flex_position<-'))
setMethod('flex_position<-', 'optimConfig', function(x, value) {
  x@flex_position <- value
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

### Different methods
setGeneric('include_constraint', function(x, constraint_object) standardGeneric('include_constraint'))
setMethod('include_constraint', 'optimConfig',
          function(x, constraint_object) {
            # This will add to the list if the field doesn't exist, but
            # replace it if it does. Makes it less likely to add various definitions
            # of the same constraint
            x@constraints[[constraint_object@constraint_name]] <- constraint_object
            return(x)
          })

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
