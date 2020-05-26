
setGeneric("fpts", function(x) standardGeneric("fpts"))
# Accessor functions
#' @keywords internal
setMethod("fpts", "player_object", function(x) x@fpts)

setGeneric("id", function(x) standardGeneric("id"))
#' @keywords internal
setMethod("id", "player_object", function(x) x@id)

setGeneric("fullname", function(x) standardGeneric("fullname"))
#' @keywords internal
setMethod("fullname", "player_object", function(x) x@fullname)

setGeneric("first_name", function(x) standardGeneric("first_name"))
#' @keywords internal
setMethod("first_name", "player_object", function(x) x@first_name)

setGeneric("last_name", function(x) standardGeneric("last_name"))
#' @keywords internal
setMethod("last_name", "player_object", function(x) x@last_name)

setGeneric("team", function(x) standardGeneric("team"))
#' @keywords internal
setMethod("team", "player_object", function(x) x@team)

setGeneric("position", function(x) standardGeneric("position"))
#' @keywords internal
setMethod("position", "player_object", function(x) x@position)

setGeneric("salary", function(x) standardGeneric("salary"))
#' @keywords internal
setMethod("salary", "player_object", function(x) x@salary)

# Generic already set in class-config
#setGeneric("max_exposure", function(x) standardGeneric("max_exposure"))
#' @keywords internal
setMethod("max_exposure", "player_object", function(x) x@max_exposure)

setGeneric('min_exposure', function(x) standardGeneric('min_exposure'))
#' @keywords internal
setMethod('min_exposure', 'player_object', function(x) x@min_exposure)

setGeneric("blocked", function(x) standardGeneric("blocked"))
#' @keywords internal
setMethod("blocked", "player_object", function(x) as.numeric(x@blocked))

setGeneric("locked", function(x) standardGeneric("locked"))
#' @keywords internal
setMethod("locked", "player_object", function(x) as.numeric(x@locked))

setGeneric('get_opposing_team', function(x) standardGeneric('get_opposing_team'))
#' Accessing Data from Player Objects
#' @param x player_object
#' @aliases get_opposing_team
#' @export
setMethod('get_opposing_team', 'player_object',
          function(x){
            opponent <- setdiff(c(home_team(x@game_info), away_team(x@game_info)),
                                team(x))
            return(opponent)

          })

#setGeneric('variance', function(x) standardGeneric('variance'))
#' @keywords internal
setMethod('variance', 'player_object', function(x) x@variance)

# Generic Defined in optim methods
#' player_object formatter method
#'
#' @param object Object of class \code{player_object}
#'
setMethod('get_player_data', 'player_object',
          function(object) {

            # Get all the object names and values
            object_names <- c('id','fullname','team','position','salary','fpts')
            vals <- lapply(object_names, function(obn) slot(object, obn))
            names(vals) <- object_names
            return(data.frame(vals, stringsAsFactors = FALSE))

          })


# Update methods
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


#' Player_object max exposure
#' @param object A Player_object
#' @param exposure Value of exposure
#' @keywords internal
setGeneric('set_player_max_exposure', function(object, exposure) standardGeneric('set_player_max_exposure'))
setMethod('set_player_max_exposure', 'player_object',
          function(object, exposure){
            if (exposure > 1 || exposure < 0) {
              stop('Maximum Exposure must be between 0 and 1')
            }
            object@max_exposure <- exposure
            return(object)
          })


#' Internal function for setting player as injured
#' @keywords internal
setGeneric('set_as_injured', function(object) standardGeneric('set_as_injured'))
setMethod('set_as_injured', 'player_object',
          function(object){
            object@is_injured <- TRUE
            return(object)
          })


#' @keywords internal
setGeneric('set_as_active', function(object) standardGeneric('set_as_active'))
setMethod('set_as_active', 'player_object',
          function(object){
            object@is_injured <- FALSE
            return(object)
          })


#' Setting Fpts
#' @param object A player_object
#' @param value Value of FPTs
#' @keywords internal
setGeneric('set_fpts<-', function(object, value) standardGeneric('set_fpts<-'))
setMethod('set_fpts<-', 'player_object',
          function(object, value) {
            object@fpts <- value
            return(object)
          })


#' Setting Variance
#' @param object A Player_object
#' @param variance Value of variance
#' @keywords internal
setGeneric('set_variance', function(object, variance) standardGeneric('set_variance'))
setMethod('set_variance', 'player_object', function(object, variance) {
  object@variance <- variance
  return(object)
  })

#setGeneric('apply_variance', function(object, varpct) standardGeneric('apply_variance'))
#' apply variance value to player object
#'
#' @param object a player_object
#' @keywords internal
setMethod('apply_variance', 'player_object',
          function(object) {
            # If no variance, return the object
            if (is.na(object@variance)) return(object)

            # Else, add random value based on that variance
            pct <- object@fpts * object@variance
            object@fpts <- object@fpts + stats::runif(1, min = -pct, max = pct)
            return(object)
          })

# Utility function
#' List player attributes
#'
#' @returns A vector of the attributes available for player objects (i.e., the values the user can set for an individual player).
#' @export
list_player_attributes <- function() {
  slotNames('player_object')
}
