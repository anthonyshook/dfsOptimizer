
# Accessor functions
#' @ export
setGeneric("fpts", function(x) standardGeneric("fpts"))
setMethod("fpts", "player_object", function(x) x@fpts)

#' @ export
setGeneric("id", function(x) standardGeneric("id"))
setMethod("id", "player_object", function(x) x@id)

#' @ export
setGeneric("fullname", function(x) standardGeneric("fullname"))
setMethod("fullname", "player_object", function(x) x@fullname)

#' @ export
setGeneric("first_name", function(x) standardGeneric("first_name"))
setMethod("first_name", "player_object", function(x) x@first_name)

#' @ export
setGeneric("last_name", function(x) standardGeneric("last_name"))
setMethod("last_name", "player_object", function(x) x@last_name)

#' @ export
setGeneric("team", function(x) standardGeneric("team"))
setMethod("team", "player_object", function(x) x@team)

#' @ export
setGeneric("position", function(x) standardGeneric("position"))
setMethod("position", "player_object", function(x) x@position)

#' @ export
setGeneric("salary", function(x) standardGeneric("salary"))
setMethod("salary", "player_object", function(x) x@salary)

# Generic already set in class-config
#setGeneric("max_exposure", function(x) standardGeneric("max_exposure"))
#' @ export
setMethod("max_exposure", "player_object", function(x) x@max_exposure)

#' @ export
setGeneric('min_exposure', function(x) standardGeneric('min_exposure'))
setMethod('min_exposure', 'player_object', function(x) x@min_exposure)

#' @ export
setGeneric("blocked", function(x) standardGeneric("blocked"))
setMethod("blocked", "player_object", function(x) as.numeric(x@blocked))

#' @ export
setGeneric("locked", function(x) standardGeneric("locked"))
setMethod("locked", "player_object", function(x) as.numeric(x@locked))

#' @ export
setGeneric('get_opposing_team', function(x) standardGeneric('get_opposing_team'))
setMethod('get_opposing_team', 'player_object',
          function(x){
            opponent <- setdiff(c(home_team(x@game_info), away_team(x@game_info)),
                                team(x))
            return(opponent)

          })

#' @ export
#setGeneric('variance', function(x) standardGeneric('variance'))
setMethod('variance', 'player_object', function(x) x@variance)


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


#' @ export
setGeneric('set_min_exposure', function(object, exposure) standardGeneric('set_min_exposure'))
setMethod('set_min_exposure', 'player_object',
          function(object, exposure){
            if (exposure > 1 || exposure < 0) {
              stop('Minimum Exposure must be between 0 and 1')
            }
            object@min_exposure <- exposure
            return(object)
          })


# setGeneric('set_max_exposure', function(object, exposure) standardGeneric('set_max_exposure'))
#' @ export
setMethod('set_max_exposure', 'player_object',
          function(object, exposure){
            if (exposure > 1 || exposure < 0) {
              stop('Maximum Exposure must be between 0 and 1')
            }
            object@max_exposure <- exposure
            return(object)
          })


#' @ export
setGeneric('set_as_injured', function(object, exposure) standardGeneric('set_as_injured'))
setMethod('set_as_injured', 'player_object',
          function(object){
            object@is_injured <- TRUE
            return(object)
          })


#' @ export
setGeneric('set_as_active', function(object, exposure) standardGeneric('set_as_active'))
setMethod('set_as_active', 'player_object',
          function(object){
            object@is_injured <- FALSE
            return(object)
          })


#' @ export
setGeneric('set_fpts', function(object, pts) standardGeneric('set_fpts'))
setMethod('set_fpts', 'player_object',
          function(object, pts) {
            object@fpts <- pts
            return(object)
          })

#' @ export
setGeneric('set_variance', function(object, variance) standardGeneric('set_variance'))
setMethod('set_variance', 'player_object', function(object, variance) {
  object@variance <- variance
  return(object)
  })

#setGeneric('apply_variance', function(object, varpct) standardGeneric('apply_variance'))
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
