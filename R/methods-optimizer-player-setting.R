setGeneric('set_player_max_exp',
           function(object, id, exp) standardGeneric('set_player_max_exp'))
#' @title Set a player's max exposure by ID
#'
#' @param object An optimizer object
#' @param id The ID of a player
#' @param exp A value of exposure
#'
#' @return Updated Optimizer object
#'
#' @examples
#' \dontrun{
#' # Set Carey Price to have a maximum exposure of 75%
#' ID <- get_player_id(opt, 'Carey Price')
#' opt <- opt %>% set_player_max_exp(id = ID, exp = .75)
#' }
#'
#' @aliases set_player_max_exp
#'
#' @export
setMethod(f = 'set_player_max_exp',
          signature = 'optimizer',
          definition = function(object, id, exp) {
            # Set and return
            object@players[[id]] <- set_player_max_exposure(object@players[[id]], exp)
            return(object)
          })


setGeneric('set_player_min_exp',
           function(object, id, exp) standardGeneric('set_player_min_exp'))
#' @title Set a player's Minimum exposure by ID
#'
#' @param object An optimizer object
#' @param id The ID of a player
#' @param exp A value of exposure
#'
#' @return Updated Optimizer object
#'
#' @examples
#' \dontrun{
#' # Set Patrick Mahomes to have a minimum exposure of 75%
#' ID <- get_player_id(opt, 'Patrick Mahomes')
#' opt <- opt %>% set_player_min_exp(id = ID, exp = .75)
#' }
#'
#' @aliases set_player_min_exp
#'
#' @export
setMethod(f = 'set_player_min_exp',
          signature = 'optimizer',
          definition = function(object, id, exp) {
            # Set and return
            object@players[[id]] <- set_min_exposure(object@players[[id]], exp)
            return(object)
          })


setGeneric('set_player_variance',
           function(object, id, variance) standardGeneric('set_player_variance'))
#' @title Set a player's max exposure by ID
#'
#' @param object An optimizer object
#' @param id The ID of a player
#' @param variance A value of variance
#'
#' @return Updated Optimizer object
#'
#' @examples
#' \dontrun{
#' # Set Carey Price's fpts to vary by plus-or-minus 25%
#' ID <- get_player_id(opt, 'Carey Price')
#' opt <- opt %>% set_player_variance(id = ID, variance = .25)
#' }
#'
#' @aliases set_player_variance
#'
#' @export
setMethod(f = 'set_player_variance',
          signature = 'optimizer',
          definition = function(object, id, variance) {
            # Set and return
            object@players[[id]] <- set_variance(object@players[[id]], variance)
            return(object)
          })


setGeneric("set_fpts_by_id", function(object, id, fpts) standardGeneric('set_fpts_by_id'))
#' Method for updating fantasy points in an object
#'
#' @param object An object of class Optimizer
#' @param id A Player ID to update
#' @param fpts Value for slot \code{fpts} of Player object
#'
#' @return Updated optimizer object
#'
#' @aliases set_fpts_by_id
#'
#' @export
setMethod('set_fpts_by_id',
          signature = 'optimizer',
          definition = function(object, id, fpts){

            # Find the player by ID
            if (is.null(object@players[[as.character(id)]])) {
              stop('ID not found in slot `players`')
            }

            set_fpts(object@players[[id]]) <- fpts

            return(object)
          })


setGeneric('block_players_by_id', function(object, player_ids) standardGeneric('block_players_by_id'))
#' Function to block players by ID
#'
#' @param object an S4 object of class Optimizer
#' @param player_ids IDs of players to block
#'
#' @return Updated optimizer object
#'
#' @aliases block_players_by_id
#'
#' @export
setMethod('block_players_by_id', 'optimizer',
          function(object, player_ids) {
            # Find player
            for (pid in player_ids){
              indx <- which(sapply(object@players, id) == pid)
              for (i in indx) {
                object@players[[i]] <- block_player(object@players[[i]])
              }
            }
            return(object)
          })


setGeneric('lock_players_by_id', function(object, player_ids) standardGeneric('lock_players_by_id'))
#' Function to block players by ID
#'
#' @param object an S4 object of class Optimizer
#' @param player_ids IDs of players to block
#'
#' @return Updated optimizer object
#'
#' @aliases lock_players_by_id
#'
#' @export
setMethod('lock_players_by_id', 'optimizer',
          function(object, player_ids) {
            # Find player
            for (pid in player_ids){
              indx <- which(sapply(object@players, id) == pid)
              for (i in indx) {
                object@players[[i]] <- lock_player(object@players[[i]])
              }
            }
            return(object)
          })


setGeneric("update_fpts", function(object, fpts_data) standardGeneric('update_fpts'))
#' Method for updating fantasy points in an object
#'
#' @param object An object of class Optimizer
#' @param fpts_data a data.frame containing players and points. See details.
#'
#' @details The data.frame passed in fpts_data must contain two columns - \code{id} and \code{fpts}.
#'
#' @return Updated optimizer object
#'
#' @aliases update_fpts
#'
#' @export
setMethod('update_fpts',
          signature = 'optimizer',
          definition = function(object, fpts_data){

            # Check for columns
            if (!all(c('fpts','id') %in% colnames(fpts_data))) { stop('fpts_data must have columns `fpts` and `id`')}

            # Update the data
            for (i in 1:nrow(fpts_data)) {

              # just for code clarity
              ID  <- fpts_data$id[i]
              PTS <- fpts_data$fpts[i]

              # check for existence
              if (is.null(object@players[[ID]])) {
                next
              } else {
                object <- set_fpts_by_id(object, id = ID, fpts = PTS)
              }

            }
            return(object)
          })
