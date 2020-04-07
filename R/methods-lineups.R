setGeneric('get_player_exposures', function(object) standardGeneric('get_player_exposures'))
#' @title Get Player Exposures
#'
#' @param object Object of class lineups
#'
#' @export
setMethod('get_player_exposures', 'lineupClass',
          function(object) {
            return(get_player_summary(object@lineups))
          })

