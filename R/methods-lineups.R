
setGeneric('get_player_exposures', function(object) standardGeneric('get_player_exposures'))
#' @title Get Player Exposures
#'
#' @param object Object of class lineups
#'
#' @aliases get_player_exposures
#'
#' @export
setMethod('get_player_exposures', 'lineupClass',
          function(object) {
            return(get_player_summary(object@lineups))
          })


setGeneric('format_for_upload', function(object) standardGeneric('format_for_upload'))
#' Function for getting lineups into uploadable format
#'
#' @param object An object of class Lineups
#'
#' @aliases format_for_upload
#'
#' @export
setMethod('format_for_upload', 'lineupClass',
          function(object) {
            ids <- lapply(object@lineups, function(i) {
              o <- data.frame(t(i$id))
              names(o) <- i$roster_position
              return(o)
            })
            return(data.table::rbindlist(ids))
          })


setGeneric('export_lineups', function(object, file = "lineup_export.csv") standardGeneric('export_lineups'))
#' @title Export Lineups to CSV
#'
#' @param object An object of class Lineups
#' @param file A character string indicating the file to write to
#'
#' @aliases export_lineups
#'
#' @export
setMethod('export_lineups', 'lineupClass',
          function(object, file = 'lineup_export.csv') {
            # Format
            flines <- format_for_upload(object)
            # Write out using data.table
            data.table::fwrite(flines, file = file)
          })


setGeneric('convert_lineup_to_vector', function(x, opt) standardGeneric('convert_lineup_to_vector'))
#' @title Convert lineup object to indexes
#'
#' @param x lineupClass object
#' @param opt an optimizer model object
#'
#' @aliases convert_lineup_to_vector
#'
#' @keywords internal
setMethod(f = 'convert_lineup_to_vector',
          signature = 'lineupClass',
          definition = function(x, opt) {

            player_data <- get_player_data(opt)

            solution_vectors <- lapply(x@lineups, function (lineup) {
              .hold <- rep(x = 0, times = nrow(player_data))
              .hold[match(lineup$id, player_data$id)] <- 1
              return(.hold)
            })
          })
