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


setGeneric('format_for_upload', function(object) standardGeneric('format_for_upload'))
#' Function for getting lineups into uploadable format
#'
#' @param object An object of class Lineups
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
#' @export
setMethod('export_lineups', 'lineupClass',
          function(object, file = 'lineup_export.csv') {
            # Format
            flines <- format_for_upload(object)
            # Write out using data.table
            data.table::fwrite(flines, file = file)
          })


