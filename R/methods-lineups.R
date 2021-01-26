
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


setGeneric('compare_lineups_with_actuals', function(object, actuals, show_warnings = TRUE) standardGeneric('compare_lineups_with_actuals'))
#' @title Compare Lineups with Actuals
#'
#' @param object An object of class Lineups
#' @param actuals A data.frame (or data.table, or tibble) containing players and actual fantasy points.  See details.
#' @param show_warnings Flag indicating whether warnings should be shown (e.g., "Player X was not found in \code{actuals}").
#'
#' @description This function can be used to determine what a lineup would have scored, given projections and actual
#' fantasy points. This makes back-testing different lineup parameters fast and easy.
#'
#' @details The \code{actuals} data.frame needs to contain a column called \code{actuals} that contains actual fantasy points,
#' and at least one of \code{id} (player IDs associated with the model run), or \code{fullname}.  If both identifier columns
#' are present, \code{id} will be preferentially used.
#'
#' @return Data.table of lineup comparisons
#'
#' @aliases compare_lineups_with_actuals
#'
#' @export
setMethod(f = 'compare_lineups_with_actuals',
          signature = 'lineupClass',
          definition = function(object, actuals, show_warnings = TRUE) {
            # Check actuals for appropriate columns
            if (!any(c('id','fullname') %in% colnames(actuals))) stop('`actuals` data.frame requires one of `id` or `fullname` as columns.')
            if (!('actuals' %in% colnames(actuals))) stop('Column `actuals` required in `actuals` data.frame.')

            # Pick the joining column
            if ('id' %in% colnames(actuals)) {
              join_term = 'id'
            } else {
              join_term = 'fullname'
            }

            # silly, maybe, but rename

            ## Now get the data out of the lineups.
            # Hold variables
            summarized_comps <- list()
            missing_names <- c()

            for (i in 1:length(object@lineups)) {
              # Merge the actuals
              tmp <- merge(x = object@lineups[[i]],
                           y = actuals[, .SD, .SDcols = c(join_term, 'actuals')],
                           by = join_term,
                           all.x = TRUE)
              # Check for missing names
              missing_names <- c(missing_names, tmp[is.na(actuals),]$fullname)

              # get the summary data
              summarized_comps[[i]] <- data.frame(
                lineup = names(object@lineups)[i],
                projected_pts = sum(tmp$fpts),
                actual_pts = sum(tmp$actuals, na.rm = TRUE)
              )
            }

            if (length(missing_names) > 0 && show_warnings) {
              warning(paste0('The following players were not found in the `actuals` table: \n    ',
                             paste(unique(missing_names),collapse='\n    ')))
            }

            # combine
            final <- data.table::rbindlist(summarized_comps)
            final[, diff := actual_pts - projected_pts]

            return(final[order(-actual_pts)])

          })

