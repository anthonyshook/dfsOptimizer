#' S4 Lineup class
#'
#' @slot num_lineups Number of lineups
#' @slot lineups A list of lineups
#'
setClass('lineupClass',
         slots = list(
           num_lineups = 'numeric',
           lineups = 'list'
         ),
         contains = 'VIRTUAL')


# Show method
setMethod('show', 'lineupClass', function(object) {
  # Just print lineups to screen
  completed_lineups <- object@lineups[!sapply(object@lineups, is.null)]
  if (length(completed_lineups) > 0) {
    print(completed_lineups)
  } else {
    print("No complete lineups found!")
  }
})


# Summary for lineupClass
summary.lineupClass <- function(object) {
  nonnull_lineups <- object@lineups[!sapply(object@lineups, is.null)]
  cat(paste0('Number of Expected Lineups: ', length(object@lineups),'\n'))
  cat(paste0('Number of Lineups Found: ', length(nonnull_lineups), '\n\n'))

  if (length(nonnull_lineups) > 0) {
    # Then we'll need to do something, but for now
    cat("Player Exposures:\n")
    smry <- get_player_summary(nonnull_lineups)
    print(smry)
    cat("\nPlayers Per Team:")
    print(get_team_summary(nonnull_lineups))
  }

}


# Simple way to extract the lineup list to do whatever you want with it.
as.list.lineupClass <- function(object) {
  return(object@lineups)
}


# Subclasses
setClass('lineupClassic', contains = 'lineupClass')
setClass('lineupSingle', contains = 'lineupClass')
