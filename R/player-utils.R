#' Get all combinations of players on same team
#'
#' @param players a list of Player objects
#' @param positions a character vector of positions to align
#'
#' @keywords internal
link_players_on_same_team <- function(players, positions) {

  # Get data
  dat <- data.table::data.table(
    inx = 1:length(players),
    tms = sapply(players, team),
    pos = sapply(players, position)
  )

  teams   <- unique(dat$tms)

  X <- lapply(teams, function(TMS) {
    tdata <- dat[tms == TMS, ]

    plists <- lapply(positions, function(P){
      return(tdata[pos==P,]$inx)
    })
    return(do.call('expand.grid.unique', plists))
  })

  return(
    data.table::rbindlist(X)
  )
}
