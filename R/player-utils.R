#' Get all combinations of players on same team
#'
#' @param players a list of Player objects
#' @param positions a character vector of positions to align
#'
#' @keywords internal
combine_same_team_players <- function(players, positions) {

  # Get teams
  teamvec <- sapply(players, team)
  teams   <- unique(teamvec)

  # Get positions
  posvec  <- sapply(players, position)

  browser()
  for (TEAM in teams) {

  }

}
