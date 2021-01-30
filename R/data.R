#' A set of NHL Player Data to use for testing
#'
#' A dataset containing the names, ids, teams, and other attributes of
#' 308 NHL Players for a given Draftkings Contest.
#'
#' @format A data table with 308 rows and 8 variables:
#' \describe{
#'   \item{id}{ID of player}
#'   \item{first_name}{Player's first name}
#'   \item{last_name}{Player's last name}
#'   \item{team}{team}
#'   \item{position}{Roster position}
#'   \item{salary}{How much the player costs}
#'   \item{fpts}{Projected fantasy points}
#'   \item{game_info}{Info about the player's game, such as home/away teams and time}
#' }
"nhl_players"


#' Actual Draftkings Fantasy Points for the nhl_players set
#'
#' A dataset containing the names, ids, and true fantasy points for
#' the same draftkings slate used in nhl_players.
#'
#' @format A data table with 308 rows and 8 variables:
#' \describe{
#'   \item{fullname}{Player's full name}
#'   \item{id}{associated DK ID}
#'   \item{actuals}{Player's actual fantasy points}
#' }
"actuals"
