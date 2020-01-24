#' S4 Class: Game Info
#'
#' @description Information about an individual player's game
#'
#' @slot home_team The home team
#' @slot away_team the away team
#' @slot shortname Short name for the game (see Details).
#' @slot start_time Start time for the game
#' @slot is_started a flag determining whether the game has started or not
#'
#' @export
gameInfo <- setClass('gameInfo',
                      slots = list(
                        home_team = 'character',
                        away_team = 'character',
                        shortname = 'character',
                        start_time = 'POSIXct',
                        is_started = 'logical'
                      ),
                     prototype = list(
                       start_time = .POSIXct(character(1)),
                       is_started = FALSE
                     ))

setMethod('show','gameInfo',function(object){
  cat(object@shortname)
})

setGeneric("home_team", function(x) standardGeneric("home_team"))
setMethod("home_team", "gameInfo", function(x) x@home_team)

setGeneric("away_team", function(x) standardGeneric("away_team"))
setMethod("away_team", "gameInfo", function(x) x@away_team)
