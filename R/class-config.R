#' S4 Class optimConfig
#'
#' @slot budget Lineup budget
#' @slot min_budget the minimum budget (Default: 0)
#' @slot roster_size Roster limitation (How many players allowed)
#' @slot min_team_req Number of teams required to be represented in the lineup
#' @slot max_players_per_team Maximum number of players from any one team
#' @slot max_overlap Maximum number of players allowed to overlap across lineups (Must be < roster_size)
#' @slot roster_key List containing roster positions and the number of each position required
#' @slot flex_position named identifer for the flex positions
#' @slot max_exposure Maximum exposure for individual players (global)
#' @slot variance Percentage global variance for fantasy points -- used to add randomness to the model.
#'
setClass('optimConfig',
         slots = list(
           budget = 'numeric',
           min_budget = 'numeric',
           roster_size = 'integer',
           min_team_req = 'integer',
           max_players_per_team = 'integer',
           max_overlap = 'numeric',
           roster_key = 'list',
           flex_position = 'character',
           max_exposure = 'numeric',
           variance = 'numeric'
         ),
         prototype = list(
           min_budget = 0,
           flex_position = NA_character_,
           max_exposure = 1,
           variance = NA_real_
         )
)


##### Base Methods #####
setMethod('show', signature = 'optimConfig', definition = function(object) {
  cat(paste0('Optimizer Configuration Object (', class(object),')'))
})

setValidity('optimConfig', method = function(object) {

  # Set controls
  validcheck <- TRUE
  # msg <- c()

  msg <- c(if (object@max_players_per_team > object@roster_size) "Cannot have more players per team than total roster size",
           if (object@min_team_req < 1) "Minimum team requirement must be at least 1",
           if (object@max_exposure > 1 |
               object@max_exposure < 0) "max exposure must be between 0 and 1",
           if (object@variance > 1 |
               object@variance < 0) "variance must be between 0 and 1",
           if (object@budget < object@min_budget) "Min budget cannot be less than max budget",
           if (object@max_overlap >= object@roster_size) "Max Overlap must be AT LEAST 1 less than roster_size",
           if (object@max_overlap < 0) "Max Overlap cannot be negative"
  )

  if (!is.null(msg)) {
    validcheck <- FALSE
    message(paste0(msg, collapse = '\n'))
  }
  validcheck
})

setMethod('summary','optimConfig', function(object){

  sNames <- slotNames(object)[slotNames(object) != 'roster_key']
  roster_key <- object@roster_key
  cat('Configuration Details:\n')
  sapply(sNames, function(Z){cat(paste0("  ", Z, ": ", slot(object, Z),"\n"))})

  X <- do.call(rbind,
          lapply(names(roster_key), function(K){
            curr <- roster_key[[K]]
            data.frame(`Roster Position` = K,
                       `Eligible Positions` = paste(roster_key[[K]]$positions, collapse = ","),
                       `Number of Slots` = roster_key[[K]]$num,
                       check.names = FALSE)
          }))
  cat("\nLineup Roster Summary:\n")
  print(X, row.names=FALSE)
  return(invisible(NULL))
})

#######################
#     Subclassing     #
#######################
# Site, sport and Contest Type specific
setClass('optimSingleGameConfig', contains = 'optimConfig',
         slots = list(multiplier_mode = 'logical',
                      multiplier_name = 'character'))

##### DRAFTKINGS #####
# Hockey, Classic
setClass('draftkingsHockeyClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 50000,
           roster_size = 9L,
           min_team_req = 3L,
           max_players_per_team = 9L,
           max_overlap = 8,
           roster_key = list('C' = list(positions = 'C', num = 2),
                             'W' = list(positions = 'W', num = 3),
                             'D' = list(positions = 'D', num = 2),
                             'G' = list(positions = 'G', num = 1),
                             'UTIL' = list(positions = c('C','W','D'), num = 1)),
           flex_position = 'UTIL'
         ))


# Football (NFL), Classic
setClass('draftkingsFootballClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 50000,
           roster_size = 9L,
           min_team_req = 3L,
           max_players_per_team = 7L,
           max_overlap = 8,
           roster_key = list('QB' = list(positions = 'QB', num = 1),
                             'RB' = list(positions = 'RB', num = 2),
                             'WR' = list(positions = 'WR', num = 3),
                             'TE' = list(positions = 'TE', num = 1),
                             'FLEX' = list(positions = c('RB','WR','TE'), num = 1),
                             'DST' = list(positions = 'DST', num = 1)),
           flex_position = 'FLEX'
         ))


# Golf, Classic
setClass('draftkingsGolfClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 50000,
           roster_size = 6L,
           min_team_req = 1L,
           max_players_per_team = 6L,
           max_overlap = 5,
           roster_key = list('G' = list(positions = 'G', num = 6)),
           flex_position = NA_character_
         ))


# Basketball, Classic
setClass('draftkingsBasketballClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 50000,
           roster_size = 8L,
           min_team_req = 2L,
           max_players_per_team = 7L,
           max_overlap = 7,
           roster_key = list('PG' = list(positions = 'PG', num = 1),
                             'SG' = list(positions = 'SG', num = 1),
                             'SF' = list(positions = 'SF', num = 1),
                             'PF' = list(positions = 'PF', num = 1),
                             'C' = list(positions = 'C', num = 1),
                             'G' = list(positions = c('PG','SG'), num = 1),
                             'F' = list(positions = c('SF', 'PF'), num = 1),
                             'UTIL' = list(positions = c('PG','SG','SF','PF','C'), num = 1)),
           flex_position = 'UTIL'
         ))


# Nascar, Classic
setClass('draftkingsNascarClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 50000,
           roster_size = 6L,
           min_team_req = 1L,
           max_players_per_team = 6L,
           max_overlap = 5,
           roster_key = list('D' = list(positions = 'G', num = 6)),
           flex_position = NA_character_
         ))


# Baseball, Classic
setClass('draftkingsBaseballClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 50000,
           roster_size = 10L,
           min_team_req = 3L,
           max_players_per_team = 9L,
           max_overlap = 9,
           roster_key = list('P' = list(positions = 'P', num = 2),
                             'C' = list(positions = 'C', num = 1),
                             '1B' = list(positions = '1B', num = 1),
                             '2B' = list(positions = '2B', num = 1),
                             '3B' = list(positions = '3B', num = 1),
                             'SS' = list(positions = 'SS', num = 1),
                             'OF' = list(positions = 'OF', num = 3)),
           flex_position = NA_character_
         ))

# WNBA, Classic
setClass('draftkingsWnbaClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 50000,
           roster_size = 6L,
           min_team_req = 3L,
           max_players_per_team = 5L,
           max_overlap = 5,
           roster_key = list('G' = list(positions = 'G', num = 2),
                             'F' = list(positions = 'F', num = 3),
                             'UTIL' = list(positions = c('G','F'), num = 1)),
           flex_position = 'UTIL'
         ))


# Soccer, Classic
setClass('draftkingsSoccerClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 50000,
           roster_size = 8L,
           min_team_req = 3L,
           max_players_per_team = 7L,
           max_overlap = 7,
           roster_key = list('GK' = list(positions = 'GK', num = 1),
                             'D' = list(positions = 'D', num = 2),
                             'M' = list(positions = 'M', num = 2),
                             'F' = list(positions = 'F', num = 2),
                             'UTIL' = list(positions = c('D','M','F'), num = 1)),
           flex_position = 'UTIL'
         ))


## Hockey, Single Game
setClass('draftkingsHockeyShowdownConfig',
         contains = 'optimSingleGameConfig',
         prototype = list(
           budget = 50000,
           roster_size = 6L,
           min_team_req = 2L,
           max_players_per_team = 5L,
           max_overlap = 5,
           roster_key = list(
             'CPT' = list(positions = c('C','W','D','G'), num = 1),
             'FLEX' = list(positions = c('C','W','D','G'), num = 5)),
           flex_position = 'FLEX',
           multiplier_name = 'CPT',
           multiplier_mode = TRUE
         ))


## NFL, Single Game
setClass('draftkingsFootballShowdownConfig',
         contains = 'optimSingleGameConfig',
         prototype = list(
           budget = 50000,
           roster_size = 6L,
           min_team_req = 2L,
           max_players_per_team = 5L,
           max_overlap = 5,
           roster_key = list(
             'CPT' = list(positions = c('QB','WR','RB','TE','K','DST'), num = 1),
             'FLEX' = list(positions = c('QB','WR','RB','TE','K','DST'), num = 5)),
           flex_position = 'FLEX',
           multiplier_name = 'CPT',
           multipler_mode = TRUE
         ))


# Baseball, Single Game
setClass('draftkingsBaseballShowdownConfig',
         contains = 'optimSingleGameConfig',
         prototype = list(
           budget = 50000,
           roster_size = 6L,
           min_team_req = 2L,
           max_players_per_team = 5L,
           max_overlap = 5,
           roster_key = list(
             'Captain' = list(positions = c('P','C','1B','2B','3B','SS','OF'), num = 1),
             'UTIL' = list(positions = c('P','C','1B','2B','3B','SS','OF'), num = 5)),
           flex_position = 'UTIL',
           multiplier_name = 'Captain',
           multipler_mode = TRUE
         ))


# Basketball (NBA), Single Game
setClass('draftkingsBasketballShowdownConfig',
         contains = 'optimSingleGameConfig',
         prototype = list(
           budget = 50000,
           roster_size = 6L,
           min_team_req = 2L,
           max_players_per_team = 5L,
           max_overlap = 5,
           roster_key = list(
             'Captain' = list(positions = c('PG','SG','SF','PF','C'), num = 1),
             'UTIL' = list(positions = c('PG','SG','SF','PF','C'), num = 5)),
           flex_position = 'UTIL',
           multiplier_name = 'Captain',
           multipler_mode = TRUE
         ))


# Soccer, Single Game
setClass('draftkingsSoccerShowdownConfig',
         contains = 'optimSingleGameConfig',
         prototype = list(
           budget = 50000,
           roster_size = 6L,
           min_team_req = 2L,
           max_players_per_team = 5L,
           max_overlap = 5,
           roster_key = list(
             'Captain' = list(positions = c('GK','D','M','F'), num = 1),
             'FLEX' = list(positions = c('GK','D','M','F'), num = 5)),
           flex_position = 'UTIL',
           multiplier_name = 'Captain',
           multipler_mode = TRUE
         ))


##### YAHOO #####
# Hockey, Classic
setClass('yahooHockeyClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 200,
           roster_size = 9L,
           min_team_req = 3L,
           max_players_per_team = 6L,
           max_overlap = 8,
           roster_key =list('C' = list(positions = 'C', num = 2),
                            'W' = list(positions = 'W', num = 3),
                            'D' = list(positions = 'D', num = 2),
                            'G' = list(positions = 'G', num = 2)),
           flex_position = NA_character_
         ))


# Golf, Classic
setClass('yahooGolfClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 200,
           roster_size = 6L,
           min_team_req = 1L,
           max_players_per_team = 9L,
           max_overlap = 5,
           roster_key =list('G' = list(positions = 'G', num = 6)),
           flex_position = NA_character_
         ))


# Basketball, Classic
setClass('yahooBasketballClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 200,
           roster_size = 8L,
           min_team_req = 3L,
           max_players_per_team = 6L,
           max_overlap = 7,
           roster_key = list('PG' = list(positions = 'PG', num = 1),
                             'SG' = list(positions = 'SG', num = 1),
                             'SF' = list(positions = 'SF', num = 1),
                             'PF' = list(positions = 'PF', num = 1),
                             'C' = list(positions = 'C', num = 1),
                             'G' = list(positions = c('PG','SG'), num = 1),
                             'F' = list(positions = c('SF', 'PF'), num = 1),
                             'UTIL' = list(positions = c('PG','SG','SF','PF','C'), num = 1)),
           flex_position = 'UTIL'
         ))


##### Fanduel #####
# Hockey, Classic
setClass('fanduelHockeyClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 55000,
           roster_size = 9L,
           min_team_req = 3L,
           max_players_per_team = 8L,
           max_overlap = 8,
           roster_key = list('C' = list(positions = 'C', num = 2),
                             'W' = list(positions = 'W', num = 4),
                             'D' = list(positions = 'D', num = 2),
                             'G' = list(positions = 'G', num = 1)),
           flex_position = NA_character_
         ))


# Football, Classic
setClass('fanduelFootballClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 60000,
           roster_size = 9L,
           min_team_req = 3L,
           max_players_per_team = 8L,
           max_overlap = 8,
           roster_key = list('QB' = list(positions = 'QB', num = 1),
                             'RB' = list(positions = 'RB', num = 2),
                             'WR' = list(positions = 'WR', num = 3),
                             'TE' = list(positions = 'TE', num = 1),
                             'DE' = list(positions = 'DE', num = 1),
                             'FLEX' = list(positions = c('RB','WR','TE'), num = 1)),
           flex_position = 'FLEX'
         ))


# Golf, Classic
setClass('fanduelGolfClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 60000,
           roster_size = 6L,
           min_team_req = 1L,
           max_players_per_team = 9L,
           max_overlap = 5,
           roster_key = list('G' = list(positions = 'G', num = 6)),
           flex_position = NA_character_
         ))


# Basketball, Classic
setClass('fanduelBasketballClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 60000,
           roster_size = 9L,
           min_team_req = 3L,
           max_players_per_team = 4L,
           max_overlap = 8,
           roster_key = list('PG' = list(positions = 'PG', num = 2),
                             'SG' = list(positions = 'SG', num = 2),
                             'SF' = list(positions = 'SF', num = 2),
                             'PF' = list(positions = 'PF', num = 2),
                             'C' = list(positions = 'C', num = 1)),
           flex_position = NA_character_
         ))


# Nascar, Classic
setClass('fanduelNascarClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 50000,
           roster_size = 5L,
           min_team_req = 1L,
           max_players_per_team = 5L,
           max_overlap = 4,
           roster_key = list('D' = list(positions = 'G', num = 5)),
           flex_position = NA_character_
         ))


###
