#' S4 Class optimConfig
#'
#' @slot budget Lineup budget
#' @slot min_budget the minimum budget (Default: 0)
#' @slot roster_size Roster limitation (How many players allowed)
#' @slot min_team_req Number of teams required to be represented in the lineup
#' @slot max_players_per_team Maximum number of players from any one team
#' @slot roster_key List containing roster positions and the number of each position required
#' @slot flex_position named identifer for the flex positions
#' @slot max_exposure Maximum exposure for individual players (global)
#' @slot variance Percentage variance for fantasy points -- used to add randomness to the model.
#' @slot constraints A \code{list} containing additional constraint objects
#'
setClass('optimConfig',
         slots = list(
           budget = 'numeric',
           min_budget = 'numeric',
           roster_size = 'integer',
           min_team_req = 'integer',
           max_players_per_team = 'integer',
           roster_key = 'list',
           flex_position = 'character',
           max_exposure = 'numeric',
           variance = 'numeric',
           constraints = 'list'
         ),
         prototype = list(
           min_budget = 0,
           flex_position = NA_character_,
           max_exposure = 1,
           variance = 0
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
           if (length(object@constraints) > 0 &&
               !all(sapply(object@constraints, class) == 'constraintClass')) "Invalid object(s) found in constraints slot! List may only contain constraintClass objects",
           if (object@budget < object@min_budget) "Min budget cannot be less than max budget"
  )
  if (!is.null(msg)) {
    validcheck <- FALSE
    message(paste0(msg, collapse = '\n'))
  }
  validcheck
})

#######################
#     Subclassing     #
#######################
# Site, sport and Contest Type specific

##### DRAFTKINGS #####
# Hockey, Classic
setClass('draftkingsHockeyClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 50000,
           roster_size = 9L,
           min_team_req = 3L,
           max_players_per_team = 9L,
           roster_key = list('C' = list(positions = 'C', num = 2),
                             'W' = list(positions = 'W', num = 3),
                             'D' = list(positions = 'D', num = 2),
                             'G' = list(positions = 'G', num = 1),
                             'UTIL' = list(positions = c('C','W','D'), num = 1)),
           flex_position = 'UTIL'
         ))


# Golf, Classic
setClass('draftkingsGolfClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 50000,
           roster_size = 6L,
           min_team_req = 1L,
           max_players_per_team = 6L,
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
           max_players_per_team = 5L,
           roster_key = list('P' = list(positions = 'P', num = 2),
                             'C' = list(positions = 'C', num = 1),
                             '1B' = list(positions = '1B', num = 1),
                             '2B' = list(positions = '2B', num = 1),
                             '3B' = list(positions = '3B', num = 1),
                             'SS' = list(positions = 'SS', num = 1),
                             'OF' = list(positions = 'OF', num = 3)),
           flex_positions = NA_character_
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
           max_players_per_team = 9L,
           roster_key = list('C' = list(positions = 'C', num = 2),
                             'W' = list(positions = 'W', num = 4),
                             'D' = list(positions = 'D', num = 2),
                             'G' = list(positions = 'G', num = 1)),
           flex_position = NA_character_
         ))


# Golf, Classic
setClass('fanduelGolfClassicConfig',
         contains = 'optimConfig',
         prototype = list(
           budget = 60000,
           roster_size = 6L,
           min_team_req = 1L,
           max_players_per_team = 9L,
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
           roster_key = list('D' = list(positions = 'G', num = 5)),
           flex_position = NA_character_
         ))



