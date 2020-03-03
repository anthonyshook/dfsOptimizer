# Necessary for @mod to not complain
setOldClass('milp_model')

#' S4 Class of object Optimizer
#'
#' @slot site The site being used for optimization
#' @slot sport The sport being optimized
#' @slot contest_type The type of contest; determines base constraints (e.g., Classic, Showdown/Single-Game)
#' @slot players List of players to build lineups from
#' @slot model The optimization model
#' @slot config An optim config class, which contains information about the model, including additional constraints
#' @slot maximize Logical, whether to maximize or minimize the objective function
#'
#' @export
#'
#' @include class-config.R class-player.R lineup-utils.R
.optimizer <- setClass(Class = 'optimizer',
                       slots = list(
                         site = 'character',
                         sport = 'character',
                         contest_type = 'character',
                         players = 'list',
                         model = 'milp_model',
                         config = 'optimConfig',
                         maximize = 'logical'
                       ),
                       prototype = list(
                         model = ompr::MILPModel(),
                         maximize = TRUE
                       ))

#' Create an object of Optimizer
#'
#' @param site The site being used for optimization
#' @param sport The sport being optimized
#' @param contest_type The type of contest; determines base constraints (e.g., Classic, Showdown/Single-Game). Default: CLASSIC
#' @param players List of players to build lineups from (defaults to empty list)
#' @param maximize Logical, whether to maximize or minimize the objective function (Defaults to TRUE)
#'
#' @details This function is used to instantiate a new object of class \code{optimizer}, which is the
#'     central component of the dfsOptimizer package.
#'
#' @examples
#' mod <- create_optimizer(site = 'DRAFTKINGS', sport = 'HOCKEY')
#'
#' @export
create_optimizer <- function(site,
                             sport,
                             contest_type = 'CLASSIC',
                             players = list(),
                             maximize = TRUE) {

  site         <- toupper(site)
  sport        <- toupper(sport)
  contest_type <- toupper(contest_type)

  # Get base config
  cfg <- base_settings[[site]][[sport]][[contest_type]]

  # Check that config is really real
  if (length(cfg) == 0 || is.null(cfg)) {
    stop('Configuration for Site + Sport + Contest Type not implemented!')
  }

  # Making configuration, to begin with
  modConfig <- .optimConfig(budget = cfg$budget,
                            roster_size = as.integer(cfg$roster),
                            min_team_req = as.integer(cfg$min_team_req),
                            max_players_per_team = as.integer(cfg$max_players_per_team),
                            roster_key = cfg$roster_key,
                            flex_position = cfg$flex_position,
                            max_exposure = 1,
                            variance = 0,
                            constraints = list())

  # Adding to optimizer class
  # Defaults to an 'empty' MILPmodel
  o <- .optimizer(site = site,
                  sport = sport,
                  contest_type = contest_type,
                  players = players,
                  config = modConfig,
                  maximize = maximize)
  return(o)
}

# Base Methods
setMethod('show', 'optimizer', function(object) {
  cat(
    paste0('Optimizer Object (S4 class)\n',
           'Site: ', object@site, '\n',
           'Sport: ', object@sport, '\n',
           'Contest Type: ', object@contest_type, '\n',
           'Number of Players: ', length(object@players)
    )
  )
})

