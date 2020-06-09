# Necessary for @mod to not complain
setOldClass('milp_model')

#' S4 Class of object Optimizer
#'
#' @slot site The site being used for optimization
#' @slot sport The sport being optimized
#' @slot contest_type The type of contest; determines base constraints (e.g., Classic, Showdown/Single-Game)
#' @slot players List of players to build lineups from
#' @slot model The optimization model
#' @slot config An optim config class, which contains information about the model and how to build it
#' @slot constraints A \code{list} containing additional constraint objects
#'
#' @export
#'
#' @include class-config.R class-player.R lineup-utils.R
setClass(Class = 'optimizer',
         slots = list(
           site = 'character',
           sport = 'character',
           contest_type = 'character',
           players = 'list',
           model = 'milp_model',
           config = 'optimConfig',
           constraints = 'list'
         ),
         prototype = list(
           model = ompr::MILPModel()
         ),
         contains = 'VIRTUAL')


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


## Site Sub-classes
# (Manages input/output of data)
setClass(Class = 'DraftkingsOptim', contains = 'optimizer', prototype = list(site = 'DRAFTKINGS'))
setClass(Class = 'FanduelOptim', contains = 'optimizer', prototype = list(site = 'FANDUEL'))
setClass(Class = 'YahooOptim', contains = 'optimizer', prototype = list(site = 'YAHOO'))

## Contest Type Sub-classes
setClass(Class = 'ClassicOptim', contains = 'optimizer')
setClass(Class = 'SingleGameOptim', contains = 'optimizer')

# Classic
setClass(Class = 'DraftkingsClassicOptim', contains = c('DraftkingsOptim', 'ClassicOptim'))
setClass(Class = 'FanduelClassicOptim', contains = c('FanduelOptim', 'ClassicOptim'))
setClass(Class = 'YahooClassicOptim', contains = c('YahooOptim', 'ClassicOptim'))

# Showdown / Captain Mode / Single Game??
setClass(Class = 'DraftkingsShowdownOptim', contains = c('DraftkingsOptim', 'SingleGameOptim'))
#setClass(Class = 'FanDuelSingleOptim', contains = c('FanduelOptim', 'SingleGameOptim'))
#setClass(Class = 'YahooSingleOptim', contains = c('YahooOptim', 'SingleGameOptim'))


### Initialization Function
#' Create an object of Optimizer
#'
#' @param site The site being used for optimization
#' @param sport The sport being optimized
#' @param contest_type The type of contest; determines base constraints (e.g., Classic, Showdown/Single-Game). Default: CLASSIC
#' @param players List of players to build lineups from (defaults to empty list)
#' @param filepath Alternate method for adding players at time of creation. Passing a filepath will result in an internal call to
#'     \code{add_players_from_csv}. If \code{players} is provided, filepath will be ignored.
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
                             filepath = NULL) {
  # Convert to ALL CAPS
  site         <- toupper(site)
  sport        <- toupper(sport)
  contest_type <- toupper(contest_type)

  # Creating correct configuration
  modConfig <- tryCatch(new(Class = get_correct_config(site = site, sport = sport, contest_type = contest_type)),
                        error = function(e){stop('Configuration for ', site,' / ', sport, ' / ', contest_type, ' not implemented!\n')})

  # Adding to optimizer class
  # Defaults to an 'empty' MILPmodel
  class_name <- paste0(capitalize(site), capitalize(contest_type), 'Optim')
  o <- new(Class = class_name,
           site = site,
           sport = sport,
           contest_type = contest_type,
           config = modConfig)

  # Add players
  if (length(players) == 0 &&
      !is.null(filepath)) {
    o <- add_players_from_csv(o, filepath)
  }
  return(o)
}


