#' S4 Class For Optimizer
#'
#' @slot site The site being used for optimization
#' @slot sport The sport being optimized
#' @slot contest_type The type of contest; determines base constraints (e.g., Classic, Showdown/Single-Game)
#' @slot players List of players to build lineups from
#' @slot model The optimization model
#' @slot maximize Logical, whether to maximize or minimize the objective function
#'
.optimizer <- setClass('optimizer',
                       slots = list(
                         site = 'character',
                         sport = 'character',
                         contest_type = 'character',
                         players = 'list',
                         model = 'optim_model',
                         maximize = 'logical'
                       ),
                       prototype = list(
                         maximize = TRUE
                       ))

#' Initialization Function For Optimizer Class
#'
#' @param site The site being used for optimization
#' @param sport The sport being optimized
#' @param contest_type The type of contest; determines base constraints (e.g., Classic, Showdown/Single-Game)
#' @param players List of players to build lineups from (defaults to empty list)
#' @param model The optimization model (Defaults to empty model)
#' @param maximize Logical, whether to maximize or minimize the objective function (Defaults to TRUE)
#'
#' @export
optimizer <- function(site, sport, contest_type, players = list(), model, maximize = TRUE) {

  if (missing('model')) {
    model = optim_model()
  }

  o <- .optimizer(site = toupper(site),
                  sport = toupper(sport),
                  contest_type = toupper(contest_type),
                  players = players,
                  model = model,
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


# Methods
setGeneric("extract_player_fpts", function(object) standardGeneric("extract_player_fpts"))
#' Extracting Fantasy Points from Player objects
#'
#' @param object an optimizer object
#'
setMethod('extract_player_fpts',
          signature = 'optimizer',
          definition = function(object) {
            if (length(object@players) == 0) {
              message('No Players in optimizer!')
              out <- NULL
            } else {
              out <- sapply(object@players, fpts)
            }
            return(out)
          })



setGeneric("add_player", function(object, pl) standardGeneric("add_player"))
#' Method for adding a player to Optimizer object
#'
#' @param object an S4 optimizer object
#' @param pl an object of class Player
#'
#' @export
setMethod('add_player',
          signature = 'optimizer',
          definition = function(object, pl) {

            # Check that if player exists, they aren't added again
            if (any(sapply(object@players, identical, pl))) {
              warning('Player already exists')
            } else {
              object@players <- c(object@players, pl)
            }

            return(object)
          })


setGeneric("remove_player", function(object, id) standardGeneric("remove_player"))
#' Method for removing a player from Optimizer object
#'
#' @param object an S4 optimizer object
#' @param id ID of player to remove
#'
#' @export
setMethod('remove_player',
          signature = 'optimizer',
          definition = function(object, id) {
            index_to_remove <- which(sapply(object@players, id) == id)

            if (length(index_to_remove) == 0) {
              stop("Could not find player with ID equal to ", id)
            } else if (length(index_to_remove) > 1) {
              stop("Found more than one player with ID of ", id)
            } else {
              object@players[[index_to_remove]] <- NULL
            }
            return(object)
          })

setGeneric("update_fpts", function(object, fpts_data) standardGeneric('update_fpts'))
#' Method for updating fantasy points in an object
#'
#' @param object An object of class Optimizer
#' @param fpts_data a data.frame containing players and points. See details.
#'
#' @details The data.frame passed in fpts_data should contain two columns - \code{id} and \code{fpts}.
#'
#' @export
setMethod('update_fpts',
          signature = 'optimizer',
          definition = function(object, fpts_data){

            # Check for columns
            if (!all(c('fpts','id') %in% colnames(fpts_data))) { stop('fpts_data must have columns `fpts` and `id`')}

            # Update the data
            for (i in 1:length(fpts_data)) {
              # check for existence
              if (is.null(object@players[[fpts_data$id[i]]])) {
                break
              } else {
                object@players[[fpts_data$id[i]]]@fpts <- fpts_data$fpts[i]
              }

            }

            return(object)

          })


setGeneric('get_player_index', function(object) standardGeneric('get_player_index'))
#' Method for getting player index (Deprecated?)
#'
#' @param object an S4 object of class Optimizer
#'
#' @export
setMethod('get_player_index',
          signature = 'optimizer',
          definition = function(object){
            if (length(object@players) == 0) {
              return(NA)
            } else {
              o <- do.call('rbind',
                           lapply(seq_len(length(object@players)),
                                  function(i){
                                    data.frame(index = i ,
                                               id = id(object@players[[i]]),
                                               fullname = fullname(object@players[[i]])
                                    )
                                  }))
            }
            return(o)

          })

setGeneric('construct_model', function(object) standardGeneric('construct_model'))
#' Method for constructing the optimization model
#'
#' @param object an S4 object of class Optimizer
#'
#' @export
setMethod('construct_model',
          signature = 'optimizer',
          definition = function(object) {
            # Checks should be added, but for now

            base_config <- base_settings[[object@site]][[object@sport]][[object@contest_type]]
            if (length(base_config) == 0 || is.null(base_config)) {
              stop('Configuration for Site, Sport and Contest Type not found!')
            }

            # Start constructing the model
            object@model@mod <- build_base_model(
              size = length(object@players),
              num_groups = data.table::uniqueN(sapply(object@players, team))
              pts  = extract_player_fpts(object)
            )

            # Adding roster limit
            object@model@mod <- add_roster_size_constraint(object@model@mod, roster_limit = base_config$roster)

            # Adding budget constraint
            salaries <- sapply(object@players, function(p){p@salary})
            object@model@mod <- add_budget_constraint(object@model@mod,
                                                      player_salaries = salaries,
                                                      budget = base_config$budget)

            return(object)

          })

setGeneric('optimize', function(object, num_lineups = 1) standardGeneric('optimize'))
#' Function to Generate lineups
#'
#' @param object an S4 object of class Optimizer
#' @param num_lineups Number of lineups to generate
#'
#' @export
setMethod('optimize',
          signature = 'optimizer',
          definition = function(object, num_lineups = 1) {

            # Build a player data set
            # We can then filter from this below, where we need the relevant rows the optimizer solved for

            solution_list <- lapply(1:num_lineups, function(Z){
              fit_model <- ompr::solve_model(object@model@mod,
                                             solver = ompr.roi::with_ROI(object@model@solver))

              # Get solution index
              get_solution <- ompr::get_solution(fit_model, players[i])

              # Get just the relevant rows
            })

            return(solution_list)


          })
