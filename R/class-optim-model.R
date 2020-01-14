
# Necessary for @mod to not complain
setOldClass('milp_model')

#' S4 Class of Optimization Model
#'
#' @slot mod The model object
#' @slot solver Method for solving; currently only 'glpk' is available
#' @slot flex_positions Character array of flex positions
#'
#' @export
optim_model <- setClass('optimModel',
                        slots = list(
                          mod = 'milp_model',
                          solver = 'character',
                          flex_positions = 'character'
                        ),
                        prototype = list(
                          mod = ompr::MILPModel(),
                          solver = 'glpk',
                          flex_positions = NA_character_
                          ))

setMethod('show','optimModel', function(object) {
  cat(paste('Solver:', object@solver, '\n'))
  show(object@mod)
  })
