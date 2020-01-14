
# Necessary for @mod to not complain
setOldClass('milp_model')

#' S4 Class of Optimization Model
#'
#' @slot mod The model object
#' @slot solver Method for solving; currently only 'glpk' is available
#'
#' @export
optim_model <- setClass('optimModel',
                        slots = list(
                          mod = 'milp_model',
                          solver = 'character'
                        ),
                        prototype = list(
                          mod = ompr::MILPModel(),
                          solver = 'glpk'
                        ))

setMethod('show','optimModel', function(object) {
  cat(paste('Solver:', object@solver, '\n'))
  show(object@mod)
})
