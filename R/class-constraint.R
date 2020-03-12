#' Constraint Class
#'
#' @slot constraint_name A name for the constraint (used internally)
#' @slot fnc A function to apply to the model
#' @slot args A list containing arguments to pass to the function. Do not include a model class here, that is handled by the method \code{apply_constraint}.
#'
#' @keywords internal
.constraintClass <- setClass('constraintClass',
                             slots = list(
                               constraint_name = 'character',
                               fnc = 'function',
                               args = 'list'
                             ),
                             prototype = list(
                               fnc = function(){message('Empty function')}
                             ))

# Apply method for adding constraints
setGeneric('apply_constraint', function(object, model, ...) standardGeneric('apply_constraint'))
#' Function to apply constraints to a model
#'
#' @param object The constraint object
#' @param model Model object
#' @param ... Additional arguments if required
#'
#' @return milp_model object
#'
#' @keywords internal
setMethod('apply_constraint', 'constraintClass',
          function(object, model, ...){
            args <- c(list(model = model), object@args, ...)
            new_model <- do.call(object@fnc, args)
            return(new_model)
          })

# Base Show method
setMethod('show', 'constraintClass', function(object) {
  cat(
      paste0('Constraint Class object (S4): ', object@constraint_name)
    )
  })
