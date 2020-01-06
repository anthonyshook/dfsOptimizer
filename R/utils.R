# Place for utility functions
get_model_length <- function(ompr_model, variable) {
  nrow(ompr_model$variables[[variable]]$index_mapping)
}
