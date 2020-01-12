# Place for utility functions
get_model_length <- function(ompr_model, variable) {
  nrow(ompr_model$variables[[variable]]$index_mapping)
}

mask_func <- function(i, data) {
  indx <- ifelse(data == unique(data)[i], 1, 0)
  return(indx)
}
