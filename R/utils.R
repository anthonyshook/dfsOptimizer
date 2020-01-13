# Place for utility functions
get_model_length <- function(ompr_model, variable) {
  nrow(ompr_model$variables[[variable]]$index_mapping)
}

# Masking function for model
mask_func <- function(i, data) {
  indx <- ifelse(data == unique(data)[i], 1, 0)
  return(indx)
}

# calculate exposure
calculate_exposure <- function(veclist) {

  # Calculates the sum of vectors
  sum_vecs <- rowSums(do.call('cbind', veclist))

  # Calculates what it WOULD be if that user were selected
  exposure <- (sum_vecs + 1) / (length(veclist) + 1)

}
