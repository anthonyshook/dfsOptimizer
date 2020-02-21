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

# Expands grid with unique combinations
# No repeats, no repetitions
expand.grid.unique <- function(..., rep = FALSE, uniqueCombinations = TRUE) {

  # All possible elements
  dat <- data.table::CJ(..., unique = TRUE)

  # Get possible combinations
  matind     <- combn(colnames(dat), 2)
  matchcheck <- 0
  intercheck <- 0

  # This compares the content of every column
  # If EXACTLY the same sets of players are present in two columns,
  # we remove half (the upper-right corner of the pairwise matrix)
  for (I in 1:ncol(matind)) {
    i <- matind[1, I]
    j <- matind[2, I]

    # Check for fully matching columns
    if (length(setdiff(dat[[i]], dat[[j]])) == 0) {
      # If there is no difference between columns, cut half out by inequality >
      dat <- dat[dat[[j]] > dat[[i]]]
    } else if (length(intersect(dat[[i]], dat[[j]])) > 0) {
      # If there is a setdiff, AND and intersection,
      # remove repeats by intersection
      dat <- dat[dat[[i]] == intersect(dat[[i]], dat[[j]])]
    }
  }

  # Note -- if NO matches are made, we should NOT have to remove repeats
  # because the code should define it that way

  # # Removes repetitions
  # if (!rep) {
  #   dat <- dat[apply(dat, 1, data.table::uniqueN) >= ncol(dat)]
  # }
  #
  # # Reduce cases to unique combinations ()
  # if (uniqueCombinations) {
  #   dat <- dat[!duplicated(t(apply(as.matrix(dat), 1, sort))), ]
  # }

  return(dat)

}

#' make position indicator
#'
#' @param posvec vector of positions
#' @param target position to flag
make_position_indicator <- function(posvec, target) {
  which(sapply(strsplit(posvec, "/"), function(Z) any(Z %in% target)))
}

