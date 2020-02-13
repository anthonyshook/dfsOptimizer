#' Reorder lineups
#'
#' @param lineup the lineup output
#' @param new_order The desired ordered output of the table (usually a character vector of positions equal to the nrow(lineup))
#'
#' @keywords internal
reorder_lineup <- function(lineup, new_order) {

  # add a row for countin'
  lineup$posindex <- NA

  for (O in unique(new_order)) {
    order_index  <- which(O == new_order)
    lineup_index <- which(O == lineup$position)

    first_matches <- lineup_index[1:length(order_index)]

    lineup$posindex[first_matches] <- order_index
  }

  # There will be NAs for UTIL/FLEX
  na_loc <- setdiff(1:length(new_order), lineup$posindex)
  lineup$posindex[is.na(lineup$posindex)] <- na_loc

  lineup <- lineup[order(posindex)]
  lineup$posindex <- NULL
  lineup$roster_position <- new_order

  return(lineup)
}
