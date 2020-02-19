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
    if (any(is.na(first_matches))) next

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

#' Parse roster key item
#'
#' @param rk Roster key from base_settings
#'
#' @details This function will take all single and multiplayer slots from the roster key, and
#'     build a table containing mins/maxes for each position.  These are then used to set the
#'     value of how many of each position is allowable in a lineup.
#'
#' @keywords internal
parse_roster_key <- function(rk) {

  # lapply through roster key
  by_row <- lapply(rk, function(i) {
    if (length(i$positions)==1) {
      # Set max and min to the number of players for that position
      out <- data.frame(pos = i$positions, min = i$num, max = i$num)
    } else if (length(i$positions > 1)) {
      # Set max to the number of players for those positions leaving min as 0
      out <- data.frame(pos = i$positions, min = 0, max = i$num)
    } else {
      # Break because it means there was no position
      stop("Length of 'positions' is less than 1!")
    }
  })

  # Sum the mins and maxes up by position
  sum_table <- data.table::rbindlist(by_row)[, .(min = sum(min), max = sum(max)), by = pos]

  return(sum_table)
}
