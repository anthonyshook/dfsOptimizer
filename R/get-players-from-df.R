#' Get Players from data.frame
#'
#' @param df A data.frame with columns for player related information.  This can contain any/all of the player related info.
#'    If a column coincides with a slot in objects of class \code{player_object}, it will be included when the
#'    data.frame is converted into unique player objects. The one exception is slot \code{game_info}, which is
#'    ignored in the conversion.
#' @details The following columns that are required, in order to avoid issues with the optimizer model:
#' \itemize{
#'   \item{first_name}
#'   \item{last_name}
#'   \item{team}
#'   \item{position}
#'   \item{salary}
#'   \item{fpts}
#' }
#' You'll get an error if these aren't included.  If you want to know what else you can include, run \code{list_player_attributes()}
#'
#' @return A list containing values of class \code{player_objects}, one per player (i.e., row) in the data set.
#'
#' @export
get_players_from_data_frame <- function(df) {
  # lower column names
  colnames(df) <- tolower(colnames(df))

  # Check for necessary columns
  necessary_columns <- c('first_name','last_name','team','position','salary','fpts')
  if (!all(necessary_columns %in% colnames(df))) {
    msg <- paste("The following required columns were missing from your data set:",
                 paste(setdiff(necessary_columns, colnames(df)), collapse = ', '))
    stop(msg)
  }

  # If there are factors, replace with characters...
  df <- rapply(df, as.character, classes="factor", how="replace")

  # If there is a game_info column, remove it
  df$game_info <- NULL

  # If there is no id column, add it.
  if (!('id' %in% colnames(df))) {
    df$id <- paste('id', 10000 + 1:nrow(df), sep="")
  }

  # make a fullname column
  df$fullname <- paste(df$first_name, df$last_name)

  # Lastly, we'll want to check and ensure the values are appropriate types
  # For now, that's mostly salary that turns out to be the sticky one
  df$salary <- as.integer(df$salary)

  # Create players
  player_list <- lapply(1:nrow(df), function(i) {
    return(do.call(new, c(Class = 'player_object', as.list(df[i,]))))
  })

  # Name them
  for (i in 1:length(player_list)){
    names(player_list)[i] <- id(player_list[[i]])
  }


  return(player_list)
}
