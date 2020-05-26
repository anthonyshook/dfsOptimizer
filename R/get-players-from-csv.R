# Methods on Optimizer sub-classes by Site


setGeneric('get_players_from_csv', function(object, path) standardGeneric('get_players_from_csv'))

#' Create Players from CSV
#'
#' @param object Optimizer object
#' @param path Path to CSV or text file
#'
#' @details Reads a CSV from a specified site, and generates objects of Player class (one per row)
#'
#' @include get-players-from-df.R
#'
#' @aliases get_players_from_csv
#'
#' @export
setMethod('get_players_from_csv', 'DraftkingsOptim',
          function(object, path) {
            dat <- data.table::fread(path, stringsAsFactors = FALSE)
            players <- lapply(seq_len(nrow(dat)),
                              function(row){
                                curr <- dat[row, ]
                                # Split the name, check encoding
                                Encoding(curr$Name) <- 'UTF-8'
                                name_split <- unlist(stringr::str_split(curr$Name, " ", n = 2))

                                # Game Info
                                gi <- parse_game_info(curr$`Game Info`)

                                # This fix is for hockey (LW/)
                                curr$Position <- gsub(pattern = 'LW|RW', replacement = 'W', x = curr$Position)

                                # Make player object
                                # Uses avg points as fpts at the moment -- need easy way to replace those.
                                pl <- player(id = curr$ID,
                                             first_name = name_split[1],
                                             last_name = name_split[2],
                                             team = curr$TeamAbbrev,
                                             position = curr$Position,
                                             salary = curr$Salary,
                                             fpts = curr$AvgPointsPerGame,
                                             game_info = gi)
                              })

            # Name the list elements
            for (i in 1:length(players)){
              names(players)[i] <- id(players[[i]])
            }

            return(players)
          })


#' Create Players from CSV
#'
#' @param object Optimizer object
#' @param path Path to CSV or text file
#'
#' @details Reads a CSV from a specified site, and generates objects of Player class (one per row)
#'
#' @include get-players-from-df.R
#'
#' @rdname get_players_from_csv
#'
#' @export
setMethod('get_players_from_csv', 'YahooOptim',
          function(object, path) {
            dat <- data.table::fread(path, stringsAsFactors = FALSE)
            players <- lapply(seq_len(nrow(dat)),
                              function(row){
                                curr <- dat[row, ]
                                # Game Info
                                gi <- parse_game_info(paste(curr$`Game`, curr$Time))

                                # This fix is for hockey (LW/)
                                curr$Position <- gsub(pattern = 'LW|RW', replacement = 'W', x = curr$Position)

                                # Make player object
                                # Uses avg points as fpts at the moment -- need easy way to replace those.
                                pl <- player(id = curr$ID,
                                             first_name = curr$`First Name`,
                                             last_name = curr$`Last Name`,
                                             team = curr$Team,
                                             position = curr$Position,
                                             salary = curr$Salary,
                                             fpts = curr$FPPG,
                                             is_injured = curr$`Injury Status` != "",
                                             game_info = gi)
                              })

            # Name the list elements
            for (i in 1:length(players)){
              names(players)[i] <- id(players[[i]])
            }

            return(players)
          })


#' Create Players from CSV
#'
#' @param object Optimizer object
#' @param path Path to CSV or text file
#'
#' @details Reads a CSV from a specified site, and generates objects of Player class (one per row)
#'
#' @include get-players-from-df.R
#'
#' @rdname get_players_from_csv
#'
#' @export
setMethod('get_players_from_csv', 'FanduelOptim',
          function(object, path) {
            dat <- data.table::fread(path, stringsAsFactors = FALSE)
            players <- lapply(seq_len(nrow(dat)),
                              function(row){
                                curr <- dat[row, ]

                                # Game Info
                                gi <- parse_game_info(curr$`Game`)

                                # This fix is for hockey (LW/)
                                curr$Position <- gsub(pattern = 'LW|RW', replacement = 'W', x = curr$Position)

                                # Make player object
                                # Uses avg points as fpts at the moment -- need easy way to replace those.
                                pl <- player(id = curr$Id,
                                             first_name = curr$`First Name`,
                                             last_name = curr$`Last Name`,
                                             team = curr$Team,
                                             position = curr$Position,
                                             salary = curr$Salary,
                                             fpts = curr$FPPG,
                                             is_injured = curr$`Injury Indicator` != "",
                                             game_info = gi)
                              })

            # Name the list elements
            for (i in 1:length(players)){
              names(players)[i] <- id(players[[i]])
            }

            return(players)
          })

# setMethod('parse_input_csv', 'Custom',function(object, dat) {
#   get_players_from_data_frame(dat)
# })

#' Function to parse string for game info (Internal)
#'
#' @param str A String containing game info
#'
#' @details Used internally to aid player creation
#'
#' @keywords internal
parse_game_info <- function(str) {

  # If Final or In Progress...
  if (str == 'Final' || str == 'In Progress' || is.na(str)) {
    return(gameInfo(home_team = "",
                    away_team = "",
                    shortname = str,
                    is_started = TRUE))
  }

  teams <- strsplit(stringr::str_extract(str, "\\w{2,3}@\\w{2,3}"), "@")
  home_team <- teams[[1]][2]
  away_team <- teams[[1]][1]

  # Conversion
  starts_at <- stringr::str_split(string = str,
                                  pattern = " ",
                                  n = 2,
                                  simplify = TRUE)[2]

  # Try converting to time
  is_started <- FALSE
  tryCatch({
    starts_at <- as.POSIXct(lubridate::as_datetime(starts_at,
                                                   format = '%m/%d/%Y %I:%M %p',
                                                   tz = Sys.timezone()))
    is_started <- starts_at <= Sys.time()
  }, error = function(e){})

  return(
    gameInfo(home_team = home_team,
             away_team = away_team,
             shortname = str,
             start_time = starts_at,
             is_started = is_started)
  )

}
