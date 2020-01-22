
#' TO DO
#' - FanDuel Import function
#' - Yahoo Import Function
#' - Formatting for Lineup output (DK, FD, and Yahoo)
#' - Add NBA
#' - Add MLB
#' - Test SHOWDOWN mode (the objective will have to change, for CAPTAIN mode. (value[1] * 1.5 + value[2:6] * 1), && budget[1]*1.5 + budget[2:6] * 1)
#' - Consider replacing the current flex-positions process with a UTIL/FLEX position process that is more easily fixed.
#'   - TRIED. doesn't work ATM because our position constraint is based on positions, so you can't have the final UTIL position == 1 if it's made up of the SUM of all others.  The way we're currently designed, it only works if UTIL is a specific position.

#' FEATURES TO ADD
#' - Add Opposing Position constraints (no Pitchers against batter, no goalies against players, etc.)
#' - Add Opposing -positive- constraints (if team A QB, then team B WR)
#' - Add stack constraints (Line matching for hockey, depth order for baseball...)


#' SEMI LONG-TERM TO DO
#' - write tests
