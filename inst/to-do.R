
#' BUG
#'
#' - cases where players have multiple possible positions (like PF or C in basketball) currently break things.
#'   - we're going to need a way to fix that here
#'   - option 1: Everybody with multiple positions gets multiple entries in player list (not ideal)
#'   - option 2: When making masks for player positions, instead of looking for ==, we split the position by '/' and call any(...) == pos on the split
#'     - This probably will work, but may cause something else to explode??

#' TO DO
#' - Methods for Setting config fields?  (STILL NEED TO CONSIDER THE API)
#' - Add NBA
#' - Add MLB
#' - Add NFL
#' - Add NASCAR
#' - Build LINEUP class to manage lineup objects
#' - Test SHOWDOWN mode (the objective will have to change, for CAPTAIN mode. (value[1] * 1.5 + value[2:6] * 1), && budget[1]*1.5 + budget[2:6] * 1)
#' - Consider replacing the current flex-positions process with a UTIL/FLEX position process that is more easily fixed.
#'   - TRIED. doesn't work ATM because our position constraint is based on positions, so you can't have the final UTIL position == 1 if it's made up of the SUM of all others.  The way we're currently designed, it only works if UTIL is a specific position. NOTE -- you don't need position constraints if you have specific positions...

#' FEATURES TO ADD
#' - Add Opposing -positive- constraints (if team A QB, then team B WR) [force_opposing_positions]
#' - Add grouped-stack constraints (Line matching for hockey, depth order for baseball...)
#' - Add method for specifying optional stack elements (like QB and *ONE OF* WR / TE -- possibly changing the input structure from single vector to list)
#' - Minimum Budget Constraint
#' - Max repeating players (code exists, function to set it does not)
#' - Consider adding variance as a player-level option (perhaps with min/max variance options.)

#' SEMI LONG-TERM TO DO
#' - write tests
#' - Vignettes
#' - Updated /cleaned docs
#' - Validity checks to objects
#' - Add other sites / contest types
#' - Add friendly error reporting for things like "You used a CSV I have never seen", etc.

#' NICE TO HAVE IMPROVEMENTS
#' - fix the date parsing for non-draftkings
