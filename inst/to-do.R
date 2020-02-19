
#' TO DO
#' - Now that Roster Key Method is in place, we can remove the current flex-position process
#'   - add UTIL as a slot in base-settings, and use flex-position as field indicating which elements of the
#'     roster_key to change ('UTIL' or 'FLEX', etc.)
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

#' NICE TO HAVE IMPROVEMENTS
#' - fix the date parsing for non-draftkings
