
#' TO DO
#' - Methods for adding all the existing constraints!
#' - Methods for Setting config fields?
#' - FanDuel Import function
#' - Yahoo Import Function
#' - Import from data.frame function
#' - Formatting for Lineup output (DK, FD, and Yahoo)
#' - Add NBA
#' - Add MLB
#' - Test SHOWDOWN mode (the objective will have to change, for CAPTAIN mode. (value[1] * 1.5 + value[2:6] * 1), && budget[1]*1.5 + budget[2:6] * 1)
#' - Consider replacing the current flex-positions process with a UTIL/FLEX position process that is more easily fixed.
#'   - TRIED. doesn't work ATM because our position constraint is based on positions, so you can't have the final UTIL position == 1 if it's made up of the SUM of all others.  The way we're currently designed, it only works if UTIL is a specific position. NOTE -- you don't need position constraints if you have specific positions...
#' - Consider replacing all the setter functions with straight `fnc<-` methods

#' FEATURES TO ADD
#' - Add Opposing -positive- constraints (if team A QB, then team B WR) [force_opposing_positions]
#' - Add grouped-stack constraints (Line matching for hockey, depth order for baseball...)
#' - Add method for specifying optional stack elements (like QB and *ONE OF* WR / TE)
#' - Minimum Budget Constraint
#' - Max repeating players (code exists, function to set it does not)


#' SEMI LONG-TERM TO DO
#' - write tests
#' - Vignettes
#' - Updated /cleaned docs
#' - Validity checks to objects
