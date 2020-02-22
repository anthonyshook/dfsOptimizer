
#' TO DO
#' - Methods for Setting config fields?  (STILL NEED TO CONSIDER THE API)
#' - Add NBA
#' - Add MLB
#' - Add NFL
#' - Add NASCAR
#' - Build LINEUP class to manage lineup objects
#' - Test SHOWDOWN mode (the objective will have to change, for CAPTAIN mode. (value[1] * 1.5 + value[2:6] * 1), && budget[1]*1.5 + budget[2:6] * 1)
#' - Make all non-base constraints take the OPTIMIZER OBJECT and not the OMPR model
#' - remove the whole model@mod construct, just put 'solver' in the build_lineups with a glpk default

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
#' - Refactor the whole damn stacking code, potentially.  We may want to check out CVXR and see if that is better from a speed of construction perspective.
#'   - If there is a WAY to use the pre-defined player-stacks, that would be the ideal
#'

#' NICE TO HAVE IMPROVEMENTS
#' - fix the date parsing for non-draftkings
