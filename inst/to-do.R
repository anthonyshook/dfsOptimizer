
#' TO DO
#' - Methods for Setting config fields?  (STILL NEED TO CONSIDER THE API)
#' -~ Add MLB (may need to wait until we can get files; * DK added (FD and YAHOO not yet available))
#' -~ Add NFL (may need to wait if I can't find old files, unless XFL does the trick)
#' - Build LINEUP class to manage lineup objects [determine if necessary... useful for passing back to model]
#' - Test SHOWDOWN mode (the objective will have to change, for CAPTAIN mode. (value[1] * 1.5 + value[2:6] * 1), && budget[1]*1.5 + budget[2:6] * 1)
#' - Make all constraints take: Optimizer object and Arguments
#'   - Interally, pass slots @players, and @model from the Optimizer object to the Constraint at application time.
#'   - This means ALL constraints should have those two input requirements
#' - Add base constraints to the constraint list of the config?? (instead of in 'construct model')


#' FEATURES TO ADD
#' - Add Opposing -positive- constraints (if team A QB, then team B WR) [force_opposing_positions]
#' - Add grouped-stack constraints (Line matching for hockey, depth order for baseball...) [would be easier with sport/site/contest_type based subclasses?]
#' - Add method for specifying optional stack elements (like QB and *ONE OF* WR / TE -- possibly changing the input structure from single vector to list)
#' - Max repeating players (code exists, function to set it does not)
#' - Consider adding variance as a player-level option (perhaps with min/max variance options.)
#' - Add functionality to include predefined lineups (Mostly useful when randomness is included, or the model will likely produce exactly the same values).

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
#' - convert the base settings to classes of objects automagically (rather than filling in config classes, just have those INHERIT based on site/sport)
