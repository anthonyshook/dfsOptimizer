
### RELEASE MILESTONES
# When complete, move on to tests, docs, vignettes, etc.

#' TO DO
#' - Methods for Setting config fields?  (STILL NEED TO CONSIDER THE API -slightly less important now)
#'   - using optimizer methods ensures pipe-ability (e.g., function(opt, args) can be `opt %>% function(args)`)
#'   - I'm feeling like using BOTH APIs is potentially very useful (for those who like to pipe and those who do not)
#' - Build LINEUP class to manage lineup objects [determine if necessary... useful for passing back to model]
#' - Test SHOWDOWN mode (the objective will have to change, for CAPTAIN mode. (value[1] * 1.5 + value[2:6] * 1), && budget[1]*1.5 + budget[2:6] * 1)

#' FEATURES TO ADD
#' - Add method for specifying optional stack elements (like QB and *ONE OF* WR / TE -- possibly changing the input structure from single vector to list)
#'   - Add argument `optional positions` where, if NA you just use 'positions' and stack the full vector.  If included,
#'     you (A) require 'positions' to be length 1, (B) use another argument `num_included` (which defaults to 1) to identify
#'     how many of the `optional_positions` should be included
#'   - OR: optional positions will _always_ be "will pick one of" but it will be on-top of the previous stack.
#'        - So (positions=c('qb','wr'), optional_positions = c('WR','TE')) will be either QB-WR-WR or QB-WR-TE
#'        - if `optional_positions` is a list, like list(c('WR','TE'), c('RB','K')), it _could_ pick one from each (sets N to four, positions to those elements...)
#'        - It may, theoretically, already be set up to do this...
#'   - OR: We make an entirely different function (constr_team_stack_opt) that uses similar logic to the force opposing (but does force
#'         same team, with options.), but can put a flag in the `add_team-stack` method to determine which to use based on arguments
#'         - This could be the best way to handle it...



### FUTURE MILESTONES

#' SEMI LONG-TERM TO DO
#' - write tests
#' - Vignettes
#' - Updated /cleaned docs
#' - Validity checks to objects
#' - Add other sites / contest types
#' - Add friendly error reporting for things like "You used a CSV I have never seen", etc.
#' - Refactor the whole damn stacking code, potentially.  We may want to check out CVXR and see if that is better from a speed of construction perspective.
#'   - If there is a WAY to use the pre-defined player-stacks, that would be the ideal
#' - Make `Force Opposing` restriction capable of taking more than just one position per team

#' NICE TO HAVE IMPROVEMENTS
#' - fix the date parsing for non-draftkings
#' - Add functionality to include predefined lineups (Mostly useful when randomness is included, or the model will likely produce exactly the same values).
#' - Add grouped-stack constraints (Line matching for hockey, depth order for baseball...) [would be easier with sport/site/contest_type based subclasses?]
#'   - Could see this as a V1.2...

