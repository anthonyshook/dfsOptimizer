
##### RELEASE MILESTONES #####
# When complete, move on to tests, docs, vignettes, etc.

#' TO DO
#' - Test SHOWDOWN mode (the objective will have to change, for CAPTAIN mode. (value[1] * 1.5 + value[2:6] * 1), && budget[1]*1.5 + budget[2:6] * 1)
#'    - NEXT -- model builder, optimizer sub-classes, and config sub-classes
#'    - [NEED TO HAVE SingleGame METHOD FOR UPDATE_OBJECTIVE]
#'    - ANNOYING PART -- budget _currently_ uses an ifelse but it's fragile-ish.
#'        (Would be better if the construction process was at the method level -- AFAIK, only budget and pts differ in construction, everything
#'        else is config-controlled already)
#'    - Also TO-DO: Lineup ordering logic (show Captain and the multiplier salary and pts)
#' - Methods for Setting config fields?  (STILL NEED TO CONSIDER THE API - slightly less important now)
#'   - using optimizer methods ensures pipe-ability (e.g., function(opt, args) can be `opt %>% function(args)`)
#'   - I'm feeling like using BOTH APIs is potentially very useful (for those who like to pipe and those who do not)
#' - Build LINEUP class to manage lineup objects [determine if necessary... useful for passing back to model]



##### FUTURE MILESTONES #####
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

#' TEAM STACK IMPROVEMENTS
#' - Add 'teams' as optional in case people want to omit certain teams from stack eligibility
#' - Add an `add_multistack` function with an API that allows team-specific stacks, and requires they ALL be met.
#'   e.g, list(COL=c('C','W'), SJS = c('D','D')) would give you both stacks. Using just one position is also a way to
#'   ensure that a position from a specific team gets pulled, like list(COL=c('C','W'), SJS = c('D')) ensures you get
#'   at least one of the SJS D (tho you may be better off locking a player)
