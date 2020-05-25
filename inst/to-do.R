
##### RELEASE MILESTONES #####
# When complete, move on to tests, docs, vignettes, etc.

#' TO DO
#' - Expand SHOWDOWN mode
#'    - Still needs
#'      - Yahoo Football and Baseball
#'      - Fanduel Classic Baseball
#'      - All Fandual Single Game content


##### FUTURE MILESTONES #####
#' SEMI LONG-TERM TO DO
#' - write tests
#' - Add friendly error reporting for things like "You used a CSV I have never seen", etc.
#' - Refactor the whole damn stacking code, potentially.  We may want to check out CVXR and see if that is better from a speed of construction perspective.

#' Big thing that would be great
#' - Make a CUSTOM class that lets people basically do whatever they want by creating a config
#'   for a specific sport.  Effectively extending the class, here.
#'   -- We can already handle imports here by doing specific data.frame imports,
#'      but sites that we cover that have import methods should be handled already, too
#'   -- Basically it just requires instructions for what to do with the classes, and how
#'      to deal with inheritence relative to specific configs / actions / etc.


#' NICE TO HAVE IMPROVEMENTS
#' - fix the date parsing for non-draftkings
#' - Add functionality to include predefined lineups (Mostly useful when randomness is included, or the model will likely produce exactly the same values).
#' - Add grouped-stack constraints (Line matching for hockey, depth order for baseball...) [would be easier with sport/site/contest_type based subclasses?]
#'   - Could see this as a V1.2...
#' - The way we adapt budget to singlegame/showdown mode _currently_ uses an `ifelse` but it's fragile-ish.
#'        (Would be better if the construction process was at the method level -- AFAIK, only budget and pts differ in construction, everything
#'        else is config-controlled already)
#' - Position-level salary ranges? (C #1 = 8000-9000, C #2 = 4000-5000)
#'   - This is not something remotely possible at the moment. It only would be if we were slating players directly into
#'     positions within a lineup, which we are NOT -- we're just looking for the binary index.


#' TEAM STACK IMPROVEMENTS (longer term)
#' - Add 'teams' as optional in case people want to omit certain teams from stack eligibility
#' - Add an `add_multistack` function with an API that allows team-specific stacks, and requires they ALL be met.
#'   e.g, list(COL=c('C','W'), SJS = c('D','D')) would give you both stacks. Using just one position is also a way to
#'   ensure that a position from a specific team gets pulled, like list(COL=c('C','W'), SJS = c('D')) ensures you get
#'   at least one of the SJS D (tho you may be better off locking a player)
