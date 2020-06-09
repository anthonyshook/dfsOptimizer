
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

#' Big thing that would be great
#' - Make a CUSTOM class that lets people basically do whatever they want by creating a config
#'   for a specific sport.  Effectively extending the class, here.
#'   -- We can already handle imports here by doing specific data.frame imports,
#'      but sites that we cover that have import methods should be handled already, too
#'   -- Basically it just requires instructions for what to do with the classes, and how
#'      to deal with inheritance relative to specific configs / actions / etc.


#' NICE TO HAVE IMPROVEMENTS
#' - Fix Date extraction in get_player_data and inclusion in add_players_from_df
#' - Add functionality to include predefined lineups (Mostly useful when randomness is included, or the model will likely produce exactly the same values).
#' - Add grouped-stack constraints (Line matching for hockey, depth order for baseball...) [would be easier with sport/site/contest_type based subclasses?]
#'   - Could see this as a V1.2...
#' - The way we adapt budget to singlegame/showdown mode _currently_ uses an `ifelse` but it's fragile-ish.
#'        (Would be better if the construction process was at the method level -- AFAIK, only budget and pts differ in construction, everything
#'        else is config-controlled already)


#' TEAM STACK IMPROVEMENTS (longer term)
#' - Add 'teams' as optional in case people want to omit certain teams from stack eligibility
#' - Add an `add_multistack` function with an API that allows team-specific stacks, and requires they ALL be met.
#'   e.g, list(COL=c('C','W'), SJS = c('D','D')) would give you both stacks. Using just one position is also a way to
#'   ensure that a position from a specific team gets pulled, like list(COL=c('C','W'), SJS = c('D')) ensures you get
#'   at least one of the SJS D (tho you may be better off locking a player)
