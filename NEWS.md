# dfsOptimizer

### 1.4.1 - November 2021
* Fixed a bug where minimum exposure player constraints were not being honored.

### 1.4.0 - February 2021
* Added `compare_lineups_with_actuals` method to make experimentation easier.  Method can be used to determine how changing specific parameters would have played out in a given slate or contest.

### 1.3.0 - January 2021
* Reduced variable and constraint construction, which should allow for the use of CPLEX Community Edition (which has a 1000 variable/constraint limit) for certain site, sport, constraint combinations.
* Updated the FanDuel Hockey config to account for 2021 changes to the lineup structure. 
* Added ability to include previously generated models in `build_lineups`
* Added 'within_lines' option to add_team_stack, which allows users building HOCKEY models to create stacks within lines, as defined by players with matching "Depth" values.
* Fixed scope warning
* Added set-max-budget method
* Added set-max-teams method
* Updated vignettes

### 1.0.0 - May 2020
* Initial Release

