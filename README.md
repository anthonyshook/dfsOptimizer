README
================

## Daily Fantasy Lineup Optimization in R

The `dfsOptimizer` package is intended to provide a straightforward way
to generate optimal daily fantasy lineups for various sports, sites, and
contests.

*NOTE WELL*: This is currently very much in Beta – I’m almost certainly
not yet exporting everything I need to, and new issues always crop up
once someone who isn’t actually developing the thing gets their hands on
it. So, if you decide to use it, I’d very much appreciate any bug
reports and/or suggestions\!

### Installation

``` r
devtools::install_github("anthonyshook/dfsOptimizer")
```

### To-Do

  - Add More Examples / Vignettes
  - Unit
Testing
  - CICD

## Included Sites/Sports

|        Sport         |      Draftkings      |       FanDuel        |        Yahoo         | Draftkings Single Game | FanDuel Single Game |
| :------------------: | :------------------: | :------------------: | :------------------: | :--------------------: | :-----------------: |
|   **HOCKEY (NHL)**   | :heavy\_check\_mark: | :heavy\_check\_mark: | :heavy\_check\_mark: |  :heavy\_check\_mark:  |         \-          |
|  **FOOTBALL (NFL)**  | :heavy\_check\_mark: | :heavy\_check\_mark: |          \-          |  :heavy\_check\_mark:  |         \-          |
|  **BASEBALL (MLB)**  | :heavy\_check\_mark: |          \-          |          \-          |  :heavy\_check\_mark:  |         \-          |
| **BASKETBALL (NBA)** | :heavy\_check\_mark: | :heavy\_check\_mark: | :heavy\_check\_mark: |  :heavy\_check\_mark:  |         \-          |
|       **GOLF**       | :heavy\_check\_mark: | :heavy\_check\_mark: | :heavy\_check\_mark: |           \-           |         \-          |
|      **NASCAR**      | :heavy\_check\_mark: | :heavy\_check\_mark: |          \-          |           \-           |         \-          |
|       **WNBA**       | :heavy\_check\_mark: |          \-          |          \-          |           \-           |         \-          |
|      **SOCCER**      | :heavy\_check\_mark: |          \-          |          \-          |           \-           |         \-          |

## Notes

The package relies on the `ROI` package, and comes with one solver as a
dependency – `ROI.plugin.glpk`. This is to ensure it works out of the
box, so to speak. However, the `solver` argument in `build_lineups()` is
compatible with the `ompr.roi` package, so if you have
`ROI.plugin.symphony` installed, it should be as simple as
\`build\_lineups(solver=‘symphony’)

## Simple Examples

Build Lineups from a Draftkings roster file (i.e., that you download
from DK). This example uses the mean Points-per-game provided by
draftkings as a projection, which I don’t recommend you actually do.

#### Quick Way - pass file in creation statement

``` r
# Build an optimizer
mod <- create_optimizer(site = 'DRAFTKINGS', 
                        sport = 'HOCKEY', 
                        contest_type = 'CLASSIC', 
                        filepath = '/Path/to/DKSalaries_nhl.csv')

# Build 20 lineups
lineups <- build_lineups(mod, num_lineups = 20)

# Get summary stats (player and team exposure breakdowns, and a cross-lineup similarity measure)
summary(lineups)

# See the lineups
lineups

# OR save them to a list
lineups_extracted <- as.list(lineups)

# OR export them into a format that you can upload to the selected site
export_lineups(lineups, file = '/path/to/lineup_export.csv')
```

#### Slightly less quick way - parse file and add players (you could use this to replace the player list on an already build model)

``` r
# Build an optimizer
mod <- create_optimizer(site = 'DRAFTKINGS', 
                        sport = 'HOCKEY', 
                        contest_type = 'CLASSIC')

# Parse and add players
mod <- add_players_from_csv(mod, filepath = '/Path/to/DKSalaries_nhl.csv')
```

You can also add players from a data.frame – see `?add_players_from_df`
or `?get_players_from_data_frame` for more detail and examples\!

#### Updating the Projected Fantasy Points for your Model

Once you have an existing optimizer model, you can update the FPTS using
the function `update_fpts)`. This requires a data.frame that contains
columns `id` and `fpts` – the easiest way to get players’ ids is using
`get_player_data` on an optimizer with players added.

``` r
# Build an optimizer
mod <- create_optimizer(site = 'DRAFTKINGS', 
                        sport = 'HOCKEY', 
                        contest_type = 'CLASSIC',
                        filepath = '/Path/to/DKSalaries_nhl.csv')

# Get the data
df <- get_player_data(mod)

# From here, you can replace the value of FPTS in df by either
# merging with another table by player name or ID
# As an example, we'll just toss in some random values
df$fpts <- runif(nrow(df), 0, 1)

# Update the model with the new FPTS
mod <- update_fpts(mod, fpts_data = df)
```
