# Base settings for sports

##### Draftkings Settings #####
dk_settings <- list(
  # Hockey
  HOCKEY = list(
    CLASSIC = list(
      budget = 50000,
      roster = 9,
      min_team_req = 3,
      max_players_per_team = 9,
      roster_key = list('C' = list(positions = 'C', num = 2),
                        'W' = list(positions = 'W', num = 3),
                        'D' = list(positions = 'D', num = 2),
                        'G' = list(positions = 'G', num = 1)),
      flex_positions = c('C', 'W', 'D')
    ),
    SHOWDOWN = list(),
    TIERS = list()
  ),

  # Football
  FOOTBALL = list(
    CLASSIC = list(),
    SHOWDOWN = list(),
    TIERS = list()
  ),

  # Golf
  GOLF = list(
    CLASSIC = list(
      budget = 50000,
      roster = 6,
      min_team_req = 1,
      max_players_per_team = 6,
      roster_key = list('G' = list(positions = 'G', num = 6)),
      flex_positions = NA_character_
    ),
    SHOWDOWN = list(),
    WEEKEND = list() # Less important
  ),

  # Basketball
  # THIS ONE IS GOING TO BE TOUGH TO SOLVE, due to the multi-position constraints
  BASKETBALL = list(
    CLASSIC = list(
      budget = 50000,
      roster = 8,
      min_team_req = 2,
      max_players_per_team = 7,
      roster_key = list('PG' = list(positions = 'PG', num = 1),
                        'SG' = list(positions = 'SG', num = 1),
                        'SF' = list(positions = 'SF', num = 1),
                        'PF' = list(positions = 'PF', num = 1),
                        'C' = list(positions = 'C', num = 1),
                        'G' = list(positions = c('PG','SG'), num = 1),
                        'F' = list(positions = c('SF', 'PF'), num = 1)),
      flex_positions = c('PG','SG','SF','PF','C')
    )
  )
)


##### Yahoo Settings #####
yahoo_settings <- list(
  # Hockey
  HOCKEY = list(
    CLASSIC = list(
      budget = 200,
      roster = 9,
      min_team_req = 3,
      max_players_per_team = 6,
      roster_key =list('C' = list(positions = 'C', num = 2),
                       'W' = list(positions = 'W', num = 3),
                       'D' = list(positions = 'D', num = 2),
                       'G' = list(positions = 'G', num = 2)),
      flex_positions = NA_character_
    )
  ),

  # Football
  FOOTBALL = list(
    CLASSIC = list()
  ),

  # Golf
  GOLF = list(
    CLASSIC = list(
      budget = 200,
      roster = 6,
      min_team_req = 1,
      max_players_per_team = 9,
      roster_key =list('G' = list(positions = 'G', num = 6)),
      flex_positions = NA_character_
    )
  ),

  # Basketball
  BASKETBALL = list(
    CLASSIC = list(
      budget = 200,
      roster = 8,
      min_team_req = 3,
      max_players_per_team = 6,
      roster_key = list('PG' = list(positions = 'PG', num = 1),
                        'SG' = list(positions = 'SG', num = 1),
                        'SF' = list(positions = 'SF', num = 1),
                        'PF' = list(positions = 'PF', num = 1),
                        'C' = list(positions = 'C', num = 1),
                        'G' = list(positions = c('PG','SG'), num = 1),
                        'F' = list(positions = c('SF', 'PF'), num = 1)),
      flex_positions = c('PG','SG','SF','PF','C')
    )
  )
)


##### FANDUEL #####
# FanDuel Settings
fd_settings <- list(
  # Hockey
  HOCKEY = list(
    CLASSIC = list(
      budget = 55000,
      roster = 9,
      min_team_req = 3,
      max_players_per_team = 9,
      roster_key = list('C' = list(positions = 'C', num = 2),
                        'W' = list(positions = 'W', num = 4),
                        'D' = list(positions = 'D', num = 2),
                        'G' = list(positions = 'G', num = 1)),
      flex_positions = NA_character_
    ),
    SINGLE = list()
  ),

  # Football
  FOOTBALL = list(
    CLASSIC = list(),
    SINGLE = list()
  ),

  # Golf
  GOLF = list(
    CLASSIC = list(
      budget = 60000,
      roster = 6,
      min_team_req = 1,
      max_players_per_team = 9,
      roster_key = list('G' = list(positions = 'G', num = 6)),
      flex_positions = NA_character_
    ),
    SINGLE = list()
  ),

  # Basketball
  BASKETBALL = list(
    CLASSIC = list(
      budget = 60000,
      roster = 9,
      min_team_req = 3,
      max_players_per_team = 4,
      roster_key = list('PG' = list(positions = 'PG', num = 2),
                        'SG' = list(positions = 'SG', num = 2),
                        'SF' = list(positions = 'SF', num = 2),
                        'PF' = list(positions = 'PF', num = 2),
                        'C' = list(positions = 'C', num = 1)),
      flex_positions = NA_character_
    )
  )
)


# Hierarchical list by Site > Sport > Contest Type
base_settings <- list(
  DRAFTKINGS = dk_settings,
  YAHOO      = yahoo_settings,
  FANDUEL    = fd_settings
)
