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
                        'G' = list(positions = 'G', num = 1),
                        'UTIL' = list(positions = c('C','W','D'), num = 1)),
      flex_position = 'UTIL'
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
      flex_position = NA_character_
    ),
    SHOWDOWN = list(),
    WEEKEND = list() # Less important
  ),

  # Basketball
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
                        'F' = list(positions = c('SF', 'PF'), num = 1),
                        'UTIL' = list(positions = c('PG','SG','SF','PF','C'), num = 1)),
      flex_position = 'UTIL'
    )
  ),

  # NASCAR
  NASCAR = list(
    CLASSIC = list(
      budget = 50000,
      roster = 6,
      min_team_req = 1,
      max_players_per_team = 6,
      roster_key = list('D' = list(positions = 'G', num = 6)),
      flex_position = NA_character_
    )
  ),

  # Baseball
  BASEBALL = list(
    CLASSIC = list(
      budget = 50000,
      roster = 10,
      min_team_req = 3,
      max_players_per_team = 5,
      roster_key = list('P' = list(positions = 'P', num = 2),
                        'C' = list(positions = 'C', num = 1),
                        '1B' = list(positions = '1B', num = 1),
                        '2B' = list(positions = '2B', num = 1),
                        '3B' = list(positions = '3B', num = 1),
                        'SS' = list(positions = 'SS', num = 1),
                        'OF' = list(positions = 'OF', num = 3)),
      flex_positions = NA_character_
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
      flex_position = NA_character_
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
      flex_position = NA_character_
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
                        'F' = list(positions = c('SF', 'PF'), num = 1),
                        'UTIL' = list(positions = c('PG','SG','SF','PF','C'), num = 1)),
      flex_position = 'UTIL'
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
      flex_position = NA_character_
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
      flex_position = NA_character_
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
      flex_position = NA_character_
    )
  ),

  # NASCAR
  NASCAR = list(
    CLASSIC = list(
      budget = 50000,
      roster = 5,
      min_team_req = 1,
      max_players_per_team = 5,
      roster_key = list('D' = list(positions = 'G', num = 5)),
      flex_position = NA_character_
    )
  )
)


# Hierarchical list by Site > Sport > Contest Type
base_settings <- list(
  DRAFTKINGS = dk_settings,
  YAHOO      = yahoo_settings,
  FANDUEL    = fd_settings
)
