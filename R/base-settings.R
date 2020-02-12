# Place for base settings
# Draftkings Settings
dk_settings <- list(
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
  FOOTBALL = list(
    CLASSIC = list(),
    SHOWDOWN = list(),
    TIERS = list()
  ),
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
  )
)

# Yahoo Settings
yahoo_settings <- list(
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
  FOOTBALL = list(
    CLASSIC = list()
  ),
  GOLF = list(
    CLASSIC = list(
      budget = 200,
      roster = 6,
      min_team_req = 1,
      max_players_per_team = 9,
      roster_key =list('G' = list(positions = 'G', num = 6)),
      flex_positions = NA_character_
    )
  )
)

# FanDuel Settings
fd_settings <- list(
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
  FOOTBALL = list(
    CLASSIC = list(),
    SINGLE = list()
  ),
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
  )
)

# Hierarchical list by Site > Sport > Contest Type

base_settings <- list(
  DRAFTKINGS = dk_settings,
  YAHOO      = yahoo_settings,
  FANDUEL    = fd_settings
)
