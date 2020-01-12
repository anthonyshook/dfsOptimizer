# Place for base settings
# Draftkings Settings
dk_settings <- list(
  HOCKEY = list(
    CLASSIC = list(
      budget = 50000,
      roster = 9,
      min_team_req = 3
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
      budget = 60000,
      roster = 6,
      min_team_req = 1
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
      max_players_from_team = 6)
  ),
  FOOTBALL = list(
    CLASSIC = list()
    ),
  GOLF = list(
    CLASSIC = list(
      budget = 200,
      roster = 6,
      max_from_one_team = 6
    )
  )
)

# FanDuel Settings
fd_settings <- list(
  HOCKEY = list(
    CLASSIC = list(),
    SINGLE = list()
  ),
  FOOTBALL = list(
    CLASSIC = list(),
    SINGLE = list()
  ),
  GOLF = list(
    CLASSIC = list(),
    SINGLE = list()
  )
)

# Hierarchical list by Site > Sport > Contest Type

base_settings <- list(
  DRAFTKINGS = dk_settings,
  YAHOO      = yahoo_settings,
  FANDUEL    = fd_settings
)
