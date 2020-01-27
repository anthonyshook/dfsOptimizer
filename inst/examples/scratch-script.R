
# Build optimizer
testmod.hockey <- optimizer(site = 'DRAFTKINGs', sport = 'HOCKEY', contest_type = 'CLASSIC')

# Add Players
hockey_players <- get_players_from_csv(path = 'C:/Users/antho/Desktop/DFS Slate Files/hockey/DKSalaries_nhl.csv')
testmod.hockey@players <- hockey_players

# Block a player (Nate MacKinnon)
testmod.hockey <- block_players_by_id(testmod.hockey, '14086443')

# lock a player (Ethan Bear)
testmod.hockey <- lock_players_by_id(testmod.hockey, "14086865")

# Set a player (Auston Matthews) to a specific max_exposure
testmod.hockey@players[[5]] <- set_max_exposure(testmod.hockey@players[[5]], exposure = .6)

# construct model
testmod.hockey <- construct_model(testmod.hockey)

# Try adding a position constraint
testmod.hockey@model@mod <- .add_opposing_position_constraint(testmod.hockey@model@mod, pos1 = c('C','W','D'), pos2 = 'G', players = testmod.hockey@players)

# Optimize.
lineups <- optimize(testmod.hockey, num_lineups = 5)

# Not exactly the method we'll use
s <- ompr::solve_model(testmod.hockey@model@mod, ompr.roi::with_ROI('glpk'))
s
## get_player_data(testmod.hockey)[ompr::get_solution(s, players[i])$value ==1,]


########### STAGE
# construct model
testmod.hockey <- construct_model(testmod.hockey)
# Try a same-team stack
testmod.hockey@model@mod <- .add_same_team_stack(testmod.hockey@model@mod, positions = c('C','G','D'), players = testmod.hockey@players)
s <- ompr::solve_model(testmod.hockey@model@mod, ompr.roi::with_ROI('glpk'))
s
which(ompr::get_solution(s, players[i])$value ==1)
ompr::get_solution(s, players[i])[c(5,23,41,25,82,91,30,205,208),]
ompr::get_solution(s, stack_count[i])
ompr::get_solution(s, stack_flag[i])

ompr::get_solution(s, positions[i])
get_player_data(testmod.hockey)[ompr::get_solution(s, players[i])$value ==1,]
which(ompr::get_solution(s, players[i])$value ==1)

ompr::get_solution(s, teams[i])
ompr::get_solution(s, teams_pos1[i])
ompr::get_solution(s, opps_pos2[i])

####
# Build optimizer
testmod.golf <- optimizer(site = 'DRAFTKINGs', sport = 'GOLF', contest_type = 'CLASSIC')

# Add Players
golfers <- get_players_from_csv(path = 'C:/Users/antho/Desktop/DFS Slate Files/golf/DK_golf_salaries.csv')
testmod.golf@players <- golfers

# construct model
testmod.golf <- construct_model(testmod.golf)
optimize(testmod.golf, num_lineups = 5)

# Not exactly the method we'll use
sgolf <- solve_model(testmod.golf@model@mod, ompr.roi::with_ROI('glpk'))
sgolf

get_player_data(testmod.golf)[get_solution(sgolf, players[i])$value ==1,]
