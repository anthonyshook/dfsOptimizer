
# Build optimizer
testmod.hockey <- create_optimizer(site = 'DRAFTKINGS', sport = 'HOCKEY', contest_type = 'CLASSIC')

# Add Players to optimizer
testmod.hockey <- add_players_from_csv(testmod.hockey, filepath = 'C:/Users/antho/Desktop/DFS Slate Files/hockey/DKSalaries_nhl.csv')

# Block a player (Nate MacKinnon)
testmod.hockey <- block_players_by_id(testmod.hockey, '14086443')

# lock a player (Ethan Bear)
#testmod.hockey <- lock_players_by_id(testmod.hockey, "14086865")

# Set a player (Auston Matthews) to a specific max_exposure
#testmod.hockey@players[[5]] <- set_max_exposure(testmod.hockey@players[[5]], exposure = .6)

# construct model
testmod.hockey <- construct_model(testmod.hockey)

# Optimize.
lineups <- build_lineups(testmod.hockey, num_lineups = 1)

# Not exactly the method we'll use
s <- ompr::solve_model(testmod.hockey@model, ompr.roi::with_ROI('glpk'))
s
## get_player_data(testmod.hockey)[ompr::get_solution(s, players[i])$value ==1,]


########### STAGE
# Add stack constraint
stack_con <- .constraintClass(constraint_name='stack constraint', fnc = .add_team_stack, args = list(positions = c('C','W','W'), players = testmod.hockey@players))
testmod.hockey@config@constraints <- list(stack_con = stack_con)

# construct model
testmod.hockey <- construct_model(testmod.hockey)
# Try a same-team stack
s <- ompr::solve_model(testmod.hockey@model, ompr.roi::with_ROI('glpk'))
s

#which(ompr::get_solution(s, players[i])$value ==1)
#ompr::get_solution(s, players[i])[c(5,23,41,25,82,91,30,205,208),]
ompr::get_solution(s, pos_team_stack[i,j])
ompr::get_solution(s, team_stack[i])
get_player_data(testmod.hockey)[ompr::get_solution(s, players[i])$value ==1,]

ompr::get_solution(s, positions[i])
which(ompr::get_solution(s, players[i])$value ==1)

ompr::get_solution(s, teams[i])
ompr::get_solution(s, teams_pos1[i])
ompr::get_solution(s, opps_pos2[i])

####
# Build optimizer
testmod.golf <- create_optimizer(site = 'DRAFTKINGS', sport = 'GOLF', contest_type = 'CLASSIC')

# Add Players
golfers <- get_players_from_csv(path = 'C:/Users/antho/Desktop/DFS Slate Files/golf/DK_golf_salaries.csv')
testmod.golf@players <- golfers

# construct model
testmod.golf <- construct_model(testmod.golf)
optimize(testmod.golf, num_lineups = 5)

# Not exactly the method we'll use
sgolf <- solve_model(testmod.golf@model, ompr.roi::with_ROI('glpk'))
sgolf

get_player_data(testmod.golf)[get_solution(sgolf, players[i])$value ==1,]

######### BASKETBALL SIMPLE
testmod.basketball <- create_optimizer(site = 'DRAFTKINGS', sport = 'BASKETBALL', contest_type = 'CLASSIC')
testmod.basketball <- add_players_from_csv(testmod.basketball, filepath = 'C:/Users/antho/Desktop/DFS Slate Files/basketball/DKSalaries.csv')
testmod.basketball <- add_team_stack(testmod.basketball, positions = c('C','PF', 'SF'))
#testmod.basketball <- restrict_opposing_positions(testmod.basketball, pos1 = 'PF', pos2 = 'PG')
#testmod.basketball <- construct_model(testmod.basketball)
build_lineups(testmod.basketball, num_lineups = 1)

s <- ompr::solve_model(testmod.basketball@model, ompr.roi::with_ROI('glpk'))
s

#ompr::get_solution(s, tmpvar[i])

get_player_data(testmod.basketball)[ompr::get_solution(s, players[i])$value ==1,]

######### HOCKEY SIMPLE
testmod.hockey <- create_optimizer(site = 'DRAFTKINGS', sport = 'HOCKEY', contest_type = 'CLASSIC')
testmod.hockey <- add_players_from_csv(testmod.hockey, filepath = 'C:/Users/antho/Desktop/DFS Slate Files/hockey/DKSalaries_nhl.csv')
#testmod.hockey <- restrict_opposing_positions(testmod.hockey, pos1 = c('C','W','D'), pos2 = 'G')
testmod.hockey <- add_team_stack(testmod.hockey, positions = c('C','W','W'))
#build_lineups(testmod.hockey, num_lineups = 1)

testmod.hockey <- construct_model(testmod.hockey)
testmod.hockey <- team_stack_2(testmod.hockey, c('C','W', 'W'))
s <- ompr::solve_model(testmod.hockey@model, ompr.roi::with_ROI('glpk'))
s

ompr::get_solution(s, tst)
which(ompr::get_solution(s, players[i])$value == 1)
which(ompr::get_solution(s, tstgroups[i])$value > 0)



