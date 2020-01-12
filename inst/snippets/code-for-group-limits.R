animals <- data.frame(group = c(rep('dog',60), rep('cat',60), rep('bird',60), rep('dragon',60)),
                      score = sort(runif(240,0,10)))

animals <- data.frame(name = sapply(hm@players, fullname),
                      group = sapply(hm@players, team),
                      score = sapply(hm@players, fpts))

set_size   <- 8
roster_req <- 3
num_groups <- length(unique(animals$group))
group_ints <- as.integer(as.factor(animals$group))

# Function
gr_ind <- function(i, data) {
  indx <- ifelse(data == unique(data)[i], 1, 0)
  return(indx)
}

# Model
m <- ompr::MILPModel() %>%
  ompr::add_variable(animal_flag[i], i=1:nrow(animals), type = 'binary') %>%
  ompr::add_variable(groups[i,j], i = 1:nrow(animals), j = 1:4, type = 'integer') %>% # j is the number of unique animals.
  ompr::add_variable(smallgroups[i], i = 1:4, type = 'integer') %>%
  ompr::add_variable(smallgroups2[i], i = 1:4, type = 'binary') %>%
  ompr::set_objective(sum_expr(colwise(animals$score[i]) * animal_flag[i], i = 1:nrow(animals))) %>%
  ompr::add_constraint(sum_expr(animal_flag[i], i = 1:nrow(animals)) == set_size) %>%
  # This is where the real fun begins...
  ompr::add_constraint(groups[i,j] == animal_flag[i] * gr_ind(j, animals$group), i = 1:nrow(animals), j = 1:4) %>%
  ## Everything below here is about getting the constraint for 3 teams represented
  ompr::add_constraint(smallgroups[j] == sum_expr(groups[i,j], i = 1:nrow(animals)), j = 1:4) %>%
  ompr::add_constraint(smallgroups2[j] <= smallgroups[j], j = 1:4) %>%
  ompr::add_constraint(smallgroups[j] <= smallgroups2[j] * 10000, j=1:4) %>%
  ompr::add_constraint(sum_expr(smallgroups2[j], j=1:4) >= roster_req) #%>%
  # Max per single team of 6
  #  ompr::add_constraint(smallgroups[j] <= 6, j = 1:4)

s <- ompr::solve_model(m, ompr.roi::with_ROI('glpk'))

# view the output
list(
  ompr::get_solution(s, animal_flag[i]),
  ompr::get_solution(s, groups[i,j]),
  ompr::get_solution(s, smallgroups[i]),
  ompr::get_solution(s, smallgroups2[i])
)
animals[ompr::get_solution(s, animal_flag[i])$value == 1,]
