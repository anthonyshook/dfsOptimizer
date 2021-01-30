## code to prepare `actuals` dataset goes here

nhl_data <- dfsOptimizer::nhl_players
nhl_pts  <- data.table::fread('./data-raw/nhl_actuals.csv')

actuals <- nhl_data[, list(id = id,
                              fullname = paste(first_name, last_name))]

actuals <- merge(actuals,
                 nhl_pts,
                 by = 'fullname',
                 all.x = TRUE)

actuals[is.na(fpts_dk), fpts_dk := 0]

data.table::setnames(actuals, 'fpts_dk', 'actuals')

usethis::use_data(actuals, overwrite = TRUE)
