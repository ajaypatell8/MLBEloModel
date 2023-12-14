library(baseballr)
library(tidyverse)
library(readr)
library(elo)

#vector with all mlb teams
mlb_teams <- readRDS("mlb_teams.RDS")

#contains up to july 27th rn
game_scores <- read_csv("game_scores.csv")

#ADDING GAMES TO GAME SCORES // FOR UPDATING MODEL
colnames(game_scores)
colnames(data)

#get team results
for (team in mlb_teams$team_abbr) {
  
  print(team)
  
  data <- baseballr::team_results_bref(team, 2023) %>% 
    mutate(
      home_team = if_else(H_A == "H", Tm, Opp),
      away_team = if_else(H_A == "H", Opp, Tm),
      home_score = if_else(home_team == Tm, R, RA),
      away_score = if_else(home_team == Tm, RA, R)
    ) %>% 
    mutate(month = sub('.*,\\s*', '', Date),
           month = str_replace(month, " \\s*\\([^\\)]+\\)", ""),
           day = as.numeric(gsub(".*?([0-9]+).*", "\\1", month)),
           month = gsub('[0-9]+', '', month),
           month= gsub(" ", "", month),
           month = match(month, month.abb),
           date = as.Date(paste(Year,month,day,sep="-"),"%Y-%m-%d")) %>% 
    select(-Attendance, -Streak, -cLI, -Record, -Rank, -Win, -Loss, -Date, -Save, -Time)
  
  game_scores <- rbind(game_scores, data)
}

#remove duplicates
game_scores <- game_scores[!duplicated(game_scores), ]


#get the starters for all games
starters_ids <- c()

game_pks_done <- readRDS("game_pks_old.RDS")

#most recent game pks
game_pks <- unique(roll_pitcher_logs_small1$game_pk)

saveRDS(game_pks, "game_pks_old.RDS")

new_game_pks <-  setdiff(game_pks, game_pks_done)

for(game_pk in new_game_pks) {
  print(game_pk)
  starters <- baseballr::mlb_probables(game_pk)
  playerids <- starters %>% pull(id)
  
  starters_ids <- append(starters_ids, playerids)
}

game_pks <- unique(game_pks)

#got from update_rolling_sp_logs
starters_ids <- unique(starters_games$id)

#make starters_ids unique
starters_ids <- unique(starters_ids)

saveRDS(starters_ids, "startersIDS.rds")

#join fg ids
mlbplayerids <- baseballr::get_chadwick_lu()

mlb_pitcher_ids <- mlbplayerids %>% 
  select(name_first, name_last, key_mlbam, key_fangraphs) %>% 
  mutate(name = paste(name_first, name_last, sep = ' ')) %>% 
  select(-name_first, -name_last) %>% 
  filter(!is.na(key_fangraphs), !is.na(key_mlbam)) %>% 
  filter(key_mlbam %in% starters_ids)

#save it
saveRDS(starters_ids, "starters_ids.RDS")
saveRDS(game_pks, "mlb_games_since_2015.RDS")
write_csv(mlb_pitcher_ids, "mlb_sp_ids.csv")
write_csv(game_scores, "game_scores.csv")


#UPDATED IN UPDATED_PITCHER_LOGS
updated_pitcher_logs1 <- read_csv("updated_sp_elo_adj.csv")

updated_pitcher_logs_small <- updated_pitcher_logs1 %>% 
  select(Team, Opp, Date, Year, name, sp_elo_adj)

game_scores_sp <- left_join(game_scores, updated_pitcher_logs_small, by = 
                              c("Tm" = "Team", "Opp", "Year", "date" = "Date"))


#75 missing entries
game_scores_sp %>% 
  filter(is.na(sp_elo_adj)) %>% View()

game_scores_sp1 <- game_scores_sp[!duplicated(game_scores_sp), ]

#remove games with missing roll_kbb
final_sp_game_scores <- game_scores_sp %>% 
  filter(!is.na(sp_elo_adj))

write_csv(final_sp_game_scores, "final_sp_game_scores.csv")

final_sp_game_scores <- read_csv("final_sp_game_scores.csv")

#pivot df out so just one row per game
elo_game_data <- final_sp_game_scores %>% 
  ungroup() %>% 
  group_by(H_A, home_team, away_team, home_score, away_score, Year) %>% 
  select(H_A, home_team, away_team, home_score, away_score, Year, name, sp_elo_adj) %>% 
  mutate(row = row_number()) %>% 
  pivot_wider(names_from = H_A, values_from = c(name, sp_elo_adj)) %>% 
  #rename columns
  rename("away_sp_name" = "name_A",
         "home_sp_name" = "name_H") %>% 
  mutate(sp_elo_adj_H = sp_elo_adj_H * 83.111795 / 50,
         sp_elo_adj_A = sp_elo_adj_A * 83.111795 / 50)

#install elo package
install.packages("elo")
library(elo)


#function to improve model
elo_cv <- function(hfa,
                   regress,
                   k_factor,
                   sp_coeff) {
  data <- elo_game_data %>%
    filter(Year != 2023 &
             Year != 2015) # filter incomplete 2023 season
  seasons <-
    unique(data$Year) # cross validate grouping over the seasons
  
  data <- data %>% 
    mutate(sp_elo_adj_H = sp_elo_adj_H * sp_coeff,
           sp_elo_adj_A = sp_elo_adj_A * sp_coeff)
  
  formula <-
    as.formula(
      glue::glue(
        "score(home_score, away_score) ~ adjust(home_team, {hfa} + sp_elo_adj_H) + adjust(away_team, sp_elo_adj_A) + regress(Year, 1500, {regress}) + k({k_factor}*log(abs(home_score - away_score) + 1))"
      )
    ) # create the formula for {elo}
  stored_brier <- c() # track the log loss of each fold
  stored_games <- c() # track the number of games of each fold
  for (eval_season in seasons) {
    # iterate over each season
    print(eval_season)
    e <- elo.run(formula,
                 data = elo_game_data %>%
                   filter(Year < eval_season)) # grab all games prior to that season
    
    
    new_data <- elo_game_data %>% # evaluate on that season
      filter(Year == eval_season)
    predictions <- predict(e, newdata = new_data)
    scores <- score(new_data$home_score, new_data$away_score)
    brier <-
      mean((scores - predictions) ^ 2 + ((1 - scores) - (1 - predictions)) ^ 2) # caculate the brier of those games
    stored_brier <- c(stored_brier, brier) # store the log loss
    stored_games <-
      c(stored_games, nrow(new_data)) # store the number of games in each tournament
  }
  brier_score <-
    sum(stored_brier * stored_games) / sum(stored_games) # calculate the ultimate log loss
  return(list(Score = -1 * brier_score)) # return the log loss
}

elo_cv(hfa = 23.53836,
       regress = 1,
       k_factor = 4.279556,
       sp_coeff = 10)

install.packages("ParBayesianOptimization")
library(ParBayesianOptimization)

#find best parameters
set.seed(123)
bounds <- list(
  hfa = c(0, 40),
  regress = c(0, 1),
  k_factor = c(0, 40),
  sp_coeff = c(0, 100)
)


optObj <- bayesOpt(
  FUN = elo_cv,
  bounds = bounds,
  initPoints = 10,
  iters.n = 25,
  iters.k = 1
)

optObj$scoreSummary %>% View()

#get best parameters
getBestPars(optObj)

#new model with updated parameters
#last updated on 7/28 morning
elo_mod_2 <-
  elo.run(
    score(home_score, away_score) ~  # score() is a helper function that returns the winning team when two scores from one game are passed in
      #may want to come back and adjust for 2020 season
      adjust(home_team, 23.4237859095654101793116 + sp_elo_adj_H) +
      adjust(away_team, sp_elo_adj_A) +
      # home_team and Opp specify the players _or_ teams involved in each game
      k(1.4129920 * log(abs(
        home_score - away_score
      ) + 1)) +
      regress(Year, 1500, 0.88014019),
    # an arbitrarily selected k factor
    data = elo_game_data
  ) # our training data

#save model object
saveRDS(elo_mod_2, "elo_model.RDS")

View(as.data.frame(elo_mod_2))

results <- as.data.frame(elo_mod_2)

#add win prob
install.packages("EloRating")
library(EloRating)

#logloss for elo model // TESTING MODEL
MLmetrics::LogLoss(results$p.A, results$wins.A)

rank.teams(elo_mod_2)
final.elos(elo_mod_2)
summary(elo_mod_2)


#calculate accuracy 
(8826+2103) / (6838+8826+2103+1367) 

(9022 + 2071) / (6970+9022+1299+2071)

#test on 2019 season
mlb_odds2021 <- read_csv("C:/Users/ajpat/Downloads/mlb-odds-2019.csv")
mlb_odds2020 <- read_csv("C:/Users/ajpat/Downloads/mlb-odds-2020.csv")
mlb_odds2019 <- read_csv("C:/Users/ajpat/Downloads/mlb-odds-2019.csv")
mlb_odds2018 <- read_csv("C:/Users/ajpat/Downloads/mlb-odds-2018.csv")
mlb_odds2017 <- read_csv("C:/Users/ajpat/Downloads/mlb-odds-2017.csv")
mlb_odds2016 <- read_csv("C:/Users/ajpat/Downloads/mlb-odds-2016.csv")
mlb_odds2015 <- read_csv("C:/Users/ajpat/Downloads/mlb-odds-2015.csv")

mlb_odds2021 <- mlb_odds2021 %>% 
  select(Date, VH, Team, Close, Final)

mlb_odds <- rbind(mlb_odds2015,mlb_odds2016, mlb_odds2017,mlb_odds2018, mlb_odds2019, mlb_odds2020,
                  mlb_odds2021)

#log loss function
install.packages("MLmetrics")
library(MLmetrics)

mlb_odds %>% 
  mutate(game = 1:nrow(mlb_odds)) %>% 
  mutate(game_number = 1:nrow(mlb_odds) %/% 2) %>% 
  mutate(game1 = if_else(game %% 2 == 1, game+1, game)) %>% 
  select(-game, -game_number) %>% 
  filter(VH != "N") %>% 
  pivot_wider(names_from = VH, values_from = c(Team, Close, Final )) %>%
  mutate(correct = if_else((Close_V < 0 & Final_V > Final_H) | (Close_V > 0 & Final_V < Final_H), 1, 0)) %>%
  mutate(v_win = if_else(Final_V > Final_H, 1, 0)) %>% 
  mutate(win_prob_v = if_else(Close_V < 0, Close_V / (Close_V - 100), 100 / (Close_V + 100)),
         win_prob_h = if_else(Close_H < 0, Close_H / (Close_H - 100), 100 / (100 + Close_H))) %>% 
  #since 2015, ml accuracy is 55.% - however, improved recently, looking to beat ~59%
  summarise(accuracy = mean(correct),
            logloss = MLmetrics::LogLoss(win_prob_v, v_win))

#log loss of model
MLmetrics::LogLoss(results$p.A, results$wins.A)

#trying predictions
#read in model
library(baseballr)
library(tidyverse)
library(readr)
library(elo)

game_scores <- read_csv("game_scores.csv")

updated_pitcher_logs1 <- read_csv("updated_sp_elo_adj.csv")

elo_mod_2 <- readRDS("elo_model.RDS")

games <- data.frame(matrix(ncol = 3))
colnames(games) <- c("game_pk","home_team", "away_team")

pks <- baseballr::get_game_pks_mlb("2023-09-08", level_ids = 1)
game_pks_today <- pks$game_pk

for (game_pk in game_pks_today) {
  print(game_pk)
  game <- baseballr::mlb_game_linescore(game_pk)
  game <- game %>%
    select(game_pk, home_team_file_code, away_team_file_code) %>%
    rename("home_team" = "home_team_file_code",
           "away_team" = "away_team_file_code") %>%
    mutate(home_team = str_to_upper(home_team),
           away_team = str_to_upper(away_team)) %>%
    #probably need to change this to a case when to account for all abbreviations
    mutate(
      away_team = case_when(
        away_team == "ANA" ~ "LAA",
        away_team == "SF" ~ "SFG",
        away_team == "WAS" ~ "WSN",
        away_team == "KC" ~ "KCR",
        away_team == "TB" ~ "TBR",
        away_team == "SD" ~ "SDP",
        away_team == "CWS" ~ "CHW",
        away_team == "LA" ~ "LAD",
        TRUE ~ away_team
      ),
      home_team = case_when(
        home_team == "ANA" ~ "LAA",
        home_team == "SF" ~ "SFG",
        home_team == "WAS" ~ "WSN",
        home_team == "KC" ~ "KCR",
        home_team == "TB" ~ "TBR",
        home_team == "SD" ~ "SDP",
        home_team == "CWS" ~ "CHW",
        home_team == "LA" ~ "LAD",
        TRUE ~ home_team
      )
    )
  
  games <- rbind(games, game)
  
}



#remove empty row
games <- games %>%
  filter(!is.na(home_team)) 

#remove duplicated rows
games <- games[!duplicated(games), ]

#get the starting pitchers
#need to filter down to pitchers only when they started, re-running loop to get probables for each game
#create empty df
dummy5 <- baseballr::mlb_probables(413663)

columns5 <- colnames(dummy5)

today_sp <- data.frame(matrix(ncol = length(columns5)))

colnames(today_sp) <- columns5

for(game_pk in game_pks_today) {
  
  flag <- TRUE
  
  tryCatch(
    {  
      print(game_pk)
      starters <- baseballr::mlb_probables(game_pk)
      
      today_sp <- rbind(today_sp, starters)},
    
    error=function(e) {
      
      flag <- FALSE
    }
  )
  if (!flag) next
  
  
}

#remove empty row
today_sp <- today_sp %>% 
  filter(!is.na(game_pk)) %>% 
  select(-game_date, -home_plate_full_name, -home_plate_id)

#teams from mlbplotr
library(mlbplotR)
mlb_teams <- mlbplotR::load_mlb_teams() %>% select(team_abbr, team_id_num)

#mutate team abbreviations
mlb_teams <- mlb_teams %>% 
  filter(!is.na(team_id_num)) %>% 
  mutate(      team_abbr = case_when(
    team_abbr == "AZ" ~ "ARI",
    team_abbr == "SF" ~ "SFG",
    team_abbr == "WSH" ~ "WSN",
    team_abbr == "KC" ~ "KCR",
    team_abbr == "TB" ~ "TBR",
    team_abbr == "SD" ~ "SDP",
    team_abbr == "CWS" ~ "CHW",
    team_abbr == "LA" ~ "LAD",
    TRUE ~ team_abbr
  )
  )



today_sp <- left_join(today_sp, mlb_teams, by = c("team_id" = "team_id_num"))

#drop team name
today_sp <- today_sp %>% 
  select(-team, -team_id)

team_abbr <- unique(game_scores$Tm)

team_abbr <- intersect(team_abbr, today_sp$team_abbr)

#pivot wider on game pk
today_sp1 <- today_sp %>% 
  group_by(game_pk) %>% 
  pivot_wider(names_from = c(team_abbr), values_from = fullName) %>% 
  mutate(name = invoke(coalesce, across(all_of(team_abbr)))) %>% 
  select(game_pk, id, name, team_abbr)

games_today <- left_join(games, today_sp1, by = c("game_pk"))

#join to games
away_teams <- games_today %>% 
  group_by(game_pk, home_team, away_team) %>% 
  slice(1) %>% 
  rename("away_id" = "id",
         "away_sp" = "name")

home_teams <- games_today %>% 
  group_by(game_pk, home_team, away_team) %>% 
  slice(2) %>% 
  rename("home_id" = "id",
         "home_sp" = "name")

games_today_final <- left_join(home_teams, away_teams, by =c ("game_pk", "home_team", "away_team"))

#IF MISSING SP, NO PREDICTION
games_today_final <- games_today_final %>% 
  filter(!is.na(home_id), !is.na(away_id))

#get most recent w_roll_kbb values
pitchers <- c(games_today_final$home_id)

pitchers <- append(pitchers, games_today_final$away_id)

#get adjustments values for pitchers today
today_sp_kbb <- data.frame(matrix(ncol = 2))

colnames(today_sp_kbb) <-c("key_mlbam", "sp_elo_adj")

#edit adjustment
updated_pitcher_logs1 <- updated_pitcher_logs1 %>% 
  mutate(sp_elo_adj = sp_elo_adj * 83.111795 / 50)


for(pitcher in pitchers) {
  
  print(pitcher)
  
  data <- updated_pitcher_logs1 %>% 
    arrange(desc(Date)) %>% 
    filter(key_mlbam == pitcher, Year == 2023) %>% 
    ungroup() %>% 
    select(key_mlbam, sp_elo_adj) %>% 
    slice(1)
  
  

  if(nrow(data) == 0) {
    key_mlbam <- pitcher
    sp_elo_adj <- 0.01
    data1 <- data.frame(key_mlbam, sp_elo_adj)

    print(data1)
    today_sp_kbb <- rbind(today_sp_kbb, data1)
  }
  else {
    print(data)
    today_sp_kbb <- rbind(today_sp_kbb, data)
    
  }

}


today_sp_kbb <- today_sp_kbb %>% 
  filter(!is.na(key_mlbam))


#remove duplicated rows
today_sp_kbb <- today_sp_kbb[!duplicated(today_sp_kbb), ]

#join adj to today's games
games_today_adj <- left_join(games_today_final, today_sp_kbb, by = c("home_id" = "key_mlbam"))

#rename col
games_today_adj <- games_today_adj %>% 
  rename("sp_elo_adj_H" = "sp_elo_adj")

#join adj to today's games
games_today_adj <- left_join(games_today_adj, today_sp_kbb, by = c("away_id" = "key_mlbam"))

#rename col
games_today_adj <- games_today_adj %>% 
  rename("sp_elo_adj_A" = "sp_elo_adj")

games_today_adj1 <- games_today_adj %>% 
  select(game_pk, home_team, away_team, home_sp, away_sp, sp_elo_adj_H, sp_elo_adj_A)

home_team_win_prob = predict(elo_mod_2, newdata = games_today_adj1)

today_win_prob <- games_today_adj1 %>%
  ungroup() %>% 
  mutate(home_team_win_prob = predict(elo_mod_2, games_today_adj1))


#prettify win probs
teams <- mlbplotR::load_mlb_teams()

teams <- teams %>% 
  select(team_abbr) %>% 
  mutate(team_abbr1 = case_when(
    team_abbr == "AZ" ~ "ARI",
    team_abbr == "SF" ~ "SFG",
    team_abbr == "WSH" ~ "WSN",
    team_abbr == "KC" ~ "KCR",
    team_abbr == "TB" ~ "TBR",
    team_abbr == "SD" ~ "SDP",
    team_abbr == "CWS" ~ "CHW",
    team_abbr == "LA" ~ "LAD",
    TRUE ~ team_abbr
  ))

today_games <- left_join(today_win_prob, teams, by =c("home_team"  = "team_abbr1")) %>% 
  rename("home_team_abbr" = "team_abbr")

today_games <- left_join(today_games, teams, by = c("away_team" = "team_abbr1")) %>% 
  rename("away_team_abbr" = "team_abbr")

library(gt)
library(gtExtras)



#ADD VEGAS ML
#remove github pat
Sys.unsetenv("GITHUB_PAT")

devtools::install_github(repo = "sportsdataverse/oddsapiR")

#edit Renviron file
usethis::edit_r_environ()

Sys.setenv(ODDS_API_KEY = "ae5caea9048e8e44d68ddb61810dd9a2")

library(oddsapiR)

#get mlb odds (live odds, might have to pull at start of every game)
vegas_odds <- oddsapiR::toa_sports_odds(sport_key = "baseball_mlb",
                                        markets = "h2h", odds_format = "american") %>% 
  filter(bookmaker_key == "fanduel")

#join team abbr
join_name <- mlbplotR::load_mlb_teams()

join_name <- join_name %>% 
  select(team_name, team_abbr)

join_name <- left_join(join_name, teams, by = c("team_abbr"))

#join to vegas odds
vegas_odds <- vegas_odds %>% 
  select(outcomes_name, outcomes_price)

vegas_odds <- left_join(vegas_odds, join_name, by = c("outcomes_name" = "team_name"))

vegas_odds <- vegas_odds %>% select(-team_abbr, -outcomes_name)


#join
today_games_vegas <- left_join(today_games, vegas_odds, by = c("home_team" = 
                                                                 "team_abbr1"))
#convert to win prob
today_games_vegas <- today_games_vegas %>% 
  mutate(vegas_wp = if_else(outcomes_price < 0, outcomes_price 
                            / (outcomes_price - 100), 100 / (100 + outcomes_price)))

today_games_vegas <- today_games_vegas[!duplicated(today_games_vegas), ]

#remove specific rows for dh if necessary
#today_games_vegas <- today_games_vegas[-c(13, 17), ]

table <- today_games_vegas %>% 
  ungroup() %>% 
  select(-game_pk, -home_team, -away_team, -outcomes_price) %>% 
  arrange(desc(home_team_win_prob)) %>% 
  gt() %>% 
  mlbplotR::gt_fmt_mlb_dot_logo(columns = contains("team_abbr")) %>% 
  gt_theme_538() %>% 
  cols_label(
    home_team_abbr = "Home Team",
    away_team_abbr = "Away Team",
    home_sp = "Home SP",
    away_sp = "Away SP",
    sp_elo_adj_H = "Home SP Elo Adj",
    sp_elo_adj_A = "Away SP Elo Adj",
    home_team_win_prob = "Home Team Win Prob",
    vegas_wp = "Vegas Win Prob"
  ) %>% 
  fmt_number(columns = c(sp_elo_adj_H, sp_elo_adj_A)) %>% 
  fmt_percent(columns = c(home_team_win_prob, vegas_wp), decimals = 0) %>% 
  cols_move_to_start(columns = c("home_team_abbr", "away_team_abbr")) %>% 
  tab_header(
    title = "Win Probabilities For 9/8 MLB Games",
    subtitle = "Games With Missing Probable SP Excluded | @ajaypatel8_"
  ) %>% 
  tab_footnote(
    footnote = "SP Elo Adjustments Are Relative To Team, Not Pitcher To Pitcher",
    locations = cells_column_labels(columns = c(sp_elo_adj_H, sp_elo_adj_A))
  )

gtsave(table, "predictions.png", vwidth = 900, vheight = 900)

elo::final.elos(elo_mod_2)

summary(updated_pitcher_logs1$sp_elo_adj)
