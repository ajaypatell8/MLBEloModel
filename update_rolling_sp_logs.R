library(zoo)
library(tidyverse)
library(readr)
library(baseballr)

pitcher_logs <- read_csv("pitcher_logs.csv")
mlb_pitcher_ids <- read_csv("mlb_pitcher_ids.csv")

#get rolling k-bb% for pitchers
#drop extra cols
pitcher_logs <- pitcher_logs %>% select(-Year, -game_number, -roll_kbb, -weighted_roll_kbb)

#get basic rolling logs
#logs currently contain up to 9/7
date <- "2023-09-07"
year <- 2023
  
for (pitcher in mlb_pitcher_ids$key_fangraphs) {
  flag <- TRUE
  
  tryCatch({
    pitch_gl <- fg_pitcher_game_logs(pitcher, year) %>%
      mutate(Year = str_sub(Date, 1, str_locate(Date, "-")[, 1])) %>%
      mutate(Year = as.numeric(str_remove(Year, "-"))) %>%
      mutate(Date = as.Date(Date))
    
    pitch_gl <-
      pitch_gl %>% select(playerid, Date, Team, Opp, IP, `K-BB%`)
    
    pitch_gl <- pitch_gl %>%
      filter(Date > as.Date(date))
    

    pitch_gl <-
      left_join(pitch_gl,
                mlb_pitcher_ids,
                by = c("playerid" = "key_fangraphs"))
    
    pitch_gl <- pitch_gl %>%
      rename("key_fangraphs" = "playerid") %>%
      select(name,
             key_mlbam,
             key_fangraphs,
             Date,
             Team,
             Opp,
             IP,
             `K-BB%`)
    
    print(pitch_gl)
    
    pitcher_logs <- rbind(pitcher_logs, pitch_gl)
    
  },
  
  error = function(e) {
    flag <- FALSE
  })
  if (!flag)
    next
  
}

#remove duplicated rows
pitcher_logs1 <- pitcher_logs[!duplicated(pitcher_logs),]

pitcher_logs1 <- pitcher_logs1 %>%
  mutate(Year = str_sub(Date, 1, str_locate(Date, "-")[, 1])) %>%
  mutate(Year = as.numeric(str_remove(Year, "-"))) %>%
  mutate(Date = as.Date(Date))

install.packages("roll")
library(roll)

roll_pitcher_logs <- pitcher_logs1 %>%
  group_by(Year, key_mlbam) %>%
  arrange(Date) %>%
  #mutate game number in
  mutate(game_number = row_number()) %>%
  mutate(roll_kbb = roll::roll_mean(`K-BB%`, width = 7))

#get career avg k-bb% to impute
avg_kbb <- roll_pitcher_logs %>%
  group_by(key_fangraphs) %>%
  summarise(avg_kbb = mean(`K-BB%`))

roll_pitcher_logs <-
  left_join(roll_pitcher_logs, avg_kbb, by = c("key_fangraphs"))

roll_pitcher_logs <- roll_pitcher_logs %>%
  mutate(roll_kbb = if_else(is.na(roll_kbb), avg_kbb, roll_kbb)) %>%
  #don't need avg
  select(-avg_kbb) %>%
  mutate(weighted_roll_kbb = (roll_kbb  * mean(IP)) / 4.8)

roll_pitcher_logs <- roll_pitcher_logs[!duplicated(roll_pitcher_logs),]
  
#temp fix, need to come back and figure out why returning df isn't right
write_csv(roll_pitcher_logs, "pitcher_logs.csv")
updated_pitcher_logs <- roll_pitcher_logs

starters_ids <- read_rds("startersIDS.rds")

#filter to only pitchers that have started
sp_logs <- updated_pitcher_logs%>% 
  filter(key_mlbam %in% starters_ids)

#find team average by year
team_avg_kbb1 <- updated_pitcher_logs%>% 
  group_by(Team, Year) %>% 
  summarise(team_avg_w_kbb = mean(weighted_roll_kbb))

updated_pitcher_logs<- left_join(updated_pitcher_logs, team_avg_kbb1, by = c("Year", "Team"))

#add rating adjustment
updated_pitcher_logs<- updated_pitcher_logs %>% 
  mutate(sp_elo_adj = 50 * (weighted_roll_kbb - team_avg_w_kbb))

#read in all game pks
game_pks <- readRDS("game_pks.RDS")

#WOULD NEED TO GET NEW GAME PKS
update_game_pks <- function(last_date) {
  
  #last set of game pks scraped on
  
  last_date <- as.Date(last_date)
  
  dates_scrape <- c(as.Date(last_date:Sys.Date()))
  
  
  for(d in dates_scrape) {
    print(d)
    gp <- baseballr::get_game_pks_mlb(as.Date(d), level_ids = 1)
    
    gp <- gp$game_pk
    
    game_pks <- append(game_pks, gp)
  }
  
  game_pks <- unique(game_pks)
  
  saveRDS(game_pks, "game_pks.RDS")
  
  return(game_pks)
}

#last ran on 9/8/2023 so next update should be "2023-09-08"
game_pks1 <- update_game_pks("2023-08-20")

#need to filter down to pitchers only when they started, re-running loop to get probables for each game
starters_games <- read_csv("sp_by_game.csv")

old_game_pks <- starters_games$game_pk

new_gpks <- setdiff(game_pks1, old_game_pks)

for(game_pk in new_gpks) {
  
  flag <- TRUE
  
  tryCatch(
    {  
      print(game_pk)
      starters <- baseballr::mlb_probables(game_pk) %>% select(-home_plate_id, -home_plate_full_name)
    
    starters_games <- rbind(starters_games, starters)},
    
    error=function(e) {
      
      flag <- FALSE
    }
  )
  if (!flag) next
  
}

#remove missing row
starters_games1 <- starters_games %>% 
  filter(!is.na(game_pk)) %>% 
  mutate(game_date = as.Date(game_date))

roll_pitcher_logs_small1 <- left_join(updated_pitcher_logs, starters_games1, by = 
                                        c("Date" = "game_date", "key_mlbam" = "id"))

roll_pitcher_logs_small1 <- roll_pitcher_logs_small1 %>% 
  #keep only sp
  filter(!is.na(game_pk)) 

#remove the "@"
roll_pitcher_logs_small1 <- roll_pitcher_logs_small1 %>% 
  mutate(Opp = str_remove(Opp, "@"))

#save csv
write_csv(roll_pitcher_logs_small1, "updated_sp_elo_adj.csv")
