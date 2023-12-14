
#remove github pat
Sys.unsetenv("GITHUB_PAT")

devtools::install_github(repo = "sportsdataverse/oddsapiR")

#edit Renviron file
usethis::edit_r_environ()

Sys.setenv(ODDS_API_KEY = "ae5caea9048e8e44d68ddb61810dd9a2")

library(oddsapiR)

#get mlb odds (live odds, might have to pull at start of every game)
oddsapiR::toa_sports_odds(sport_key = "baseball_mlb",
                          markets = "h2h", odds_format = "american") %>% 
  filter(bookmaker_key == "fanduel") %>%  View()

#get historical odds, needs event ids from toa_sports_odds
oddsapiR::toa_sports_odds_history(sport_key = "baseball_mlb",
                                  markets = "h2h") %>% View()
  filter(bookmaker_key == "fanduel") %>% View()

