library(tidyverse)
library(spotifyr)
library(rjson)

# Read Spotify client id and secret from json file.
credentials <- fromJSON(file = "credentials.json")

# Set this id and secret as environmental variables for spotifyr to pull by default.
Sys.setenv(SPOTIFY_CLIENT_ID = credentials$SPOTIFY_CLIENT_ID)
Sys.setenv(SPOTIFY_CLIENT_SECRET = credentials$SPOTIFY_CLIENT_SECRET)

# Obtain my playlists to pull track ids from.
my_playlists <- get_my_playlists(limit = 50) %>% 
  select(id, name) %>% 
  slice(1:41)

# Pull tracks and composer from each playlist and combine in larger tibble.
all_tracks <- map(my_playlists$id, function(playlist_id) {
  dummy <- get_playlist_tracks(playlist_id = playlist_id)
  
  dummy <- bind_cols((dummy %>% 
               select(track.id, track.name)),
            (dummy %>% 
               pull(track.artists) %>% 
               map(., function(x) {
                 x$name[1]
               }) %>% 
               tibble(composer = .))) %>% 
    mutate(playlist_id = playlist_id)
  
  # Perform some composer clean-up.
  primary <- dummy %>% 
    count(composer) %>% 
    .[["composer"]] %>% 
    .[[1]]
  
  dummy %>% 
    mutate(composer = primary) %>% 
    select(-playlist_id)
  
}) %>% bind_rows()
