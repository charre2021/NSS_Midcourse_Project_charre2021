library(tidyverse)
library(spotifyr)
library(rjson)
library(stringr)
library(lubridate)

# Read Spotify client id and secret from json file.
credentials <- fromJSON(file = "data/credentials.json")

# Set this id and secret as environmental variables for spotifyr to pull by default.
Sys.setenv(SPOTIFY_CLIENT_ID = credentials$SPOTIFY_CLIENT_ID)
Sys.setenv(SPOTIFY_CLIENT_SECRET = credentials$SPOTIFY_CLIENT_SECRET)

# Obtain my playlists to pull track ids from.
my_playlists <- get_my_playlists(limit = 36) %>% 
  select(id, name)

# Pull tracks and composer from each playlist and combine in larger tibble.
all_tracks <- map(my_playlists$id, function(playlist_id) {
  dummy <- get_playlist_tracks(playlist_id = playlist_id)
  
  # Obtain composer and composer_id.
  dummy <- bind_cols((dummy %>% 
                        select(track.id, track.name)),
                     (dummy %>% 
                        pull(track.artists) %>% 
                        map(., function(x) {
                          composer_vec <- c(composer_id = x$id[1],
                                            composer = x$name[1])
                          return(composer_vec)
                        }) %>% 
                        tibble(composer = .) %>% 
                        unnest_wider(composer))) %>% 
    mutate(playlist_id = playlist_id)
  
  # Perform some composer clean-up through majority rule.
  primary <- dummy %>% 
    count(composer) %>% 
    .[["composer"]] %>% 
    .[[1]]
  
  dummy <- dummy %>% 
    mutate(composer = primary) %>% 
    select(-playlist_id)
  
  # Obtain artist image.
  artist <- get_artist(id = dummy$composer_id[1])
  dummy %>% 
    mutate(composer_image = artist$images$url[1])
  
}) %>% bind_rows() %>% 
  rename(track_id = track.id,
         track_name = track.name)

# Initiate progress bar.
pb <- progress::progress_bar$new(total = length(all_tracks$track_id))

# Pull the audio analysis for each track by track.
all_tracks_audio_analysis <- map(all_tracks$track_id, function(track_id) {
  
  # Progress bar progress display.
  pb$tick()
  
  # Call to API for track audio analysis information.
  full_track_analysis <- get_track_audio_analysis(id = track_id)
  
  # Pull out track values and create tibble.
  track_analysis <- full_track_analysis$track %>%
    as_tibble_row() %>%
    select(loudness,
           tempo,
           tempo_confidence,
           time_signature,
           time_signature_confidence,
           key,
           key_confidence,
           mode,
           mode_confidence) %>%
    rename_with(~paste0("track_",.x)) %>%
    mutate(track_id = track_id)
  
  return(track_analysis)
  
  # Then bind each track analysis together and join with all_tracks.
}) %>%
  bind_rows() %>%
  left_join(all_tracks)

# Create pitch column names and timbre column names for separation.
pitch_names_vec <- c(0:11) %>% 
  map(., function(x) {paste0("pitch_class_score_",str_trim(x))})

timbre_names_vec <- c(0:11) %>% 
  map(., function(x) {paste0("timbre_score_",str_trim(x))})

# Initiate progress bar.
pb <- progress::progress_bar$new(total = length(all_tracks$track_id))

# Pull the audio analysis for each track by segment for pitch and timbre and section.
all_audio_analysis <- map(all_tracks$track_id, function(track_id) {
  
  # Progress bar progress display.
  pb$tick()
  
  # API call through SpotifyR.
  full_track_analysis <- get_track_audio_analysis(id = track_id)
  
  # Pull in section information.
  section_analysis <- full_track_analysis$sections %>% 
    as_tibble() %>% 
    rename_with(~paste0("section_",.x)) %>%
    mutate(track_id = track_id,
           section_end = section_start + section_duration)
  
  # Pull in segment information.
  segment_analysis <- full_track_analysis$segments %>%
    as_tibble() %>%
    select(start,
           duration,
           pitches,
           timbre) %>%
    # Set names for splitting of vectors into separate columns.
    mutate(pitches = map(pitches, setNames, pitch_names_vec),
           timbre = map(timbre, setNames, timbre_names_vec)) %>%
    # Break out all pitch and timbre vectors into separate columns.
    unnest_wider(pitches) %>%
    unnest_wider(timbre)
  
  # Replace all segment start time values with applicable section start time values.
  segment_analysis$start <- map(segment_analysis$start, function(segment_start) {
    map2(section_analysis$section_start, section_analysis$section_end, 
         function(section_start, section_end) {
           if(between(segment_start, section_start, section_end)){
             segment_start <- section_start
           }
         }) %>% plyr::compact(.) %>% as.numeric(.[1])
  }) %>% as.numeric(.)
  
  # Initially combine section and segment information.
  audio_analysis <- left_join(section_analysis, 
                              segment_analysis,
                              by = c("section_start" = "start"))
  
  # Create mean pitch and timbre scores by section.
  mean_section_analysis <- audio_analysis %>%
    group_by(section_start) %>%
    dplyr::summarize(across(starts_with("pitch"), mean),
                     across(starts_with("timbre"), mean)) %>%
    rename_with(~paste0("mean_",.x))
  
  # Create median pitch and timbre scores by section.
  median_section_analysis <- audio_analysis %>%
    group_by(section_start) %>% 
    dplyr::summarise(across(starts_with("pitch"), median),
                     across(starts_with("timbre"), median)) %>%
    rename_with(~paste0("median_",.x))
  
  # Join mean and median datasets.
  section_analysis <- full_join(mean_section_analysis,
                                median_section_analysis,
                                by = c("mean_section_start" = "median_section_start")) %>% 
    rename("section_start" = "mean_section_start")
  
  # Clean up initially combined dataset for ultimate join to mean and median datasets.
  audio_analysis <- audio_analysis %>% 
    select(-starts_with("pitch"),
           -starts_with("timbre"),
           -duration) %>% 
    .[!duplicated(.),]
  
  # Final join for one track.
  audio_analysis <- inner_join(audio_analysis, 
                               section_analysis, 
                               by = "section_start")
  
  return(section_analysis)
  
  # Then bind each segment analysis together and join with track analysis.
}) %>%
  bind_rows() %>%
  right_join(all_tracks_audio_analysis)

# Write a csv file to a local folder.
write_csv(all_audio_analysis, "data/all_audio_analysis.csv")

