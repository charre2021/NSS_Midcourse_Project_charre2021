library(tidyverse)
library(lubridate)

all_audio_analysis <- read_csv("data/all_audio_analysis.csv")

# Adding a period column.
medieval_renaissance_composers <- c("Hildegard von Bingen",
                                    "Guillaume Dufay",
                                    "Josquin des Prez",
                                    "Claudio Monteverdi")

baroque_classical_composers <- c("Henry Purcell",
                                 "Antonio Vivaldi",
                                 "Ludwig van Beethoven",
                                 "George Frideric Handel",
                                 "Johann Sebastian Bach",
                                 "Wolfgang Amadeus Mozart",
                                 "Franz Schubert")

romantic_composers <- c("Sergei Rachmaninoff",
                        "Richard Strauss",
                        "Gabriel Fauré",
                        "Gustav Mahler",
                        "Felix Mendelssohn",
                        "Antonín Dvorák",
                        "Ralph Vaughan Williams",
                        "Franz Liszt",
                        "Frédéric Chopin",
                        "Robert Schumann",
                        "Giacomo Puccini",
                        "Giuseppe Verdi",
                        "Richard Wagner",
                        "Johannes Brahms",
                        "Pyotr Ilyich Tchaikovsky")

modern_postmodern_composers <- c("Aaron Copland",
                                 "Charles Ives",
                                 "Igor Stravinsky",
                                 "Arvo Pärt",
                                 "Krzysztof Penderecki",
                                 "Dmitri Shostakovich",
                                 "Arnold Schoenberg",
                                 "Eric Whitacre",
                                 "Maurice Ravel",
                                 "Claude Debussy")

all_audio_analysis <- all_audio_analysis %>% 
  mutate(composer_period = case_when(
    composer %in% medieval_renaissance_composers ~ "Medieval/Renaissance",
    composer %in% baroque_classical_composers ~ "Baroque/Classical",
    composer %in% romantic_composers ~ "Romantic",
    composer %in% modern_postmodern_composers ~ "Modern/Post-Modern",
    # Handling Dvorak.
    composer_id == "6uRJnvQ3f8whVnmeoecv5Z" |
      composer_id == "4GQwgdcDQwqtcHICjUNndp" |
      composer_id == "6n7nd5iceYpXVwcx8VPpxF" ~ "Romantic"),
    composer_period = as_factor(composer_period),
    section_start = seconds_to_period(round(section_start,0)),
    section_duration = seconds_to_period(round(section_duration,0)))

# Replacing missing/bad pictures with better links.
mozart_picture <- "https://utahsymphony.org/app/uploads/sites/2/2018/08/Wolfgang-amadeus-mozart.jpg"
schoenberg_picture <- "https://i.scdn.co/image/6baf76a199780038b113c76aaa8d62e467fb45b5"

# Vector for renaming pitch classes.
pitch_classes <- c("0" = "C",
                   "1" = "C#/Db",
                   "2" = "D",
                   "3" = "D#/Eb",
                   "4" = "E",
                   "5" = "F",
                   "6" = "F#/Gb",
                   "7" = "G",
                   "8" = "G#/Ab",
                   "9" = "A",
                   "10" = "A#/Bb",
                   "11" = "B")

# Vectors for reordering datasets later.
tdv_col_order <- c("track_id",
                   "track_name",
                   "composer",
                   "composer_period",
                   "composer_image",
                   "section_start",
                   "section_duration",
                   "track_or_section_value",
                   "descriptive_value_type",
                   "confidence_or_value",
                   "descriptive_value")

tpt_col_order <- c("track_id",
                   "track_name",
                   "composer",
                   "composer_period",
                   "composer_image",
                   "section_start",
                   "section_duration",
                   "mean_or_median",
                   "pitch_or_timbre",
                   "class",
                   "score")

# Rename all timbre values to add "class" for consistency later.
rename_timbre <- function(variable_names) {
  new_variable_names <- c()
  for (variable_name in variable_names) {
    variable_name <- as.list(unlist(str_split(variable_name, "_"))) %>% 
      append("class", 2) %>% 
      paste(collapse = "_")
    new_variable_names <- c(new_variable_names, variable_name)
  }
  return(new_variable_names)
}

# Vector for pivoting duplicate numeric value columns for tracks and sections.
st_columns_to_pivot <- c("section_loudness",
                         "section_tempo",
                         "section_tempo_confidence",
                         "section_key",
                         "section_key_confidence",
                         "section_mode",
                         "section_mode_confidence",
                         "section_time_signature",
                         "section_time_signature_confidence",
                         "track_loudness",
                         "track_tempo",
                         "track_tempo_confidence",
                         "track_key",
                         "track_key_confidence",
                         "track_mode",
                         "track_mode_confidence",
                         "track_time_signature",
                         "track_time_signature_confidence")

tidy_descriptive_values <- all_audio_analysis %>% 
  pivot_longer(cols = all_of(st_columns_to_pivot),
               names_to = "track_or_section",
               values_to = "descriptive_value") %>% 
  separate(col = "track_or_section",
           into = c("track_or_section_value",
                    "descriptive_value_type"),
           sep = "_",
           extra = "merge") %>% 
  # Clean up of track/section descriptive values after pivot.
  mutate(confidence_or_value = if_else(str_detect(descriptive_value_type, 
                                                  "\\w*_confidence\\b"),
                                       "Confidence",
                                       "Value"),
         descriptive_value_type = str_to_title(str_replace(if_else(str_detect(descriptive_value_type, 
                                                                              "\\w*_confidence\\b"),
                                                                   str_remove(descriptive_value_type, 
                                                                              "_confidence"),
                                                                   descriptive_value_type),"_", " ")),
         track_or_section_value = str_to_title(track_or_section_value),
         composer_image = if_else(composer == "Wolfgang Amadeus Mozart", 
                                  mozart_picture,
                                  if_else(composer == "Arnold Schoenberg",
                                          schoenberg_picture,
                                          composer_image))) %>% 
  # Talking track that needs to be removed.
  filter(track_id != "1PAl9YSmvWNreOH7UFkesP") %>% 
  .[,tdv_col_order]

write_rds(tidy_descriptive_values, "data/tidy_descriptive_values.rds")

# Pivot based on mean/median pitch/timbre values.
tidy_pitch_timbre <- all_audio_analysis %>% 
  
  rename_with(.fn = rename_timbre, .cols = c(starts_with("mean_timbre"),
                                             starts_with("median_timbre"))) %>% 
  pivot_longer(cols = c(starts_with("mean"),
                        starts_with("median")), 
               names_to = "mean_or_median",
               values_to = "score") %>% 
  separate(col = "mean_or_median",
           into = c("mean_or_median", 
                    "pitch_or_timbre", 
                    "delete_class", 
                    "delete_score", 
                    "class"),
           sep = "_") %>% 
  filter(track_id != "1PAl9YSmvWNreOH7UFkesP") %>%
  mutate(mean_or_median = str_to_title(mean_or_median),
         pitch_or_timbre = str_to_title(pitch_or_timbre),
         class = as.numeric(class),
         class = if_else(pitch_or_timbre == "Timbre",
                         class + 1,
                         class),
         class = as.character(class),
         class = if_else(pitch_or_timbre == "Pitch",
                         pitch_classes[class],
                         class),
         class = as_factor(class),
         composer_image = if_else(composer_id == "4NJhFmfw43RLBLjQvxDuRS", 
                                  mozart_picture,
                                  if_else(composer_id == "5U827e4jbYz6EjtN0fIDt9",
                                          schoenberg_picture,
                                          composer_image))) %>% 
  .[,tpt_col_order]

write_rds(tidy_pitch_timbre, "data/tidy_pitch_timbre.rds")

composer_vec <- tidy_descriptive_values %>% 
  select(composer) %>% 
  unique()


