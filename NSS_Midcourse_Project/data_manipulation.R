library(tidyverse)
library(stringr)

all_audio_analysis <- read_csv("data/all_audio_analysis.csv")

# Vector for reordering tidy_descriptive_values later.
tdv_col_order <- c("track_id",
                   "track_name",
                   "composer_id",
                   "composer",
                   "composer_image",
                   "section_start",
                   "section_duration",
                   "track_or_section_value",
                   "descriptive_value_type",
                   "confidence_or_value",
                   "descriptive_value")


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
         track_or_section_value = str_to_title(track_or_section_value)) %>% 
  # Talking track that needs to be removed.
  filter(track_id != "1PAl9YSmvWNreOH7UFkesP") %>% 
  select(-c(starts_with("mean"),
            starts_with("median")),
         -section_confidence,
         -section_end) %>% 
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
                    "number"),
           sep = "_") %>% 
  select(-c("delete_class", "delete_score"))

