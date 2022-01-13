library(shiny)
library(tidyverse)
library(shinydashboard)
library(waiter)
library(shinyWidgets)
library(showtext)
library(tidymodels)
library(DT)
library(formattable)

if(!any(grepl("Baskervville", font_families(), ignore.case = TRUE))){
  font_add_google("Baskervville", "Baskervville")
}
showtext_auto()

# Load in datasets.
pitch_timbre <- read_rds("data/tidy_pitch_timbre.rds")
general_audio_values <- read_rds("data/tidy_descriptive_values.rds")
logreg_track_tibble <- read_rds("data/logreg_track_tibble.rds")
logreg_section_tibble <- read_rds("data/logreg_section_tibble.rds")

# Making pitches colored like piano keys.
pitch_color_vector <- c("white",
                        "black",
                        "white",
                        "black",
                        "white",
                        "white",
                        "black",
                        "white",
                        "black",
                        "white",
                        "black",
                        "white")

timbre_color_palette <- colorRampPalette(c("#080d13", "#12466b", "#d0f7ff"))(12)

density_color_palette <- c("#080d13", "#12466b", "#d0f7ff")

filter_for_group <- function(composer_period_group, sot, cv, cov) {
  tbl <- general_audio_values %>% 
    filter(composer_period == composer_period_group,
           track_or_section_value == sot,
           descriptive_value_type == cv,
           confidence_or_value == cov)
  if(sot == "Track") {
    tbl <- tbl %>% 
      select(-section_start, -section_duration) %>%
      unique() %>%
      mutate(composer_group = composer_period) %>% 
      select(composer_group, descriptive_value)
  } else {
    tbl <- tbl %>% 
      mutate(composer_group = composer_period) %>% 
      select(composer_group, descriptive_value)
  }
  return(tbl)
}

filter_for_all <- function(sot, cv, cov) {
  tbl <- general_audio_values %>% 
    filter(track_or_section_value == sot,
           descriptive_value_type == cv,
           confidence_or_value == cov)
  if(sot == "Track") {
    tbl <- tbl %>% 
      select(-section_start, -section_duration) %>%
      unique() %>%
      mutate(composer_group = "All") %>% 
      select(composer_group, descriptive_value)
  } else {
    tbl <- tbl %>% 
      mutate(composer_group = "All") %>% 
      select(composer_group, descriptive_value)
  }
  return(tbl)
}

pitch_classes <- c("C",
                   "C#/Db",
                   "D",
                   "D#/Eb",
                   "E",
                   "F",
                   "F#/Gb",
                   "G",
                   "G#/Ab",
                   "A",
                   "A#/Bb",
                   "B")

time_signature_classes <- c("Unknown Meter",
                            "Simple Meter in 3",
                            "Simple Meter in 2 or 4",
                            "Odd Meter (3, 2)",
                            "Compound Meter",
                            "Odd Meter (3, 2, 2)")

mean_mode <- function(whole_column) {
  unique_column <- unique(whole_column)
  all_modes <- (tabulate(match(whole_column,unique_column)))
  return(mean(unique_column[all_modes == max(all_modes)]))
}

