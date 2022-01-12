library(shiny)
library(tidyverse)
library(shinydashboard)
library(gganimate)
library(waiter)
library(shinyWidgets)
library(showtext)

if(!any(grepl("Baskervville", font_families(), ignore.case = TRUE))){
  font_add_google("Baskervville", "Baskervville")
}
showtext_auto()

# Load in datasets.
pitch_timbre <- read_rds("data/tidy_pitch_timbre.rds")
general_audio_values <- read_rds("data/tidy_descriptive_values.rds")

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
           confidence_or_value == cov) %>% 
    mutate(composer_group = composer_period) %>% 
    select(composer_group, descriptive_value)
  return(tbl)
}

filter_for_all <- function(sot, cv, cov) {
  tbl <- general_audio_values %>% 
    filter(track_or_section_value == sot,
           descriptive_value_type == cv,
           confidence_or_value == cov) %>% 
    mutate(composer_group = "All") %>% 
    select(composer_group, descriptive_value)
  return(tbl)
}
