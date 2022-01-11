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

time_signature_classes <- c("3" = "Simple Meter in 3",
                            "4" = "Simple Meter in 2 or 4",
                            "5" = "Odd Meter (3, 2)",
                            "6" = "Compound Meter",
                            "7" = "Odd Meter (3, 2, 2)")


