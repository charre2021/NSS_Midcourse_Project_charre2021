library(shiny)
library(tidyverse)
library(shinydashboard)
library(gganimate)
library(waiter)
library(shinyWidgets)
library(showtext)

# Only load the font once.
#font_add_google("Baskervville", "Baskervville")
showtext_auto()

pitch_timbre <- read_rds("data/tidy_pitch_timbre.rds")
general_audio_values <- read_rds("data/tidy_descriptive_values.rds")

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
