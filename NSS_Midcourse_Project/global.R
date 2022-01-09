library(shiny)
library(tidyverse)
library(shinydashboard)
library(gganimate)
library(waiter)

pitch_timbre <- read_rds("data/tidy_pitch_timbre.rds")
general_audio_values <- read_rds("data/tidy_descriptive_values.rds")
