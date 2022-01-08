library(shiny)
library(tidyverse)
library(shinydashboard)
library(gganimate)
library(waiter)

composer_data <- read_csv("data/all_audio_analysis.csv")