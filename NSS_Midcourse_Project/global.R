library(shiny)
library(tidyverse)
library(shinydashboard)
library(gganimate)
library(waiter)
library(shinyWidgets)
library(showtext)

font_add_google("Baskervville", "Baskervville")
showtext_auto()

pitch_timbre <- read_rds("data/tidy_pitch_timbre.rds")
general_audio_values <- read_rds("data/tidy_descriptive_values.rds")

test <- pitch_timbre %>%
  filter(composer == "Aaron Copland",
         track_name == "Fanfare for the Common Man",
         pitch_or_timbre == "Pitch",
         mean_or_median == "Mean",
         section_start == section_start)
# 
# circlebarplot <- ggplot(test,
#                         aes(x = class,
#                             y = exp(score))) +
#   geom_bar(stat = "identity",
#            fill = "blue")
# 
# ggplot_build(circlebarplot)
# 
# ggplot_build(circlebarplot)$data[[1]]$y
# 
# circlebarplot +
#   ylim(-1,max(exp(score)) + 1) +
#   coord_polar(start = -pi/12) +
#   geom_text(aes(x = class,
#                 y = -0.1,
#                 label = class)) +
#   theme(
#     panel.grid = element_blank(),
#     panel.background = element_rect(fill = "#fafafa", colour = "#000000"),
#     plot.background = element_rect(fill = "#fafafa"),
#     axis.text = element_blank(),
#     axis.title = element_blank())
