library(shiny)
library(tidyverse)
library(waiter)
library(shinyWidgets)
library(showtext)
library(tidymodels)
library(DT)
library(caret)
library(sever)
library(timevis)
library(readxl)
library(rvest)
library(formattable)
library(seewave)
library(tuneR)
library(viridis)
library(lubridate)
library(shinyjs)

if(!any(grepl("Baskervville", font_families(), ignore.case = TRUE))){
  font_add_google("Baskervville", "Baskervville")
}
showtext_auto()

pitch_timbre <- read_rds("data/tidy_pitch_timbre.rds")
general_audio_values <- read_rds("data/tidy_descriptive_values.rds")
logreg_pt_tibble <- read_rds("data/logreg_pt_tibble.rds")
bkg_data <- read_excel("data/background_data.xlsx",
                       na = "NA")
bkg_data <- bkg_data %>% 
  mutate(id = 1:nrow(bkg_data),
         start = Birth,
         end = Death,
         end = replace_na(end, "January 22, 2022"),
         type = "box",
         content = paste0("<img src=",Image," width='50' height='60'>"))

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

pt_picker_choices <- names(logreg_pt_tibble)[8:length(names(logreg_pt_tibble))]
pt_picker_choices <- setNames(pt_picker_choices,str_replace_all(pt_picker_choices, "_", " "))

mean_group <- pt_picker_choices[grep("Mean", names(pt_picker_choices))]
mean_timbre_group <- mean_group[grep("Timbre", names(mean_group))]
mean_pitch_group <- mean_group[grep("Pitch", names(mean_group))]
median_group <- pt_picker_choices[grep("Median", names(pt_picker_choices))]
median_timbre_group <- median_group[grep("Timbre", names(median_group))]
median_pitch_group <- median_group[grep("Pitch", names(median_group))]

source <- "Source: Naxos Classical Composer Database."

disconnected <- sever_default(
  title = "", 
  subtitle = HTML("<div style = 'font-size: 20px;'>
                  Your session has been disconnected.</div>"), 
  button = "Reconnect")

disconnect_image <- "https://www.alaskapublic.org/wp-content/uploads/2020/12/Tongass-National-Forest.jpg"

icon_formatter <- formatter("span", 
                            x ~ icontext(ifelse(x == "Positive", 
                                                "ok", 
                                                ifelse(x == "Negative", 
                                                       "remove", 
                                                       "minus")), 
                                         ifelse(x == "Positive", 
                                                "Positive", 
                                                ifelse(x == "Negative", 
                                                       "Negative", 
                                                       "Weak"))), 
                            style = x ~ style(color = ifelse(x == "Positive", 
                                                             "green", 
                                                             ifelse(x == "Negative", 
                                                                    "red", 
                                                                    "grey"))))

stat_formatter <- formatter("span",
                            style = x ~ style(display = "block", 
                                              padding = "0px 4px 4px 4px", 
                                              `border-radius` = "4px", 
                                              `background-color` = ifelse(x <= 0.05, 
                                                                          "#000029",
                                                                          "none"),
                                              color = ifelse(x <= 0.05, 
                                                             "#fafafa",
                                                             "none")))

highlight_formatter <- formatter("span",
                                 style = x ~ style(display = "block", 
                                                   padding = "0px 4px 4px 4px", 
                                                   `border-radius` = "4px", 
                                                   `background-color` = ifelse(x >= 0.5 | x <= -0.5, 
                                                                               "yellow",
                                                                               "none"),
                                                   `font.weight` = ifelse(x >= 0.5 | x <= -0.5, 
                                                                          "bold",
                                                                          "none")))




