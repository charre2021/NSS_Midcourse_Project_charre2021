shinyUI(
  navbarPage(
    "Nessun Data!",
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "stylesheet.css")
    ),
    tabPanel("", 
             icon = icon('book','fa-2x'),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 box(
                   width = 12,
                   id = "textbox",
                   h2("About"),
                   # Replace with real text once finalized.
                   p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                   sed do eiusmod tempor incididunt ut labore et dolore magna
                   aliqua. Ut enim ad minim veniam, quis nostrud exercitation
                   ullamco laboris nisi ut aliquip ex ea commodo consequat.
                   Duis aute irure dolor in reprehenderit in voluptate velit
                   esse cillum dolore eu fugiat nulla pariatur. Excepteur
                   sint occaecat cupidatat non proident, sunt in culpa qui
                   officia deserunt mollit anim id est laborum.")
                 )
               )
             )
    ),
    tabPanel("", 
             icon = icon('chart-bar','fa-2x'),
             sidebarLayout(
               sidebarPanel(
                 pickerInput(inputId = "composer",
                             label = "Select Composer:",
                             choices = unique(general_audio_values$composer),
                             selected = NULL,
                             options = list(
                               `live-search` = TRUE)
                 ),
                 selectizeInput("updateSong", 
                                "Select Song:", 
                                choices = NULL),
                 selectInput(
                   inputId = "first_comparison_group",
                   label = "Choose Comparison Group", 
                   choices = c("Medieval/Renaissance",
                               "Baroque/Classical",
                               "Romantic",
                               "Modern/Post-Modern",
                               "All"),
                   selected = "Medieval/Renaissance"
                 ),
                 selectInput(
                   inputId = "second_comparison_group",
                   label = "Choose Comparison Group", 
                   choices = c("Medieval/Renaissance",
                               "Baroque/Classical",
                               "Romantic",
                               "Modern/Post-Modern",
                               "All"),
                   selected = "Medieval/Renaissance"
                 ),
                 htmlOutput("frame")
               ),
               mainPanel(
                 tabsetPanel(id = "primary_tabs",
                             tabPanel("",
                                      icon = icon('chart-pie','fa-2x'),
                                      value = "descriptive_active",
                                      fluidRow(
                                        column(width = 3,
                                               uiOutput("composer")
                                        ),
                                        column(width = 3,
                                               awesomeRadio(
                                                 inputId = "section_or_track",
                                                 label = "Track or Section Basis:", 
                                                 choices = c("Track", "Section"),
                                                 selected = "Track",
                                                 inline = TRUE
                                               ),
                                               awesomeRadio(
                                                 inputId = "comparison_value",
                                                 label = "Select Value to Compare On:", 
                                                 choices = c("Tempo", 
                                                             "Loudness",
                                                             "Key",
                                                             "Timbre",
                                                             "Time Signature"),
                                                 selected = "Tempo"
                                               ),
                                               awesomeRadio(
                                                 inputId = "confidence_or_value",
                                                 label = "Select Confidence or Value:", 
                                                 choices = c("Confidence", "Value"),
                                                 selected = "Confidence",
                                                 inline = TRUE
                                               ),
                                        ),
                                        column(width = 6,
                                               plotOutput("comparison_jitter")
                                        )
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               awesomeRadio(
                                                 inputId = "mean_or_median",
                                                 label = "Select Median or Median:", 
                                                 choices = c("Mean", "Median"),
                                                 selected = "Mean",
                                                 inline = TRUE
                                               ),
                                               plotOutput("circlebarplot")
                                        ),
                                        column(width = 6,
                                               selectizeInput("section_start",
                                                              "Select Section:",
                                                              choices = NULL),
                                               plotOutput("timbrebarplot")
                                        )
                                      )
                             ),
                             tabPanel("",
                                      icon = icon('chart-line','fa-2x'),
                                      value = "logistic_active",
                                      fluidRow(
                                        column(width = 6),
                                        column(width = 6)
                                      ),
                                      fluidRow(
                                        column(width = 6),
                                        column(width = 6)
                                      )
                             )
                 )
               )
             )
    )
  )
)
