shinyUI(
  navbarPage(
    "Nessun Data!",
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "stylesheet.css"),
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"
      ),
      useSever()
    ),
    tabPanel("", 
             icon = icon('book','fa-2x'),
             value = "about_active",
             autoWaiter(id = c("frame", 
                               "comparison_density", 
                               "circlebarplot", 
                               "timbrebarplot",
                               "logreg_table",
                               "confusion_matrix",
                               "show_curve",
                               "timeline",
                               "waveform",
                               "spectrogram"),
                        html = spin_wave(), 
                        color = '#000029'),
             sidebarLayout(
               sidebarPanel(id = "about_sidebar",
                            h2("About"),
                            p("It's no surprise that data is powerful.
                              Data can reinforce our common sense or knowledge about 
                              the world, or it can completely uproot our understanding of it."),
                            br(),
                            p("For this R Shiny App, I have used data to analyze one 
                              of my domain knowledges and great loves-classical music.
                              In music history classes, I was taught that western 
                              classical music was a story of progression, then 
                              deconstruction, and then reconstruction. But how 
                              true is that analysis? What does the data show? 
                                And can we reveal-through objective variables in 
                              data-this musical path? Or does the data challenge 
                              our assumptions about how we believe our ears 
                              are hearing the differences between pieces?"),
                            br(),
                            p("Using the Spotify Web API, I analyzed objective 
                              variables from 2,066 pieces by 36 different 
                              classical composers from 1098 CE to today to 
                              understand and answer these questions. These 
                              variables include key, tempo, volume, time signature,
                              pitch and timbre, as well as confidences related to those 
                              variables. I sought with this R Shiny App to replicate a 
                              specific problem in musicology and music theory: Can we
                              determine what period a piece is from or which composer 
                              composed it based on its musical characteristics?")),
               mainPanel(id = "about_info",
                         h4("Timeline of Western Music",
                            style = "text-align: center;"),
                         fluidRow(id = "timeline_box",
                                  timevisOutput("timeline"),
                                  style = "height: 675px;")
               )
             )
    ),
    tabPanel("", 
             icon = icon('chart-bar','fa-2x'),
             value = "plots_active",
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
                 htmlOutput("frame"),
                 style = "position: fixed; width: 32%",
               ),
               mainPanel(
                 tabsetPanel(id = "primary_tabs",
                             tabPanel("",
                                      icon = icon('chart-pie','fa-2x'),
                                      value = "descriptive_active",
                                      fluidRow(
                                        column(width = 3,
                                               HTML("<h4><b>Comparison Analysis</b></h4>"),
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
                                               selectInput(
                                                 inputId = "first_comparison_group",
                                                 label = "Choose First Group:", 
                                                 choices = c("Medieval/Renaissance",
                                                             "Baroque/Classical",
                                                             "Romantic",
                                                             "Modern/Post-Modern",
                                                             "All"),
                                                 selected = "Medieval/Renaissance"
                                               ),
                                               selectInput(
                                                 inputId = "second_comparison_group",
                                                 label = "Choose Second Group:", 
                                                 choices = c("Medieval/Renaissance",
                                                             "Baroque/Classical",
                                                             "Romantic",
                                                             "Modern/Post-Modern",
                                                             "All"),
                                                 selected = "Baroque/Classical"
                                               ),
                                               style='border-right: 1px solid #d1d1d1; height: 400px;',
                                        ),
                                        column(width = 9,
                                               plotOutput("comparison_density")
                                        )
                                      ),
                                      br(),
                                      hr(color = "#d1d1d1"),
                                      fluidRow(
                                        HTML("<h4><b>Song Pitch and Timbre Analysis</b></h4>"),
                                        br(),
                                        column(width = 6,
                                               awesomeRadio(
                                                 inputId = "mean_or_median",
                                                 label = "Select Median or Median:", 
                                                 choices = c("Mean", "Median"),
                                                 selected = "Mean",
                                                 inline = TRUE
                                               ),
                                               plotOutput("circlebarplot"),
                                               style='border-right: 1px solid #d1d1d1; height: 400px;',
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
                                        column(width = 3,
                                               HTML("<h4><b>Regression Analysis</br>
                                                    By Composer</b></h4>"),
                                               br(),
                                               selectInput(
                                                 inputId = "regression_comparison_group",
                                                 label = "Choose Regression Comparison Group:", 
                                                 choices = c("Medieval/Renaissance",
                                                             "Baroque/Classical",
                                                             "Romantic",
                                                             "Modern/Post-Modern",
                                                             "All"),
                                                 selected = "Romantic"
                                               ),
                                               pickerInput(
                                                 inputId = "logreg_variables",
                                                 label = "Select Regression Variables:", 
                                                 choices = list(
                                                   `Mean Timbre` = mean_timbre_group,
                                                   `Median Timbre` = median_timbre_group,
                                                   `Mean Pitch` = mean_pitch_group,
                                                   `Median Pitch` = median_pitch_group
                                                 ),
                                                 selected = c(mean_timbre_group,mean_pitch_group),
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE
                                               ),
                                               awesomeRadio(
                                                 inputId = "curve_type",
                                                 label = "Curve Type:", 
                                                 choices = c("Calibration",
                                                             "Gain",
                                                             "ROC"),
                                                 selected = "Calibration"
                                               ),
                                               numericInput("setseed", 
                                                            "Set Seed:", 
                                                            value = 36),
                                               style = 'border-right: 1px solid #d1d1d1; height: 400px;'
                                        ),
                                        column(width = 9,
                                               DTOutput('logreg_table')
                                        ),
                                      ),
                                      br(),
                                      hr(color = "#d1d1d1"),
                                      fluidRow(
                                        column(width = 6,
                                               plotOutput("confusion_matrix"),
                                               style ='border-right: 1px solid #d1d1d1; 
                                               height: 400px;'
                                        ),
                                        column(width = 6,
                                               plotOutput("show_curve"))
                                      )
                             ),
                             tabPanel("",
                                      icon = icon('volume-up','fa-2x'),
                                      value = "spectrogram_active",
                                      fluidRow(column(3,
                                                      HTML("<h4><b>Sound Sample Analysis</b></h4>"),
                                                      br(),
                                                      actionBttn(
                                                        inputId = "spectro",
                                                        label = "Start Analysis",
                                                        style = "jelly", 
                                                        color = "primary",
                                                        size = "sm"
                                                      ),
                                                      br(),
                                                      br(),
                                                      numericInput("setstarttime", 
                                                                   "Start Time in Seconds:", 
                                                                   value = 36,
                                                                   min = 0,
                                                                   max = 100,
                                                                   step = 1),
                                                      br(),
                                                      sliderTextInput("duration",
                                                                      "Set Duration of Sample:",
                                                                      choices = c(1:15)),
                                                      br(),
                                                      style = 'border-right: 1px solid #d1d1d1; height: 400px;'),
                                               column(9,
                                                      plotOutput("waveform"))),
                                      br(),
                                      hr(color = "#d1d1d1"),
                                      fluidRow(plotOutput("spectrogram")))
                             )
                 )
               )
             )
    )
  )
  