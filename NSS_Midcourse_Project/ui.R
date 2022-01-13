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
             autoWaiter(
               id = c("frame", 
                      "comparison_density", 
                      "circlebarplot", 
                      "timbrebarplot",
                      "logreg_table"),
               html = spin_wave(), 
               color = '#000029'
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 box(
                   width = 12,
                   id = "textbox",
                   h2("About"),
                   p("It's no surprise that data is powerful. 
                     Data can reinforce our common sense or knowledge 
                     about the world, or it can completely uproot our 
                     understanding of it."),
                   br(),
                   p("For this RShiny App, I have used data to analyze one of my
                     domain knowledges and great loves-classical music. 
                     In music history classes, I was taught that western 
                     classical music was a story of progression, 
                     then deconstruction, and then reconstruction. 
                     But how true is that analysis? What does the data show? 
                     And can we reveal--through objective variables in 
                     data--this musical path? Or does the data challenge our 
                     assumptions about how we believe our ears are hearing 
                     the differences between pieces?"),
                   br(),
                   p("Using the Spotify Web API, I analyzed objective variables 
                     from roughly 2,066 pieces by 36 different classical composers 
                     from 1098 CE to today to understand and answer these questions. 
                     These variables include key, tempo, volume, time signature, 
                     pitch and timbre, as well as confidences related to these 
                     variables. I sought with this RShiny App to replicate a 
                     specific problem in musicology and music theory: Can we 
                     determine what period a piece is from or which composer 
                     composed it based on just listening to the music?"),
                   br(),
                   p("What follows are the results of that analysis.")
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
                 htmlOutput("frame")
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
                                               style='border-right:1px solid #d1d1d1; height: 400px;',
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
                                               style='border-right:1px solid #d1d1d1; height: 400px;',
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
                                               HTML("<h4><b>Regression Analysis</b></h4>"),
                                               br(),
                                               awesomeRadio(
                                                 inputId = "logreg_SOT",
                                                 label = "Track or Section Basis:", 
                                                 choices = c("Track", "Section"),
                                                 selected = "Track",
                                                 inline = TRUE
                                               ),
                                               br(),
                                               pickerInput(
                                                 inputId = "logreg_variables",
                                                 label = "Select Regression Variables:", 
                                                 choices = c("Loudness Value" = "Loudness_Value",
                                                             "Tempo Value" = "Tempo_Value",
                                                             "Tempo Confidence" = "Tempo_Confidence",
                                                             "Key Value" = "Key_Value",
                                                             "Key Confidence" = "Key_Confidence",
                                                             "Mode Value" = "Mode_Value",
                                                             "Mode Confidence" = "Mode_Confidence",
                                                             "Time Signature Value" = "Time_Signature_Value",
                                                             "Time Signature Confidence" = "Time_Signature_Confidence"),
                                                 options = list(`actions-box` = TRUE), 
                                                 multiple = TRUE
                                               ),
                                               br(),
                                               awesomeRadio(
                                                 inputId = "logreg_comparison_group",
                                                 label = "Choose Regression Group:", 
                                                 choices = c("Medieval/Renaissance",
                                                             "Baroque/Classical",
                                                             "Romantic",
                                                             "Modern/Post-Modern",
                                                             "All"),
                                                 selected = "Baroque/Classical"
                                               ),
                                               style = 'border-right:1px solid #d1d1d1; height: 400px;'
                                        ),
                                        column(width = 9,
                                               DTOutput('logreg_table')
                                        ),
                                      ),
                                      br(),
                                      hr(color = "#d1d1d1"),
                                      fluidRow(
                                        column(width = 6,
                                               style ='border-right:1px solid #d1d1d1; height: 400px;'
                                        ),
                                        column(width = 6)
                                      )
                             )
                 )
               )
             )
    )
  )
)
