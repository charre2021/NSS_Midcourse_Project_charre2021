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
                 # Test Values
                 selectInput("composer",
                             "Select Composer:",
                             choices = c("Wolfgang Amadeus Mozart",
                                         "Johann Sebastian Bach",
                                         "Richard Wagner")
                 ),
                 # Test Values, will need to correspond to actual names.
                 # Will also need to be filtered by selected composer.
                 selectInput("song", 
                             "Select Song:", 
                             choices = c("2twkod822LTomifvDubtM8",
                                         "6SpLc7EXZIPpy0sVko0aoU",
                                         "1HNE2PX70ztbEl6MLxrpNL"),
                             selected = "2twkod822LTomifvDubtM8"),
                 htmlOutput("frame")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("", 
                            icon = icon('chart-pie','fa-2x')
                   ),
                   tabPanel("", 
                            icon = icon('chart-line','fa-2x')
                   )
                 )
               )
             )
    )
  )
)
