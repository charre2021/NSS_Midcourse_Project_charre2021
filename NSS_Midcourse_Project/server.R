shinyServer(function(input, output, session) {
  
  new_song <- reactive({
    query <- input$song
    paste0("https://open.spotify.com/embed/track/", 
           query, 
           "?utm_source=generator&theme=0")
  })
  
  output$frame <- renderUI({
    my_test <- tags$iframe(src = new_song(), 
                           width = "100%", 
                           height = 380)
  })
})