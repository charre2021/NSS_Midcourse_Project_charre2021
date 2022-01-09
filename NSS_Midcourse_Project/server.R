shinyServer(function(input, output, session) {
  
  filtered_by_composer <- reactive({
    general_audio_values %>% 
      filter(composer == input$composer)
  })
  
  output$composer_name <- renderText({
    input$composer
  })
  
  output$composer <- renderUI({
    imgURL <- filtered_by_composer() %>%
      select(composer_image) %>%
      unique()
    
    tags$img(src = imgURL,
             height = 400,
             width = 350)
    })
  
  
  
  observeEvent(input$composer,{
    song_choices <- filtered_by_composer() %>%
      select(track_name) %>% 
      rename("Name" = "track_name")
    updateSelectizeInput(session,
                         "updateSong",
                         choices = song_choices)
  })
  
  
  new_song <- reactive({
    query <- filtered_by_composer() %>% 
      filter(track_name == input$updateSong) %>% 
      select(track_id) %>% 
      unique()
    
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