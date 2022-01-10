shinyServer(function(input, output, session) {
  
  filtered_by_composer <- reactive({
    general_audio_values %>% 
      filter(composer == input$composer)
  })
  
  filtered_by_composer_and_song <- reactive({
    filtered_by_composer() %>% 
      filter(track_name == input$updateSong)
  })
  
  pt_filtered_by_composer <- reactive({
    pitch_timbre %>% 
      filter(composer == input$composer)
  })
  
  pt_filtered_by_composer_and_song <- reactive({
    pt_filtered_by_composer() %>% 
      filter(track_name == input$updateSong)
  })
  
  circular_bar_plot_filter <- reactive({
    pt_filtered_by_composer_and_song() %>% 
      filter(pitch_or_timbre == "Pitch",
             mean_or_median == input$mean_or_median,
             section_start == input$section_start)
  })
  
  observeEvent(input$updateSong,{
    section_choices <- pt_filtered_by_composer_and_song() %>% 
      filter(pitch_or_timbre == "Pitch",
             mean_or_median == input$mean_or_median) %>% 
      select(section_start) %>% 
      unique()
    updateSelectizeInput(session,
                         "section_start",
                         choices = section_choices)
  })
  
  histogram_filter <- reactive({
    filtered_by_composer() %>% 
      filter(track_or_section_value == input$section_or_track,
             descriptive_value_type == input$value_type,
             confidence_or_value == "Confidence")
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
             width = 375)
  })
  
  output$histogram <- renderPlot({
    histogram_plot <- ggplot(histogram_filter(), aes(x = descriptive_value)) +
      geom_histogram(aes(fill = ..count..),
                     bins = 30,
                     breaks = seq(0,1,0.1),
                     color = "black",
                     size = 0.5)
    
    histogram_plot <- histogram_plot +
      scale_x_continuous(expand = c(0.01,0.01)) +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0, max(ggplot_build(histogram_plot)$data[[1]]$count)*1.1)) +
      labs(title = "Selected Value Confidence by Composer",
           x = "Confidence",
           y = "Count within Track or Section") +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size = 16, 
                                family = "Baskervville"),
            panel.background = element_rect(fill = "#fafafa", colour = "#000000"),
            panel.grid = element_blank(), 
            plot.background = element_rect(fill = "#fafafa"),
            legend.position = "none")
    
    print(histogram_plot)
  })
  
  output$circlebarplot <- renderPlot({
    circlebarplot <- ggplot(circular_bar_plot_filter(), 
           aes(x = class, 
               y = score)) +
      geom_bar(stat = "identity", 
               fill = "blue")
    
    circlebarplot <- circlebarplot +
      ylim(-1, max(ggplot_build(circlebarplot)$data[[1]]$y) * 1.1) +
      coord_polar(start = -pi/12) +
      geom_text(aes(x = circular_bar_plot_filter()$class,
                    y = -0.1,
                    label = circular_bar_plot_filter()$class),
                inherit.aes = FALSE) +
      theme(panel.grid = element_blank(),
            panel.background = element_rect(fill = "#fafafa", 
                                            colour = "#000000"),
            plot.background = element_rect(fill = "#fafafa"),
            axis.text = element_blank(),
            axis.title = element_blank())
    
    print(circlebarplot)
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
    query <- filtered_by_composer_and_song() %>% 
      select(track_id) %>% 
      unique()
    
    paste0("https://open.spotify.com/embed/track/", 
           query, 
           "?utm_source=generator&theme=0")
  })
  
  output$frame <- renderUI({
    my_test <- tags$iframe(src = new_song(), 
                           width = "100%", 
                           height = 380,
                           autoplay = "allow")
  })
})