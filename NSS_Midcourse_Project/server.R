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
      rename("Section Starting At:" = "section_start") %>% 
      select(`Section Starting At:`) %>% 
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
             width = 350)
  })
  
  output$histogram <- renderPlot({
    histogram_plot <- ggplot(histogram_filter(), aes(x = descriptive_value)) +
      geom_histogram(aes(fill = ..count..),
                     bins = 30,
                     breaks = seq(0,1,0.1),
                     color = "black")
    
    histogram_plot <- histogram_plot +
      scale_x_continuous(expand = c(0.01,0.01)) +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0, max(ggplot_build(histogram_plot)$data[[1]]$count)*1.1)) +
      labs(title = "Distribution of Composer Value Confidence",
           x = "Confidence Value",
           y = "Count on Track or Section Basis") +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size = 12,
                                family = "Baskervville",
                                color = "#000000"),
            panel.background = element_rect(fill = "#fafafa", colour = "#000000"),
            panel.grid = element_blank(), 
            plot.background = element_rect(fill = "#fafafa", color = NA),
            legend.position = "none",
            plot.margin = margin(t = 2, r = 2, b = 2, l = 2))
    
    print(histogram_plot)
  })
  
  output$circlebarplot <- renderPlot({
    
    if(length(circular_bar_plot_filter()$class) == 0)
      return()
    
    isolate({
      
      scale <- 1000
      
      circlebarplot <- circular_bar_plot_filter() %>% 
        ggplot(aes(x = class, 
                   y = score * scale,
                   fill = class)) +
        geom_bar(stat = "identity",
                 color = "black") +
        scale_fill_manual(values = pitch_color_vector)
      
      score_points <- ggplot_build(circlebarplot)$data[[1]]$y
      
      circlebarplot <- circlebarplot +
        ylim(-scale, max(score_points) * 1.2) +
        coord_polar(start = -pi/12) +
        geom_text(aes(x = class,
                      y = -scale/4.1,
                      label = class),
                  size = 4.25,
                  family = "Baskervville",
                  inherit.aes = FALSE) +
        geom_text(aes(x = class,
                      y = max(score_points) * 1.2,
                      label = round(score,2)),
                  size = 4.25,
                  family = "Baskervville",
                  inherit.aes = FALSE) +
        labs(title = "Relative Proportion of Pitches Used in this Track") +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5),
              text = element_text(size = 12,
                                  family = "Baskervville",
                                  color = "#000000"),
              panel.background = element_rect(fill = "#fafafa", 
                                              colour = "#000000"),
              legend.position = "none",
              plot.background = element_rect(fill = "#fafafa", color = NA),
              plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) 
      
      plot(circlebarplot)
    })
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