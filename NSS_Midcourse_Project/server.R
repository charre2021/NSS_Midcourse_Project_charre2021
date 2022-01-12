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
  
  bar_plot_filter <- reactive({
    pt_filtered_by_composer_and_song() %>% 
      filter(mean_or_median == input$mean_or_median,
             section_start == input$section_start)
  })
  
  circular_bar_plot_filter <- reactive({
    bar_plot_filter() %>% 
      filter(pitch_or_timbre == "Pitch")
  })
  
  timbre_bar_plot_filter <- reactive({
    bar_plot_filter() %>% 
      filter(pitch_or_timbre == "Timbre")
  })
  
  observeEvent(input$updateSong,{
    section_choices <- pt_filtered_by_composer_and_song() %>% 
      filter(mean_or_median == input$mean_or_median) %>%
      rename("Section Starting At:" = "section_start") %>% 
      select(`Section Starting At:`) %>% 
      unique()
    
    updateSelectizeInput(session,
                         "section_start",
                         choices = section_choices)
  })
  
  comparison_group_levels <- reactive(
    c(input$composer, 
      input$first_comparison_group,
      input$second_comparison_group)
  )
  
  composer_tbl <- reactive({
    filtered_by_composer() %>%
      filter(track_or_section_value == input$section_or_track,
             descriptive_value_type == input$comparison_value,
             confidence_or_value == input$confidence_or_value) %>%
      mutate(composer_group = composer) %>%
      select(composer_group, descriptive_value)
  })
  
  
  first_comparison_group_tbl <- reactive({
    if (!(input$first_comparison_group %in% "All")) {
      filter_for_group(input$first_comparison_group,
                       input$section_or_track,
                       input$comparison_value,
                       input$confidence_or_value)
    } else {
      filter_for_all(input$section_or_track,
                     input$comparison_value,
                     input$confidence_or_value)
    }
  })
  
  second_comparison_group_tbl <- reactive({
    if(!(input$second_comparison_group %in% "All")) {
      filter_for_group(input$second_comparison_group,
                       input$section_or_track,
                       input$comparison_value,
                       input$confidence_or_value)
    } else {
      filter_for_all(input$section_or_track,
                     input$comparison_value,
                     input$confidence_or_value)
    }
  })
  
  density_filter <- reactive({
    bind_rows(composer_tbl(),
              first_comparison_group_tbl(),
              second_comparison_group_tbl()) %>% 
      mutate(composer_group = factor(composer_group, 
                                     levels = comparison_group_levels()))
  })
  
  output$comparison_density <- renderPlot({
    density_plot <- density_filter() %>% 
      ggplot(aes(x = descriptive_value, fill = composer_group)) + 
      geom_density(alpha = 0.5)
    
    density_plot <- density_plot +
      scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
      scale_x_continuous(expand = expansion(mult = c(0,0))) + 
      labs(title = "Group Comparison of Descriptive Values",
           x = "Descriptive Value",
           y = "Density") + 
      scale_fill_manual(name = "Comparison Groups", 
                        values = density_color_palette) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            text = element_text(size = 15,
                                family = "Baskervville",
                                color = "#000000"),
            panel.background = element_rect(fill = "#fafafa", 
                                            colour = "#000000"),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "#fafafa", color = NA),
            plot.margin = margin(t = 0, r = 10, b = 0, l = 0),
            legend.position = c(0.04, .985),
            legend.justification = c("left", "top"),
            legend.box.just = "left",
            legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) 
    
    plot(density_plot)
    
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
        ylim(-scale, max(score_points) * 1.25) +
        coord_polar(start = -pi/12) +
        geom_text(aes(x = class,
                      y = -scale/4.1,
                      label = class),
                  size = 4.25,
                  family = "Baskervville",
                  inherit.aes = FALSE) +
        geom_text(aes(x = class,
                      y = max(score_points) * 1.25,
                      label = round(score,2)),
                  size = 4.25,
                  family = "Baskervville",
                  inherit.aes = FALSE) +
        labs(title = "Pitch Class Scores for this Section") +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5),
              text = element_text(size = 15,
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
  
  output$timbrebarplot <- renderPlot({
    
    if(length(timbre_bar_plot_filter()$class) == 0)
      return()
    
    isolate({
      
      timbrebarplot <- timbre_bar_plot_filter() %>% 
        ggplot(aes(x = class, 
                   y = score,
                   fill = class)) +
        geom_bar(stat = "identity",
                 color = "black") +
        scale_fill_manual(values = timbre_color_palette)
      
      score_points <- ggplot_build(timbrebarplot)$data[[1]]$y
      
      timbrebarplot <- timbrebarplot +
        geom_text(aes(x = class,
                      y = if_else(score < 0, score - 6, score + 6),
                      label = round(score,2)),
                  size = 4.25,
                  family = "Baskervville",
                  inherit.aes = FALSE) +
        labs(title = "Timbre Scores for this Section",
             x = "Class",
             y = "Score") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              text = element_text(size = 15,
                                  family = "Baskervville",
                                  color = "#000000"),
              panel.background = element_rect(fill = "#fafafa", 
                                              colour = "#000000"),
              legend.position = "none",
              panel.grid = element_blank(),
              plot.background = element_rect(fill = "#fafafa", color = NA),
              plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) 
      
      plot(timbrebarplot)
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
                           allow = "encrypted-media")
  })
})