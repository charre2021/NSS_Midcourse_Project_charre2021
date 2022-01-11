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
  
  hex_filter <- reactive({
    
    composer_tbl <- filtered_by_composer() %>% 
      filter(track_or_section_value == input$section_or_track,
             descriptive_value_type == input$comparison_value,
             confidence_or_value == input$confidence_or_value) %>% 
      mutate(group = composer) %>% 
      select(group, descriptive_value)
    
    filter_for_group <- function(composer_period_group) {
      tbl <- general_audio_values %>% 
        filter(composer_period == composer_period_group,
               track_or_section_value == input$section_or_track,
               descriptive_value_type == input$comparison_value,
               confidence_or_value == input$confidence_or_value) %>% 
        mutate(group = composer_period) %>% 
        select(group, descriptive_value)
      return(tbl)
    }
    
    filter_for_all <- function() {
      tbl <- general_audio_values %>% 
        filter(track_or_section_value == input$section_or_track,
               descriptive_value_type == input$comparison_value,
               confidence_or_value == input$confidence_or_value) %>% 
        mutate(group = "All") %>% 
        select(group, descriptive_value)
      return(tbl)
    }
    
    if(input$first_comparison_group != "All") {
      first_comparison_group_tbl <- filter_for_group(input$first_comparison_group)
    } else {
      first_comparison_group_tbl <- filter_for_all()
    }
    
    if(input$first_comparison_group != "All") {
      second_comparison_group_tbl <- filter_for_group(input$second_comparison_group)
    } else {
      second_comparison_group_tbl <- filter_for_all()
    }
    
    if(input$comparison_value == "Key" & 
       confidence_or_value == "Value") {
      final_tbl <- bind_rows(composer_tbl, 
                             first_comparison_group_tbl, 
                             second_comparison_group_tbl) %>% 
        mutate(descriptive_value = as.factor(descriptive_value),
               descriptive_value = pitch_classes[descriptive_value])
    } else if(input$comparison_value == "Time Signature" & 
              confidence_or_value == "Value") {
      final_tbl <- bind_rows(composer_tbl, 
                             first_comparison_group_tbl, 
                             second_comparison_group_tbl) %>% 
        mutate(descriptive_value = as.factor(descriptive_value),
               descriptive_value = pitch_classes[descriptive_value])
    } else {
      final_tbl <- bind_rows(composer_tbl, 
                             first_comparison_group_tbl, 
                             second_comparison_group_tbl)
    }
    
    return(final_tbl)
    
  })
  
  output$composer <- renderUI({
    imgURL <- filtered_by_composer() %>%
      select(composer_image) %>%
      unique()
    
    tags$img(src = imgURL,
             height = 250,
             width = 235)
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
                           autoplay = "allow")
  })
})