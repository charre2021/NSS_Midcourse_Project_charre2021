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
    single_composer_tbl <- filtered_by_composer() %>%
      filter(track_or_section_value == input$section_or_track,
             descriptive_value_type == input$comparison_value,
             confidence_or_value == input$confidence_or_value)
    if(input$section_or_track == "Track") {
      single_composer_tbl <- single_composer_tbl %>% 
        select(-section_start, -section_duration) %>%
        unique() %>%
        mutate(composer_group = composer) %>%
        select(composer_group, descriptive_value)
    } else {
      single_composer_tbl <- single_composer_tbl %>% 
        mutate(composer_group = composer) %>%
        select(composer_group, descriptive_value)
    }
    return(single_composer_tbl)
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
  
  split_tibble <- reactive({
    initial_split_tibble <- logreg_pt_tibble %>% 
      mutate(log_value = factor(if_else(composer_period == input$logreg_comparison_group,
                                        TRUE,
                                        FALSE), 
                                levels = c(TRUE, FALSE))) %>%
      select(log_value, c(input$logreg_variables)) %>% 
      initial_split()
    
    return(initial_split_tibble)
  })
  
  testing_set <- reactive({
    split_tibble() %>% 
      testing()
  })
  
  training_set <- reactive({
    split_tibble() %>% 
      training()
  })
  
  logreg_model <- reactive({
    logistic_reg() %>% 
      set_engine("glm") %>% 
      set_mode("classification") %>% 
      fit(log_value~.,data = training_set())
  })
  
  logreg_results <- reactive({
    logreg_results <- testing_set() %>% 
      select(log_value) %>% 
      bind_cols(predict(logreg_model(),
                        new_data = testing_set(),
                        type = "class"))
    
    logreg_results <- confusionMatrix(logreg_results$.pred_class,
                                      logreg_results$log_value)$table %>%
      as_tibble() %>% 
      mutate(Match = if_else(Reference == Prediction, TRUE, FALSE)) %>% 
      rename("Frequency" = "n") %>% 
      group_by(Reference)
    
    return(logreg_results)
  })
  
  logreg_probabilities <- reactive({
    testing_set() %>% 
      select(log_value) %>% 
      bind_cols(predict(logreg_model(),
                        new_data = testing_set(),
                        type = "prob"))
  })
  
  
  logreg_calibration <- reactive({
    calibration(log_value ~ .pred_TRUE,
                data = logreg_probabilities(),
                cuts = 10)
  })
  
  gc_plot_data <- reactive({
    gain_curve(logreg_probabilities(),
               truth = log_value,
               estimate = .pred_TRUE)
  })
  
  roc_curve_data <- reactive({
    roc_curve(logreg_probabilities(),
              truth = log_value,
              estimate = .pred_TRUE)
  })
  
  output$show_curve <- renderPlot({
    
    if(input$curve_type == "Calibration") {
      calibration_curve <- ggplot(logreg_calibration()) + 
        labs(title = "Calibration Curve for Regression Model") + 
        scale_y_continuous(limits = c(0,100)) +
        scale_x_continuous(limits = c(0,100)) + 
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              text = element_text(size = 15,
                                  family = "Baskervville",
                                  color = "#000000"),
              panel.background = element_rect(fill = "#fafafa",
                                              colour = "#000000"),
              panel.grid = element_blank(),
              plot.background = element_rect(fill = "#fafafa", color = NA),
              plot.margin = margin(t = 10, r = 10, b = 0, l = 0))
      
      plot(calibration_curve)
      
    } else if(input$curve_type == "Gain") {
      gc_plot <- ggplot(gc_plot_data(), aes(.percent_tested, .percent_found)) + 
        geom_line(color = "black") + 
        geom_abline(intercept = 0, 
                    slope = 1, 
                    linetype = "dashed",
                    color = "black") + 
        geom_ribbon(aes(x = .percent_tested, 
                        ymin = .percent_tested, 
                        ymax = .percent_found),
                    fill = "#000029",
                    alpha = 0.5) + 
        scale_x_continuous(limits = c(-1,100),
                           expand = c(0,0)) +
        scale_y_continuous(limits = c(-1,105),
                           expand = c(0,0)) + 
        labs(y = "% Found",
             x = "% Tested",
             title = "Gain Curve for Regression Model") + 
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              text = element_text(size = 15,
                                  family = "Baskervville",
                                  color = "#000000"),
              panel.background = element_rect(fill = "#fafafa",
                                              colour = "#000000"),
              panel.grid = element_blank(),
              plot.background = element_rect(fill = "#fafafa", color = NA),
              plot.margin = margin(t = 10, r = 10, b = 0, l = 0))
      
      plot(gc_plot)
      
    } else {
      ggplot(roc_curve_data(), aes(x = 1 - specificity, y = sensitivity)) +
        geom_line(color = "black") + 
        geom_abline(intercept = 0, 
                    slope = 1, 
                    linetype = "dashed",
                    color = "black") +
        geom_ribbon(aes(x = 1 - specificity, 
                        ymin = 1 - specificity, 
                        ymax = sensitivity),
                    fill = "#000029",
                    alpha = 0.5) + 
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) + 
        labs(y = "Sensitivity",
             x = "Specificity",
             title = "ROC Curve for Regression Model") + 
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              text = element_text(size = 15,
                                  family = "Baskervville",
                                  color = "#000000"),
              panel.background = element_rect(fill = "#fafafa",
                                              colour = "#000000"),
              panel.grid = element_blank(),
              plot.background = element_rect(fill = "#fafafa", color = NA),
              plot.margin = margin(t = 10, r = 10, b = 0, l = 0))
    }
  })
  
  output$logreg_table <- renderDT(
    caption = tags$caption(HTML("<h4><b>Logistic Regression Results</b></h4>"),
                           style = "color: black; text-align: Center;"),
    rownames = FALSE,
    options = list(columnDefs = list(list(className = 'dt-right', 
                                          targets = 0:4))),
    {
      tidy(logreg_model(), exponentiate = TRUE) %>% 
        mutate(term = sapply(term, 
                             function(x) str_replace_all(x,c("_"=" ","`" = "")))) %>% 
        mutate_if(is.numeric, ~round(.,2)) %>% 
        rename("Term" = "term",
               "Estimate" = "estimate",
               "Standard Error" = "std.error",
               "Statistic" = "statistic",
               "P-Value" = "p.value")
    })
  
  output$confusion_matrix <- renderPlot({
    ggplot(logreg_results(), 
           aes(x = factor(Reference, levels = c("TRUE", "FALSE")), 
               y = Prediction, 
               fill = Match, 
               alpha = Frequency)) +
      geom_tile() +
      scale_fill_manual(values = c('#87ebff','#ff8787')) + 
      geom_text(aes(x = Reference,
                    y = Prediction,
                    label = Frequency),
                size = 5,
                family = "Baskervville",
                inherit.aes = FALSE) +
      labs(title = "Confusion Matrix for Regression Model",
           x = "Actual") + 
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
  })
  
  
  output$comparison_density <- renderPlot({
    if (("Loudness" %in% input$comparison_value) &
        ("Confidence" %in% input$confidence_or_value) |
        (input$first_comparison_group == input$second_comparison_group)) {
      return()
    }
    
    density_plot <- density_filter() %>% 
      ggplot(aes(x = descriptive_value, fill = composer_group)) + 
      geom_density(alpha = 0.5)
    
    density_plot <- density_plot +
      scale_y_continuous(expand = expansion(mult = c(0,0.1))) + 
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
            plot.margin = margin(t = 0, r = 10, b = 0, l = 10),
            legend.margin = margin(t = 0, r = 0, b = 0, l = 0))
    
    if (("Key" %in% input$comparison_value) &
        ("Value" %in% input$confidence_or_value)) {
      density_plot <- density_plot +
        scale_x_continuous(breaks = c(0:11),
                           labels = pitch_classes,
                           expand = expansion(mult = c(0,0)),
                           limits = c(0,11))
    } else if (("Time Signature" %in% input$comparison_value) &
               ("Value" %in% input$confidence_or_value)) {
      density_plot <- density_plot +
        scale_x_continuous(breaks = c(1,3:7),
                           labels = time_signature_classes,
                           expand = expansion(mult = c(0,0)),
                           limits = c(0.5,7.5)) + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    } else {
      density_plot <- density_plot +
        scale_x_continuous(expand = expansion(mult = c(0,0)))
    }
    
    dv_mean_mode <- mean_mode(density_filter()$descriptive_value)
    range_max <- max(density_filter()$descriptive_value)
    range_min <- min(density_filter()$descriptive_value)
    range_portion <- (range_max - range_min)/2.90
    
    if(dv_mean_mode < range_portion) {
      
      density_plot <- density_plot +
        theme(legend.position = c(0.985, .985),
              legend.justification = c("right", "top"),
              legend.box.just = "right")
    } else {
      density_plot <- density_plot +
        theme(legend.position = c(0.015, .985),
              legend.justification = c("left", "top"),
              legend.box.just = "left")
    }
    
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
        ylim(-scale * 1.05, max(score_points) * 1.25) +
        coord_polar(start = -pi/12) +
        geom_text(aes(x = class,
                      y = -scale/3.5,
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
                           height = "380",
                           allow = "encrypted-media")
  })
})