library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyr)
library(tibble)


my_theme <- bs_theme(
  bg = "#F5F1E9",  # Warm off-white background
  fg = "#1A1A1A",  # Darker text for better contrast
  primary = "#654062",  # Deeper purple
  secondary = "#9C3D54",  # Richer rose
  success = "#497174",  # Deeper sage
  info = "#344D67",  # Deeper blue
  font_scale = 0.9,
  heading_font = "Garamond",
  base_font = "Garamond",
  "card-bg" = "#FFFFFF",
  "navbar-bg" = "#F5F1E9",  # Light background
  # Update navbar colors to dark
  "navbar-light-color" = "#1A1A1A",
  "navbar-light-active-color" = "#000000",
  "navbar-light-hover-color" = "#2A2A2A",
  "nav-link-color" = "#1A1A1A",
  "nav-link-hover-color" = "#000000",
  "nav-pills-link-active-color" = "#000000",
  # Update hamburger menu to dark
  "navbar-light-toggler-icon-bg" = "url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 30 30'%3e%3cpath stroke='rgba%280, 0, 0, 0.75%29' stroke-linecap='round' stroke-miterlimit='10' stroke-width='2' d='M4 7h22M4 15h22M4 23h22'/%3e%3c/svg%3e\")",
  "navbar-light-toggler-border-color" = "rgba(0, 0, 0, 0.3)",
  # Make the brand (title) dark
  "navbar-light-brand-color" = "#1A1A1A",
  "navbar-light-brand-hover-color" = "#000000"
)


nostalgic_colors <- c(
  "#654062",  # Deep purple
  "#9C3D54",  # Rich rose
  "#497174",  # Deep sage
  "#344D67",  # Navy blue
  "#B85C38",  # Rust orange
  "#4D4C7D"   # Deep indigo
)

ui <- page_navbar(
  theme = my_theme,
  title = "MindFill: A Brain Dump Analyser",
  nav_panel(
    title = "Overview",
    layout_sidebar(
      sidebar = sidebar(
        fileInput("file", "Upload your emotion journal data (CSV format)"),
        selectInput("emotions", "Select Emotions to Analyze:", 
                    choices = NULL, 
                    multiple = TRUE),
        dateRangeInput("date_range", "Select Date Range:"),
        selectInput("time_group", "Group by:",
                    choices = c("Year", "Month", "Day", "Hour")
        )
      ),
      layout_columns(
        fill = FALSE,
        value_box(
          title = "Average Selected Emotions",
          value = htmlOutput("avg_emotions"),
          theme = "primary"
        ),
        value_box(
          title = "Number of Entries",
          value = textOutput("entry_count"),
          theme = "secondary"
        ),
        value_box(
          title = "Time Period",
          value = textOutput("time_period"),
          theme = "info"
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Emotion Trends Over Time"),
        plotOutput("trend_plot", height = "400px")
      ),
      card(
        full_screen = TRUE,
        card_header("Emotion Distributions"),
        plotOutput("dist_plot", height = "400px")
      )
    )
  ),
  nav_panel(
    title = "Sentence Analysis",
    layout_sidebar(
      sidebar = sidebar(
        dateRangeInput("doc_date_range", "Select Date Range:"),
        selectInput("doc_id", "Select Document ID:", choices = NULL),
        selectInput("doc_emotions", "Select Emotions:", 
                    choices = NULL,
                    multiple = TRUE)
      ),
      card(
        full_screen = TRUE,
        card_header("Sentence Emotion Analysis"),
        DTOutput("doc_analysis")
      ),
      layout_columns(
        card(
          full_screen = TRUE,
          card_header("Sentence Emotion Scores"),
          plotOutput("doc_emotion_plot", height = "400px")
        ),
        card(
          full_screen = TRUE,
          card_header("Emotion Progress Through Document"),
          plotOutput("doc_time_plot", height = "400px")
        )
      )
    )
  ),
  nav_panel(
    title = "Data Explorer",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("plot_type", "Select Plot Type:",
                    choices = c(
                      "Emotions Over Time" = "emotions_time",
                      "Emotion Correlations" = "correlations",
                      "Entries per Day" = "entries_day",
                      "Average Emotions by Hour" = "emotions_hour"
                    )
        ),
        selectInput("explore_emotions", "Select Emotions to Compare:",
                    choices = NULL,
                    multiple = TRUE
        )
      ),
      layout_columns(
        card(
          full_screen = TRUE,
          card_header("Data Visualization"),
          plotOutput("explorer_plot", height = "400px")
        ),
        card(
          full_screen = TRUE,
          card_header("Journal Entries"),
          DTOutput("journal_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, sep="|")
    df$date <- as.Date(df$date)
    return(df)
  })
  
  observe({
    req(data())
    emotion_cols <- names(data())[grep("^[^(text|sentence|date|year|month|day|hour|doc_id)]", names(data()))]
    updateSelectInput(session, "emotions", choices = emotion_cols, selected = emotion_cols[1])
    updateSelectInput(session, "doc_emotions", choices = emotion_cols, selected = emotion_cols[1])
    updateSelectInput(session, "explore_emotions", 
                      choices = emotion_cols,
                      selected = emotion_cols[1:2])
  })
  
  observe({
    req(data())
    updateDateRangeInput(session, "date_range",
                         start = min(data()$date),
                         end = max(data()$date)
    )
    updateDateRangeInput(session, "doc_date_range",
                         start = min(data()$date),
                         end = max(data()$date)
    )
  })
  
  filtered_doc_ids <- reactive({
    req(data(), input$doc_date_range)
    data() %>%
      filter(
        date >= input$doc_date_range[1],
        date <= input$doc_date_range[2]
      ) %>%
      pull(doc_id) %>%
      unique() %>%
      sort()
  })
  
  observe({
    req(filtered_doc_ids())
    updateSelectInput(session, "doc_id", 
                      choices = filtered_doc_ids())
  })
  
  filtered_data <- reactive({
    req(data(), input$date_range)
    data() %>%
      filter(
        date >= input$date_range[1],
        date <= input$date_range[2]
      )
  })
  
  output$avg_emotions <- renderUI({
    req(filtered_data(), input$emotions)
    averages <- sapply(input$emotions, function(emotion) {
      sprintf("<strong>%s</strong>: %.2f%%", emotion, 
              mean(filtered_data()[[emotion]]) * 100)
    })
    HTML(paste(averages, collapse = "<br>"))
  })
  
  output$entry_count <- renderText({
    req(filtered_data())
    n_distinct(filtered_data()$doc_id)
  })
  
  output$time_period <- renderText({
    req(input$date_range)
    paste(format(input$date_range[1], "%b %Y"),
          "to",
          format(input$date_range[2], "%b %Y")
    )
  })
  
  output$trend_plot <- renderPlot({
    req(filtered_data(), input$emotions, input$time_group)
    
    df <- filtered_data()
    group_var <- tolower(input$time_group)
    
    df %>%
      group_by(across(all_of(group_var))) %>%
      summarise(
        across(all_of(input$emotions), mean),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = all_of(input$emotions),
                   names_to = "emotion",
                   values_to = "value") %>%
      ggplot(aes(x = as.factor(get(group_var)), y = value, color = emotion)) +
      geom_line(aes(group = emotion), size = 1) +
      geom_point(size = 3) +
      scale_color_manual(values = nostalgic_colors) +
      labs(
        x = input$time_group,
        y = "Score",
        title = "Emotion Trends Over Time"
      ) +
      theme_minimal() +
      theme(
        text = element_text(family = "Garamond", color = "#1A1A1A"),
        plot.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.grid.major = element_line(color = "#E5E5E5"),
        panel.grid.minor = element_line(color = "#F0F0F0"),
        plot.title = element_text(size = 16, face = "bold", color = "#1A1A1A"),
        axis.text = element_text(color = "#1A1A1A"),
        axis.title = element_text(color = "#1A1A1A"),
        legend.text = element_text(color = "#1A1A1A"),
        legend.title = element_text(color = "#1A1A1A"),
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$dist_plot <- renderPlot({
    req(filtered_data(), input$emotions)
    
    filtered_data() %>%
      select(all_of(input$emotions)) %>%
      pivot_longer(cols = everything(),
                   names_to = "emotion",
                   values_to = "value") %>%
      ggplot(aes(x = value, fill = emotion)) +
      geom_density(alpha = 0.7) +
      scale_fill_manual(values = nostalgic_colors) +
      labs(
        x = "Score",
        y = "Density",
        title = "Distribution of Emotion Scores"
      ) +
      theme_minimal() +
      theme(
        text = element_text(family = "Garamond", color = "#1A1A1A"),
        plot.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.grid.major = element_line(color = "#E5E5E5"),
        panel.grid.minor = element_line(color = "#F0F0F0"),
        plot.title = element_text(size = 16, face = "bold", color = "#1A1A1A"),
        axis.text = element_text(color = "#1A1A1A"),
        axis.title = element_text(color = "#1A1A1A"),
        legend.text = element_text(color = "#1A1A1A"),
        legend.title = element_text(color = "#1A1A1A"),
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$doc_analysis <- renderDT({
    req(data(), input$doc_id, input$doc_emotions)
    
    data() %>%
      filter(doc_id == input$doc_id) %>%
      select(sentence, all_of(input$doc_emotions)) %>%
      arrange(desc(across(all_of(input$doc_emotions[1])))) %>%
      datatable(
        options = list(
          pageLength = 10,
          dom = 'lftip',
          filters = list(position = 'top', clear = TRUE)
        ),
        filter = 'top'
      )
  })
  
  output$doc_emotion_plot <- renderPlot({
    req(data(), input$doc_id, input$doc_emotions)
    
    data() %>%
      filter(doc_id == input$doc_id) %>%
      select(sentence, all_of(input$doc_emotions)) %>%
      pivot_longer(cols = all_of(input$doc_emotions),
                   names_to = "emotion",
                   values_to = "value") %>%
      ggplot(aes(x = value, y = sentence, fill = emotion)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = nostalgic_colors) +
      labs(
        x = "Score",
        y = "Sentence",
        title = "Sentence-Level Emotion Scores"
      ) +
      theme_minimal() +
      theme(
        text = element_text(family = "Garamond", color = "#1A1A1A"),
        plot.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.grid.major = element_line(color = "#E5E5E5"),
        panel.grid.minor = element_line(color = "#F0F0F0"),
        plot.title = element_text(size = 16, face = "bold", color = "#1A1A1A"),
        axis.text = element_text(color = "#1A1A1A"),
        axis.title = element_text(color = "#1A1A1A"),
        legend.text = element_text(color = "#1A1A1A"),
        legend.title = element_text(color = "#1A1A1A"),
        legend.position = "top"
      )
  })
  
  output$doc_time_plot <- renderPlot({
    req(data(), input$doc_id, input$doc_emotions)
    
    data() %>%
      filter(doc_id == input$doc_id) %>%
      mutate(sentence_num = row_number()) %>%
      select(sentence_num, all_of(input$doc_emotions)) %>%
      pivot_longer(cols = all_of(input$doc_emotions),
                   names_to = "emotion",
                   values_to = "value") %>%
      ggplot(aes(x = sentence_num, y = value, color = emotion)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      scale_color_manual(values = nostalgic_colors) +
      labs(
        x = "Sentence Number",
        y = "Score",
        title = "Sentence-Level Emotion Progress"
      ) +
      theme_minimal() +
      theme(
        text = element_text(family = "Garamond", color = "#1A1A1A"),
        plot.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        panel.grid.major = element_line(color = "#E5E5E5"),
        panel.grid.minor = element_line(color = "#F0F0F0"),
        plot.title = element_text(size = 16, face = "bold", color = "#1A1A1A"),
        axis.text = element_text(color = "#1A1A1A"),
        axis.title = element_text(color = "#1A1A1A"),
        legend.text = element_text(color = "#1A1A1A"),
        legend.title = element_text(color = "#1A1A1A"),
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  filtered_table_data <- reactive({
    req(filtered_data())
    if (!is.null(input[["journal_table_rows_all"]])) {
      filtered_data()[input[["journal_table_rows_all"]], ]
    } else {
      filtered_data()
    }
  })
  
  output$journal_table <- renderDT({
    req(filtered_data())
    filtered_data() %>%
      select(doc_id, date, text, sentence, everything()) %>%
      datatable(
        options = list(
          pageLength = 10,
          dom = 'lftip',
          filters = list(position = 'top', clear = TRUE),
          server = TRUE
        ),
        filter = 'top'
      )
  })
  
  output$explorer_plot <- renderPlot({
    req(filtered_table_data(), input$plot_type, input$explore_emotions)
    
    switch(input$plot_type,
           "emotions_time" = {
             filtered_table_data() %>%
               group_by(date) %>%
               summarise(across(all_of(input$explore_emotions), mean)) %>%
               pivot_longer(cols = all_of(input$explore_emotions),
                            names_to = "emotion",
                            values_to = "value") %>%
               ggplot(aes(x = date, y = value, color = emotion)) +
               geom_line(size = 1) +
               geom_point(size = 3, alpha = 0.5) +
               scale_color_manual(values = nostalgic_colors) +
               labs(
                 title = "Emotions Over Time",
                 y = "Average Score",
                 x = "Date"
               ) +
               theme_minimal() +
               theme(
                 text = element_text(family = "Garamond", color = "#1A1A1A"),
                 plot.background = element_rect(fill = "#FFFFFF", color = NA),
                 panel.background = element_rect(fill = "#FFFFFF", color = NA),
                 panel.grid.major = element_line(color = "#E5E5E5"),
                 panel.grid.minor = element_line(color = "#F0F0F0"),
                 plot.title = element_text(size = 16, face = "bold", color = "#1A1A1A"),
                 axis.text = element_text(color = "#1A1A1A"),
                 axis.title = element_text(color = "#1A1A1A"),
                 legend.text = element_text(color = "#1A1A1A"),
                 legend.title = element_text(color = "#1A1A1A"),
                 legend.position = "top",
                 axis.text.x = element_text(angle = 45, hjust = 1)
               )
           },
           "correlations" = {
             filtered_table_data() %>%
               select(all_of(input$explore_emotions)) %>%
               cor() %>%
               as.data.frame() %>%
               rownames_to_column("emotion1") %>%
               pivot_longer(-emotion1, 
                            names_to = "emotion2",
                            values_to = "correlation") %>%
               ggplot(aes(x = emotion1, y = emotion2, fill = correlation)) +
               geom_tile() +
               scale_fill_gradient2(
                 low = "#654062",    # Deep purple
                 mid = "#FFFFFF",    # White
                 high = "#9C3D54",   # Rich rose
                 midpoint = 0,
                 limits = c(-1, 1)
               ) +
               geom_text(
                 aes(label = round(correlation, 2)),
                 family = "Garamond"
               ) +
               labs(title = "Emotion Correlations") +
               theme_minimal() +
               theme(
                 text = element_text(family = "Garamond"),
                 plot.background = element_rect(fill = "#F5F1E9", color = NA),
                 panel.background = element_rect(fill = "#F5F1E9", color = NA),
                 plot.title = element_text(size = 16, face = "bold"),
                 axis.text.x = element_text(angle = 45, hjust = 1),
                 legend.position = "top"
               )
           },
           "entries_day" = {
             filtered_table_data() %>%
               group_by(date) %>%
               summarise(entries = n_distinct(doc_id)) %>%
               ggplot(aes(x = date, y = entries, fill = "Entries")) +  # Added fill aesthetic
               geom_col(position = "dodge") +
               scale_fill_manual(values = c("Entries" = nostalgic_colors[1])) +  # Use first color from nostalgic_colors
               labs(
                 title = "Number of Entries per Day",
                 y = "Number of Entries",
                 x = "Date"
               ) +
               theme_minimal() +
               theme(
                 text = element_text(family = "Garamond", color = "#1A1A1A"),
                 plot.background = element_rect(fill = "#FFFFFF", color = NA),
                 panel.background = element_rect(fill = "#FFFFFF", color = NA),
                 panel.grid.major = element_line(color = "#E5E5E5"),
                 panel.grid.minor = element_line(color = "#F0F0F0"),
                 plot.title = element_text(size = 16, face = "bold", color = "#1A1A1A"),
                 axis.text = element_text(color = "#1A1A1A"),
                 axis.title = element_text(color = "#1A1A1A"),
                 legend.text = element_text(color = "#1A1A1A"),
                 legend.title = element_text(color = "#1A1A1A"),
                 legend.position = "none",  # Hide legend since we only have one category
                 axis.text.x = element_text(angle = 45, hjust = 1)
               )
           },
           "emotions_hour" = {
             filtered_table_data() %>%
               group_by(hour) %>%
               summarise(across(all_of(input$explore_emotions), mean)) %>%
               pivot_longer(cols = all_of(input$explore_emotions),
                            names_to = "emotion",
                            values_to = "value") %>%
               ggplot(aes(x = as.factor(hour), y = value, fill = emotion)) +
               geom_col(position = "dodge") +
               scale_fill_manual(values = nostalgic_colors) +
               labs(
                 title = "Average Emotions by Hour of Day",
                 y = "Average Score",
                 x = "Hour"
               ) +
               theme_minimal() +
               theme(
                 text = element_text(family = "Garamond", color = "#1A1A1A"),
                 plot.background = element_rect(fill = "#FFFFFF", color = NA),
                 panel.background = element_rect(fill = "#FFFFFF", color = NA),
                 panel.grid.major = element_line(color = "#E5E5E5"),
                 panel.grid.minor = element_line(color = "#F0F0F0"),
                 plot.title = element_text(size = 16, face = "bold", color = "#1A1A1A"),
                 axis.text = element_text(color = "#1A1A1A"),
                 axis.title = element_text(color = "#1A1A1A"),
                 legend.text = element_text(color = "#1A1A1A"),
                 legend.title = element_text(color = "#1A1A1A"),
                 legend.position = "top",
                 axis.text.x = element_text(angle = 45, hjust = 1)
               )
           }
    )
  })
}

shinyApp(ui, server)