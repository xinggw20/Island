#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(stringr)

# --- Data Cleaning Function ---
process_census_data <- function(file_path) {
  # Read data with "minimal" name repair to keep original headers for regex matching
  raw_data <- read_csv(file_path, col_types = cols(.default = "c"), 
                       show_col_types = FALSE, name_repair = "minimal")
  
  # Rename the first column
  names(raw_data)[1] <- "Label_Grouping"
  
  # Function to clean race names and prevent duplicates
  clean_race_name <- function(x) {
    x %>%
      # 1. Handle specific duplicate-prone columns first
      str_replace("^Guam!!Total$", "Total Population") %>%
      str_replace("^Guam!!One Race!!Total$", "Total One Race") %>%
      str_replace("^Guam!!Hispanic or Latino \\(of any race\\)!!Total$", "Total Hispanic") %>%
      # 2. General cleanup
      str_remove_all("Guam!!") %>%
      str_remove_all("One Race!!") %>%
      str_remove_all("Hispanic or Latino \\(of any race\\)!!") %>%
      # Replace long name with user requested format
      str_replace_all("Native Hawaiian and Other Pacific Islander", "Native Hawaiian & Pacific Islander") %>%
      str_remove_all("!!Total") %>% # Remove trailing !!Total from sub-groups
      str_replace_all("!!", " - ") %>% # Replace remaining separators with hyphen
      str_remove_all("\\[.*?\\]") %>% # Remove footnotes like [1]
      str_trim()
  }
  
  # Apply cleaning
  current_names <- names(raw_data)
  # Skip the first column (Label_Grouping)
  new_race_names <- sapply(current_names[-1], clean_race_name)
  
  # Ensure names are strictly unique (adds .1, .2 if any duplicates remain)
  final_colnames <- make.unique(c("Label_Grouping", new_race_names))
  names(raw_data) <- final_colnames
  
  # Process Hierarchical Row Labels
  clean_df <- raw_data %>%
    mutate(
      CleanLabel = str_replace_all(Label_Grouping, "\u00A0", " "), # Remove non-breaking spaces
      Indent = nchar(str_match(CleanLabel, "^ *")[, 1]),
      CleanLabel = str_trim(CleanLabel)
    ) %>%
    # Define Sections and SubSections
    mutate(
      Section = ifelse(Indent == 0, CleanLabel, NA),
      SubSection = ifelse(Indent == 4, CleanLabel, NA)
    ) %>%
    fill(Section, .direction = "down") %>%
    group_by(Section) %>%
    fill(SubSection, .direction = "down") %>%
    ungroup() %>%
    # Create Display Label
    mutate(
      DisplayLabel = case_when(
        Indent == 4 ~ CleanLabel, 
        Indent == 8 ~ paste0(SubSection, " - ", CleanLabel),
        Indent > 8 ~ paste0(SubSection, " - ", CleanLabel),
        TRUE ~ CleanLabel
      )
    ) %>%
    filter(Indent > 0) %>% # Remove headers
    # Pivot to Long Format
    pivot_longer(
      cols = -c(Label_Grouping, CleanLabel, Indent, Section, SubSection, DisplayLabel),
      names_to = "Race",
      values_to = "Value"
    ) %>%
    mutate(
      NumericValue = suppressWarnings(as.numeric(str_remove_all(Value, ","))),
      NumericValue = ifelse(is.na(NumericValue), 0, NumericValue)
    )
  
  return(clean_df)
}

# --- Load Data ---
file_name <- "DECENNIALCROSSTABGU2020.CT1-2025-12-08T210638.csv"

# Error handling for file loading
if(file.exists(file_name)) {
  df <- process_census_data(file_name)
} else {
  # Fallback for testing/rendering if file missing
  df <- data.frame() 
  warning("CSV file not found. Please ensure the file is in the working directory.")
}

# --- Shiny UI ---
ui <- fluidPage(
  titlePanel("Guam Census 2020: Demographic Data by Race"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Explore population characteristics (Sex, Age, Marital Status, Fertility) by Race."),
      
      # Topic Selector
      selectInput("topic", "Select Category:", 
                  choices = unique(df$Section)),
      
      # Variable Selector (Updates based on Topic)
      uiOutput("variable_ui"),
      
      # Race Selector
      checkboxGroupInput("selected_races", "Select Races to Compare:",
                         choices = unique(df$Race),
                         selected = c("Total Population", "Native Hawaiian & Pacific Islander", "Asian", "White", "Total One Race"))
    ),
    
    mainPanel(
      plotOutput("barPlot", height = "600px"),
      hr(),
      h4("Data Table"),
      dataTableOutput("dataTable")
    )
  )
)

# --- Shiny Server ---
server <- function(input, output, session) {
  
  # Update Variable choices when Topic changes
  output$variable_ui <- renderUI({
    req(input$topic)
    filtered_data <- df %>% filter(Section == input$topic)
    selectInput("variable", "Select Metric:", 
                choices = unique(filtered_data$DisplayLabel))
  })
  
  # Reactive Data Filtering
  plot_data <- reactive({
    req(input$topic, input$variable, input$selected_races)
    
    df %>%
      filter(
        Section == input$topic,
        DisplayLabel == input$variable,
        Race %in% input$selected_races
      )
  })
  
  # Bar Plot
  output$barPlot <- renderPlot({
    data <- plot_data()
    validate(need(nrow(data) > 0, "Please select at least one race and metric."))
    
    # Updated ggplot: Swapped X and Y mappings
    ggplot(data, aes(x = NumericValue, y = Race, fill = Race)) +
      geom_col() +
      theme_minimal() +
      labs(
        title = paste(input$topic, ":", input$variable),
        x = "Count",             # Swapped label
        y = "Race / Ethnic Group" # Swapped label
      ) +
      theme(
        axis.text.y = element_text(size = 11, face = "bold"), # Race labels are now on Y
        axis.text.x = element_text(size = 12),
        legend.position = "none"
      ) +
      scale_x_continuous(labels = scales::comma) # Apply formatting to X axis (Count)
  })
  
  # Data Table
  output$dataTable <- renderDataTable({
    plot_data() %>%
      select(Category = Section, Metric = DisplayLabel, Race, Count = Value)
  }, options = list(pageLength = 10))
}

# --- Run App ---
shinyApp(ui = ui, server = server)