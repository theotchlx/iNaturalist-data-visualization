# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(treemapify)

# Define UI
ui <- navbarPage("iNaturalist Observations Analysis",
                 tabPanel("Introduction",
                          fluidPage(
                            h2("Goal of this report"),
                            p("This report is an analysis of iNaturalist observations made by one individual. The objective is to uncover patterns and biases in the data that can inform future research and observation strategies."),
                            p("Observations are unevenly distributed depending on the month and the hour of the day, impacting the likelihood of observing certain species. This analysis aims to provide practical insights into observation methods and potential biases."),
                            h2("Data Overview"),
                            verbatimTextOutput("data_summary"),
                            h2("Percentage of Missing Data per Column"),
                            tableOutput("missing_data")
                          )
                 ),
                 tabPanel("Distribution by Time Ranges",
                          tabsetPanel(
                            tabPanel("Hourly Distribution",
                                     fluidPage(
                                       sidebarLayout(
                                         sidebarPanel(
                                           h3("Parameters"),
                                           dateRangeInput("hour_range", "Select Date Range:",
                                                          start = as.Date("2011-01-01"),
                                                          end = as.Date("2023-12-31"),
                                                          min = as.Date("2011-01-01"),
                                                          max = as.Date("2023-12-31"))
                                         ),
                                         mainPanel(
                                           h2("Distribution of Observations by Hour of the Day"),
                                           plotOutput("hour_distribution"),
                                           h2("Proportion of Observations with and without Temporal Data"),
                                           textOutput("missing_hours_hour_distribution")
                                         )
                                       )
                                     )
                            ),
                            tabPanel("Monthly Distribution",
                                     fluidPage(
                                       sidebarLayout(
                                         sidebarPanel(
                                           h3("Parameters"),
                                           dateRangeInput("month_range", "Select Date Range:",
                                                          start = as.Date("2011-01-01"),
                                                          end = as.Date("2023-12-31"),
                                                          min = as.Date("2011-01-01"),
                                                          max = as.Date("2023-12-31"))
                                         ),
                                         mainPanel(
                                           h2("Distribution of observations by month"),
                                           plotOutput("observations_by_month")
                                         )
                                       )
                                     )
                            )
                          )
                 ),
                 tabPanel("Taxonomic Analysis",
                          tabsetPanel(
                            tabPanel("Treemap",
                                     fluidPage(
                                       h2("Distribution of observations by taxonomic group (Treemap)"),
                                       plotOutput("taxonomic_treemap")
                                     )
                            ),
                            tabPanel("Pie Chart",
                                     fluidPage(
                                       h2("Distribution of observations by taxonomic group (Pie Chart)"),
                                       plotOutput("taxonomic_pie_chart", height = "600px")
                                     )
                            )
                          )
                 ),
                 tabPanel("Frequency by Species",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                selectizeInput("species", "Select Species:", choices = NULL, selected = "Linyphiidae", options = list(placeholder = 'Search for a species', maxOptions = 10000))
                              ),
                              mainPanel(
                                h2("Observation of species by Time of Day"),
                                plotOutput("species_correlation_plot"),
                                textOutput("species_observation_count"),
                                h2("Most Frequently Observed Species"),
                                tableOutput("most_observed_species")
                              )
                            )
                          )
                 ),
                 tabPanel("Additional Analysis",
                          tabsetPanel(
                            tabPanel("Granularity Graph",
                                     fluidPage(
                                       sidebarLayout(
                                         sidebarPanel(
                                           sliderInput("week_range", "Select Date Range:",
                                                       min = as.Date("2011-01-01"), 
                                                       max = as.Date("2023-12-31"), 
                                                       value = c(as.Date("2020-01-01"), as.Date("2023-12-31")),
                                                       timeFormat = "%Y-%m-%d")
                                         ),
                                         mainPanel(
                                           h2("Quantity of Observations in a Year by Month, Week, and Day"),
                                           plotOutput("time_granularity_graph"),
                                           h3("Due to technical limitations, this diagram is experimental and should not be used for research purposes."),
                                         )
                                       )
                                     )
                            )
                          )
                 )
)

# Define server logic
server <- function(input, output, session) {
  # Load the data
  observations <- read.csv("data/observations.csv")
  
  # Ensure 'time_observed_at' and 'observed_on' are properly formatted
  observations$time_observed_at <- ymd_hms(observations$time_observed_at)
  observations$observed_on <- ymd(observations$observed_on)
  
  # Calculate the number of observations
  num_observations <- nrow(observations)
  
  # Calculate the number of observed species
  num_species <- observations %>% filter(!is.na(scientific_name)) %>% pull(scientific_name) %>% unique() %>% length()
  
  # Summary of data
  output$data_summary <- renderText({
    paste("This dataset contains", num_observations, "observations on", num_species, "species.")
  })
  
  # Calculate the percentage of missing data for each column
  missing_data <- sapply(observations, function(x) mean(is.na(x)) * 100)
  missing_data <- data.frame(Column = names(missing_data), MissingPercentage = missing_data, row.names = NULL)
  
  # Display the missing data table
  output$missing_data <- renderTable({
    missing_data
  }, colnames = TRUE, digits = 2)
  
  # Extract hour from time_observed_at
  observations$hour <- hour(observations$time_observed_at)
  
  # Calculate the counts of NA and non-NA values in the hours column
  na_count <- sum(is.na(observations$hour))
  total_count <- nrow(observations)
  na_percentage <- (na_count / total_count) * 100
  
  # Display the proportion of missing hour data in Distribution by Hour tab
  output$missing_hours_hour_distribution <- renderText({
    paste(round(na_percentage, 2), "% of data concerning the hour of the observation is not given.")
  })
  
  # Filter observations based on user input for Distribution by Hour tab
  filtered_observations <- reactive({
    obs <- observations %>% filter(!is.na(hour))
    obs <- obs %>% filter(observed_on >= input$hour_range[1] & observed_on <= input$hour_range[2])
    obs
  })
  
  # Plot the distribution of observations by hour for Hourly Distribution tab
  output$hour_distribution <- renderPlot({
    num_bins <- length(unique(filtered_observations()$hour))
    my_colors <- c("lightsteelblue", "steelblue")[1:num_bins %% 2 + 1]
    
    ggplot(filtered_observations(), aes(x = hour)) +
      geom_bar(fill = my_colors) +
      labs(title = "Distribution of Observations by Hour of the Day",
           x = "Hour of the Day",
           y = "Number of Observations") +
      theme_minimal()
  })
  
  # Filter observations based on date range input for Monthly Distribution tab
  filtered_observations_by_month <- reactive({
    obs <- observations %>% filter(observed_on >= input$month_range[1] & observed_on <= input$month_range[2])
    obs$month <- month(obs$observed_on, label = TRUE, abbr = TRUE)
    obs
  })
  
  # Plot the distribution of observations by month for Monthly Distribution tab
  output$observations_by_month <- renderPlot({
    num_bins <- length(unique(filtered_observations_by_month()$month))
    my_colors <- c("lightsteelblue", "steelblue")[1:num_bins %% 2 + 1]
    
    ggplot(filtered_observations_by_month(), aes(x = month)) +
      geom_bar(fill = my_colors) +
      labs(title = "Distribution of observations by month",
           x = "Month",
           y = "Number of observations") +
      theme_minimal()
  })
  
  # Filter observations based on date range input for Granularity Graph tab
  filtered_observations_by_date <- reactive({
    obs <- observations %>% filter(observed_on >= input$week_range[1] & observed_on <= input$week_range[2])
    obs
  })
  
  # Plot the quantity of observations by month, week, and day for Granularity Graph tab
  output$time_granularity_graph <- renderPlot({
    filtered_data <- filtered_observations_by_date()
    
    # Aggregate data by month, week, and day
    filtered_data <- filtered_data %>% 
      mutate(month = floor_date(observed_on, "month"),
             week = floor_date(observed_on, "week"),
             day = floor_date(observed_on, "day"))
    
    # Summarize counts by month, week, and day
    monthly_data <- filtered_data %>% group_by(month) %>% summarise(count = n())
    weekly_data <- filtered_data %>% group_by(week) %>% summarise(count = n())
    daily_data <- filtered_data %>% group_by(day) %>% summarise(count = n())
    
    # Plot the data
    ggplot() +
      geom_bar(data = monthly_data, aes(x = month, y = count), stat = "identity", fill = "steelblue", alpha = 0.7) +
      geom_bar(data = weekly_data, aes(x = week, y = count), stat = "identity", fill = "darkorange", alpha = 0.5) +
      geom_line(data = daily_data, aes(x = day, y = count), color = "darkred") +
      labs(title = "Quantity of Observations in a Year by Month, Week, and Day",
           x = "Date",
           y = "Number of Observations") +
      theme_minimal()
  })
  
  # Plot the distribution of observations by taxonomic group (Treemap) for Taxonomic Analysis tab
  output$taxonomic_treemap <- renderPlot({
    # Prepare data for treemap
    taxonomic_data <- observations %>% 
      filter(!is.na(iconic_taxon_name) & iconic_taxon_name != "") %>% 
      count(iconic_taxon_name)
    
    # Plot the treemap
    ggplot(taxonomic_data, aes(area = n, fill = iconic_taxon_name, label = iconic_taxon_name)) +
      geom_treemap() +
      geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
      labs(title = "Distribution of observations by taxonomic group (Treemap)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Plot the distribution of observations by taxonomic group (Pie Chart) for Taxonomic Analysis tab
  output$taxonomic_pie_chart <- renderPlot({
    # Calculate percentage for pie chart and reorder levels
    taxonomic_data <- observations %>% 
      filter(!is.na(iconic_taxon_name) & iconic_taxon_name != "") %>% 
      count(iconic_taxon_name) %>%
      mutate(percentage = n / sum(n) * 100,
             iconic_taxon_name = factor(iconic_taxon_name, levels = iconic_taxon_name[order(-percentage)]))
    
    # Plot the pie chart
    pie <- ggplot(taxonomic_data, aes(x = "", y = percentage, fill = iconic_taxon_name)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y") +
      labs(title = "Distribution of observations by taxonomic group (Pie Chart)") +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank()) +
      scale_fill_manual(values = scales::hue_pal()(length(unique(taxonomic_data$iconic_taxon_name))),
                        labels = paste(levels(taxonomic_data$iconic_taxon_name), 
                                       "(", round(taxonomic_data$percentage[order(-taxonomic_data$percentage)], 1), "%)", sep = "")) +
      guides(fill = guide_legend(title = "Taxon Name"))
    
    pie
  })
  
  # Update species dropdown choices
  updateSelectInput(session, "species", choices = sort(unique(observations$scientific_name[!is.na(observations$scientific_name)])), selected = "Linyphiidae")
  
  # Plot the correlation between time of day and species observations for Frequency by Species tab
  output$species_correlation_plot <- renderPlot({
    req(input$species)
    
    species_data <- observations %>% filter(scientific_name == input$species)
    
    # Check if there are valid hour data for the selected species
    if (nrow(species_data) == 0) {
      return(NULL)
    }
    
    num_bins <- length(unique(species_data$hour))
    my_colors <- c("lightsteelblue", "steelblue")[1:num_bins %% 2 + 1]
    
    ggplot(species_data, aes(x = hour)) +
      geom_histogram(binwidth = 1, fill = my_colors, na.rm = TRUE) +
      labs(title = paste("Observation of", input$species,"by Time of Day"),
           x = "Hour of the Day",
           y = "Number of Observations") +
      theme_minimal()
  })
  
  # Number of times a species has been observed
  output$species_observation_count <- renderText({
    req(input$species)
    species_data <- observations %>% filter(scientific_name == input$species)
    paste("Number of observations for", input$species, ":", nrow(species_data))
  })
  
  # Most frequently observed species
  output$most_observed_species <- renderTable({
    species_counts <- observations %>%
      filter(!is.na(scientific_name)) %>%
      group_by(scientific_name) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    head(species_counts, 20)
  }, colnames = TRUE, digits = 0)
}

# Run the application
shinyApp(ui = ui, server = server)
