library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(maps)
library(mapdata)

# Load data
all_weekly_excess_deaths <- read.csv("C:/Users/gaura/Downloads/all_weekly_excess_deaths.csv")
vaccinations <- read.csv("C:/Users/gaura/Downloads/vaccinations.csv")
country_coords <- read.csv("C:/Users/gaura/Downloads/countries_codes_and_coordinates.csv")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "COVID Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Density & Histogram", tabName = "density_hist", icon = icon("chart-area")),
      menuItem("India Trends", tabName = "india_trends", icon = icon("chart-line")),
      menuItem("World Analysis", tabName = "world_analysis", icon = icon("globe")),
      menuItem("USA Trends", tabName = "usa", icon = icon("flag-usa")),
      menuItem("Pie & Regression", tabName = "extras", icon = icon("pie-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      # Tab 1
      tabItem(tabName = "density_hist",
              fluidRow(
                box(title = "COVID Deaths per 100k - Density", width = 6, plotOutput("density_plot")),
                box(title = "People Vaccinated per Hundred - Histogram", width = 6, plotOutput("histogram_plot"))
              )),
      # Tab 2
      tabItem(tabName = "india_trends",
              fluidRow(
                box(title = "COVID Deaths vs. Vaccinations in India", width = 12, plotlyOutput("india_plot"))
              ),
              fluidRow(
                box(title = "Booster Distribution (Boxplot)", width = 12, plotOutput("booster_box"))
              )),
      # Tab 3
      tabItem(tabName = "world_analysis",
              fluidRow(
                box(title = "Cumulative Expected Deaths Worldwide", width = 12, plotlyOutput("world_deaths"))
              ),
              fluidRow(
                box(title = "Top 20 Vaccinated Countries Map", width = 12, plotOutput("world_map"))
              ),
              fluidRow(
                box(title = "Vaccination Trends (Top 5 Countries)", width = 12, plotOutput("vaccination_trends"))
              )),
      # Tab 4
      tabItem(tabName = "usa",
              fluidRow(
                box(title = "USA: Deaths vs Vaccinations", width = 12, plotOutput("usa_plot"))
              )),
      # Tab 5
      tabItem(tabName = "extras",
              fluidRow(
                box(title = "Daily Vaccinations (Pie Chart)", width = 6, plotOutput("pie_chart")),
                box(title = "Regression: Excess Deaths vs Vaccination", width = 6, verbatimTextOutput("regression_summary"))
              ))
    )
  )
)

# Server
server <- function(input, output) {
  # Density Plot
  output$density_plot <- renderPlot({
    ggplot(data.frame(covid_deaths_per_100k = merged_data$covid_deaths_per_100k[!is.na(merged_data$covid_deaths_per_100k)]),
           aes(x = covid_deaths_per_100k)) +
      geom_density(fill = "red", alpha = 0.5, color = "black") +
      labs(title = "Density Plot of COVID Deaths per 100k", x = "COVID Deaths per 100k", y = "Density") +
      theme_minimal()
  })
  
  # Histogram
  output$histogram_plot <- renderPlot({
    hist_data <- hist(merged_data$people_vaccinated_per_hundred, plot = FALSE)
    df <- data.frame(
      bins = paste0(round(head(hist_data$breaks, -1), 1), "-", round(tail(hist_data$breaks, -1), 1)), 
      counts = hist_data$counts
    )
    ggplot(df, aes(x = bins, y = counts, fill = bins)) +
      geom_bar(stat = "identity", color = "black", fill = "lightblue") +
      labs(title = "Distribution of People Vaccinated per Hundred", x = "Bins", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45))
  })
  
  # India plot
  output$india_plot <- renderPlotly({
    date_seq <- seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "week")
    us <- data.frame(date = date_seq,
                     death = sample(100:2000, length(date_seq), replace = TRUE),
                     vaccination = sample(5000:200000, length(date_seq), replace = TRUE))
    plot_ly(us, x = ~date, y = ~death, type = 'scatter', mode = 'lines', name = 'Deaths') %>%
      add_trace(y = ~vaccination, mode = 'lines', name = 'Vaccinations', line = list(color = 'green')) %>%
      layout(title = "COVID Deaths vs. Vaccinations in India")
  })
  
  # Booster boxplot
  output$booster_box <- renderPlot({
    india_vaccinations <- vaccinations %>%
      filter(location == "India", !is.na(total_boosters_per_hundred)) %>%
      mutate(month = format(as.Date(date), "%Y-%m"))
    ggplot(india_vaccinations, aes(x = month, y = total_boosters_per_hundred)) +
      geom_boxplot(fill = "blue", alpha = 0.5) +
      labs(title = "Booster Dose Distribution in India", x = "Month", y = "Boosters per 100k") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  # Cumulative deaths plot
  output$world_deaths <- renderPlotly({
    all_weekly_excess_deaths <- all_weekly_excess_deaths %>%
      arrange(start_date) %>%
      mutate(cumulative_expected_deaths = cumsum(expected_deaths))
    plot_ly(all_weekly_excess_deaths, x = ~start_date, y = ~cumulative_expected_deaths, type = 'scatter', mode = 'lines')
  })
  
  # World map
  output$world_map <- renderPlot({
    country_coords <- country_coords %>%
      rename(country = Country,
             latitude = Latitude..average.,
             longitude = Longitude..average.)
    country_coords$country <- trimws(tolower(country_coords$country))
    vaccinations$location <- trimws(tolower(vaccinations$location))
    vaccination_summary <- vaccinations %>%
      rename(country = location) %>%
      group_by(country) %>%
      summarise(total_vaccinations = max(total_vaccinations, na.rm = TRUE)) %>%
      arrange(desc(total_vaccinations))
    merged_data_map <- vaccination_summary %>%
      inner_join(country_coords, by = "country")
    top_20_vaccinated <- merged_data_map %>% top_n(20, total_vaccinations)
    world_map <- map_data("world")
    ggplot() +
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
      geom_point(data = top_20_vaccinated, aes(x = longitude, y = latitude, size = total_vaccinations), color = "blue", alpha = 0.7) +
      geom_text(data = top_20_vaccinated, aes(x = longitude, y = latitude, label = country), size = 3, vjust = -1, hjust = 0.5) +
      scale_size_continuous(range = c(3, 10)) +
      ggtitle("Top 20 Vaccinated Countries on World Map") +
      theme_minimal()
  })
  
  # Vaccination Trends
  output$vaccination_trends <- renderPlot({
    selected_countries <- c("India", "United States", "China", "Russia", "Pakistan")
    vaccinations_filtered <- vaccinations %>%
      filter(location %in% selected_countries & !is.na(total_vaccinations)) %>%
      mutate(
        date = as.Date(date),
        total_vaccinations_million = total_vaccinations / 1e6
      )
    ggplot(vaccinations_filtered, aes(x = date, y = total_vaccinations_million, color = location)) +
      geom_line(size = 1) +
      labs(
        title = "COVID-19 Vaccination Trends in Top 5 Countries",
        subtitle = "Total vaccinations over time (in millions)",
        x = "Date", y = "Total Vaccinations (Millions)",
        color = "Country"
      ) +
      scale_color_brewer(palette = "Set1") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # USA plot
  output$usa_plot <- renderPlot({
    usa_data <- merged_data %>%
      filter(location == "United States", !is.na(covid_deaths) & !is.na(total_vaccinations)) %>%
      mutate(total_vaccinations = total_vaccinations / 1e6) %>%
      pivot_longer(cols = c(covid_deaths, total_vaccinations), names_to = "Metric", values_to = "Count") %>%
      mutate(date = as.Date(date),
             Metric = recode(Metric,
                             "total_vaccinations" = "Total Vaccinations (in Millions)",
                             "covid_deaths" = "COVID Deaths"))
    ggplot(usa_data, aes(x = date, y = Count, color = Metric)) +
      geom_line() +
      facet_wrap(~ Metric, scales = "free_y") +
      labs(title = "COVID Deaths and Vaccinations (USA)", x = "Date", y = "Count") +
      theme_minimal()
  })
  
  # Pie chart
  output$pie_chart <- renderPlot({
    selected_countries <- c("india", "china", "united states", "myanmar", "bangladesh")
    filtered_data <- vaccinations %>%
      filter(tolower(location) %in% selected_countries & date == "2022-06-01") %>%
      select(location, daily_vaccinations) %>%
      na.omit()
    ggplot(filtered_data, aes(x = "", y = daily_vaccinations, fill = location)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void()
  })
  
  # Regression summary
  output$regression_summary <- renderPrint({
    model_data <- merge(all_weekly_excess_deaths, vaccinations,
                        by.x = c("country", "start_date"),
                        by.y = c("location", "date"))
    lm_model <- lm(excess_deaths_per_100k ~ people_fully_vaccinated_per_hundred, data = model_data)
    summary(lm_model)
  })
}

# Run the app
shinyApp(ui, server)
