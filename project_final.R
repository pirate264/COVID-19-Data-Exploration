# Load necessary libraries
library(ggplot2)      # For data visualization
library(dplyr)        # For data manipulation
library(tidyr)        # For data cleaning and reshaping
library(plotly)       # For interactive plots
library(maps)         # For map visualization
library(mapdata)      # For additional map data

# Read datasets
all_weekly_excess_deaths <- read.csv("C:/Users/gaura/Downloads/all_weekly_excess_deaths.csv")
vaccinations <- read.csv("C:/Users/gaura/Downloads/vaccinations.csv")
country_coords <- read.csv("C:/Users/gaura/Downloads/countries_codes_and_coordinates.csv")

# Display column names to understand dataset structure
colnames(all_weekly_excess_deaths)
colnames(vaccinations)

# Merge datasets based on country and date
merged_data <- merge(vaccinations, all_weekly_excess_deaths, 
                     by.x = c("location", "date"), 
                     by.y = c("country", "start_date"))
colnames(merged_data)
# ---- 1. Density Plot of COVID Deaths Per 100k ----
ggplot(data.frame(covid_deaths_per_100k = merged_data$covid_deaths_per_100k[!is.na(merged_data$covid_deaths_per_100k)]), 
       aes(x = covid_deaths_per_100k)) +
  geom_density(fill = "red", alpha = 0.5, color = "black") +
  labs(title = "Density Plot of COVID Deaths per 100k",
       x = "COVID Deaths per 100k",
       y = "Density") +
  theme_minimal()

# ---- 2. Histogram: People Vaccinated Per Hundred ----
hist_data <- hist(merged_data$people_vaccinated_per_hundred, plot = FALSE)

# Create a data frame for plotting
df <- data.frame(
  bins = paste0(round(head(hist_data$breaks, -1), 1), "-", round(tail(hist_data$breaks, -1), 1)), 
  counts = hist_data$counts
)

# Plot histogram as bar chart
ggplot(df, aes(x = bins, y = counts, fill = bins)) +
  geom_bar(stat = "identity", color = "black", fill = "lightblue") +
  labs(title = "Distribution of People Vaccinated per Hundred",
       x = "People Vaccinated per Hundred (Binned)", 
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- 3. COVID Deaths vs. Vaccinations in India (Interactive) ----
# Generate weekly date sequence
date_seq <- seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "week")

# Simulated dataset for plotting
us <- data.frame(
  date = date_seq,  
  death = sample(100:2000, length(date_seq), replace = TRUE),  
  vaccination = sample(5000:200000, length(date_seq), replace = TRUE)  
)

# Interactive Plot: Deaths and Vaccinations over time
fig1 <- plot_ly(us, x = ~date, y = ~death, type = 'scatter', mode = 'lines', name = 'Deaths') %>%
  add_trace(y = ~vaccination, mode = 'lines', name = 'Vaccinations', line = list(color = 'green')) %>%
  layout(title = "COVID Deaths vs. Vaccinations in India",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Count"),
         hovermode = "x unified")
fig1

# ---- 4. Cumulative Frequency of Expected Deaths (World) ----
all_weekly_excess_deaths <- all_weekly_excess_deaths %>%
  arrange(start_date) %>%
  mutate(cumulative_expected_deaths = cumsum(expected_deaths))

fig2 <- plot_ly(all_weekly_excess_deaths, x = ~start_date, y = ~cumulative_expected_deaths, 
                type = 'scatter', mode = 'lines', name = 'Cumulative Expected Deaths', 
                line = list(color = 'blue')) %>%
  layout(title = "Cumulative Frequency of Expected Deaths in the World",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Cumulative Expected Deaths"),
         hovermode = "x unified")
fig2

# ---- 5. Total COVID Vaccinations Over Time (Selected Countries) ----
selected_countries <- c("India", "USA", "China", "Russia", "Pakistan")

vaccinations_filtered <- vaccinations %>%
  filter(location %in% selected_countries) %>%
  mutate(total_vaccinations_million = replace_na(total_vaccinations, 0) / 1e6)

ggplot(vaccinations_filtered, aes(x = date, y = total_vaccinations_million, color = location)) +
  geom_line(linewidth = 1) +
  labs(title = "Total COVID Vaccinations Over Time (in Millions)",
       x = "Date", y = "Total Vaccinations (Millions)",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "bottom")

# ---- 6. Distribution of Total Boosters Per 100k in India (Box Plot) ----
india_vaccinations <- vaccinations %>%
  filter(location == "India", date >= as.Date("2021-01-01") & date <= as.Date("2023-12-31"),
         !is.na(total_boosters_per_hundred)) %>%
  mutate(month = format(date, "%Y-%m"))

ggplot(india_vaccinations, aes(x = month, y = total_boosters_per_hundred)) +
  geom_boxplot(fill = "blue", alpha = 0.5, outlier.color = "red") +
  labs(title = "Distribution of Total Boosters Per 100k in India (2021-2023)",
       x = "Month", y = "Total Boosters Per 100k") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ---- 7. Worldmap: top 20 most People Vaccinated country ----
# Rename columns in country_coords
country_coords <- country_coords %>%
  rename(
    country = Country,
    latitude = Latitude..average.,
    longitude = Longitude..average.
  )

# Clean country names
country_coords$country <- trimws(tolower(country_coords$country))
vaccinations$location <- trimws(tolower(vaccinations$location))

# Summarize total vaccinations per country
vaccination_summary <- vaccinations %>%
  rename(country = location) %>%  # Rename 'location' to 'country'
  group_by(country) %>%
  summarise(total_vaccinations = max(total_vaccinations, na.rm = TRUE)) %>%
  arrange(desc(total_vaccinations))  # Sort by highest vaccinations

# Merge with coordinates
merged_data <- vaccination_summary %>%
  inner_join(country_coords, by = "country") 

# Select top 20 vaccinated countries
top_20_vaccinated <- merged_data %>% top_n(20, total_vaccinations)

# Get world map data
world_map <- map_data("world")

# Plot world map with top 20 vaccinated countries and labels
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_point(data = top_20_vaccinated, aes(x = longitude, y = latitude, size = total_vaccinations),
             color = "blue", alpha = 0.7) +
  geom_text(data = top_20_vaccinated, aes(x = longitude, y = latitude, label = country),
            size = 3, vjust = -1, hjust = 0.5, color = "black") +  # Add country names
  scale_size_continuous(range = c(3, 10)) +  # Adjust point size
  ggtitle("Top 20 Vaccinated Countries on World Map") +
  theme_minimal()

# ---- 8. In USA:  Deaths vs. Vaccination  ----
# Filter dataset for United States only
usa_data <- merged_data %>%
  filter(location == "United States") %>%  # Keep only USA
  select(date, location, covid_deaths, total_vaccinations) %>%  # Select relevant columns
  filter(!is.na(covid_deaths) & !is.na(total_vaccinations))  # Remove missing values

# Convert total vaccinations to millions
usa_data <- usa_data %>%
  mutate(total_vaccinations = total_vaccinations / 1e6)  # Convert to millions

# Reshape data for plotting
data_long <- usa_data %>%
  pivot_longer(cols = c(covid_deaths, total_vaccinations), names_to = "Metric", values_to = "Count")

# Convert date to Date format
data_long$date <- as.Date(data_long$date)

# Update labels for better readability
data_long$Metric <- recode(data_long$Metric,
                           "total_vaccinations" = "Total Vaccinations (in Millions)",
                           "covid_deaths" = "COVID Deaths")

# Plot deaths and vaccinations over time for USA with specific colors
ggplot(data_long, aes(x = date, y = Count, color = Metric)) +
  geom_line(size = 1) +  # Line plot
  scale_color_manual(values = c("COVID Deaths" = "red", "Total Vaccinations (in Millions)" = "green")) +  # Assign colors
  facet_wrap(~ Metric, scales = "free_y") +  # Separate panels for Deaths and Vaccinations
  labs(title = "COVID Deaths and Vaccinations Over Time (United States)",
       x = "Date", y = "Count", color = "Metric") +
  theme_minimal()


# ---- 9. Daily Vaccinations: India, china, united states, Myanmar, Bangladesh ----
#  selected countries
selected_countries <- c("india", "china", "united states", "myanmar", "bangladesh")

# Define the target date (Change this to the date you want)
target_date <- "2022-06-01"

# Filter data for selected countries and the target date
filtered_data <- vaccinations %>%
  filter(tolower(location) %in% selected_countries & date == target_date) %>%
  select(location, daily_vaccinations) %>%
  na.omit()  # Remove missing values

# Create Pie Chart
ggplot(filtered_data, aes(x = "", y = daily_vaccinations, fill = location)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +  # Convert to pie chart
  labs(title = paste("Daily Vaccinations on", target_date)) +
  theme_void() +  # Remove background elements
  theme(legend.position = "right")  # Move legend to right


# ---- 10. Linear Regression: Excess Deaths vs. Vaccination Rates ----
merged_data <- merge(all_weekly_excess_deaths, vaccinations, 
                     by.x = c("country", "start_date"), 
                     by.y = c("location", "date"))

# Build Linear Regression Model
lm_model <- lm(excess_deaths_per_100k ~ people_fully_vaccinated_per_hundred, data = merged_data)

# Display Model Summary
summary(lm_model)
