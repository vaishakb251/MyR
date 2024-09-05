# Load necessary libraries
library(dplyr)  # For data manipulation
library(ggplot2)  # For data visualization

# Load the dataset
sales_data <- read.csv("appearances.htm")

# View the first few rows of the dataset
head(sales_data)

# Convert 'date' to Date type
sales_data$date <- as.Date(sales_data$date, format = "%Y-%m-%d")

# Convert relevant columns to numeric
numeric_columns <- c("yellow_cards", "red_cards", "goals", "assists", "minutes_played")
sales_data[numeric_columns] <- lapply(sales_data[numeric_columns], as.numeric)

# Summarize total goals, assists, and minutes played by each player
player_summary <- sales_data %>%
  group_by(player_name) %>%
  summarize(
    TotalGoals = sum(goals, na.rm = TRUE),
    TotalAssists = sum(assists, na.rm = TRUE),
    TotalMinutes = sum(minutes_played, na.rm = TRUE),
    TotalYellowCards = sum(yellow_cards, na.rm = TRUE),
    TotalRedCards = sum(red_cards, na.rm = TRUE)
  ) %>%
  arrange(desc(TotalGoals))  # Sort by total goals

# Print player summary
print(player_summary)

# Plot total goals vs. total assists for each player
ggplot(player_summary, aes(x = TotalGoals, y = TotalAssists, label = player_name)) +
  geom_point(color = "blue") +
  geom_text(vjust = 1.5, hjust = 1.5) +
  labs(title = "Total Goals vs. Total Assists by Player",
       x = "Total Goals",
       y = "Total Assists")

# Plot total minutes played for each player
ggplot(player_summary, aes(x = reorder(player_name, TotalMinutes), y = TotalMinutes)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Total Minutes Played by Each Player",
       x = "Player Name",
       y = "Total Minutes Played")

# Calculate average statistics per player
average_stats <- player_summary %>%
  summarize(
    AvgGoals = mean(TotalGoals, na.rm = TRUE),
    AvgAssists = mean(TotalAssists, na.rm = TRUE),
    AvgMinutes = mean(TotalMinutes, na.rm = TRUE),
    AvgYellowCards = mean(TotalYellowCards, na.rm = TRUE),
    AvgRedCards = mean(TotalRedCards, na.rm = TRUE)
  )

# Print average statistics
print(average_stats)

# Perform correlation analysis
correlation_matrix <- cor(sales_data[numeric_columns], use = "complete.obs")
print(correlation_matrix)

# Plot correlation matrix
library(corrplot)
corrplot(correlation_matrix, method = "circle", type = "upper", 
         title = "Correlation Matrix of Football Stats")
