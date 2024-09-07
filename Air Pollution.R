# Function to calculate the mean of a pollutant across specified monitors
pollutantmean <- function(directory, pollutant, id = 1:332) {
  pollutant_values <- c()
  
  for (monitor in id) {
    file_name <- sprintf("%03d.csv", monitor)
    file_path <- file.path(directory, file_name)
    data <- read.csv(file_path)
    pollutant_values <- c(pollutant_values, data[[pollutant]])
  }
  
  mean_value <- mean(pollutant_values, na.rm = TRUE)
  return(mean_value)
}

# Function to count complete cases in specified monitors
complete <- function(directory, id = 1:332) {
  results <- data.frame(id = integer(), nobs = integer())
  
  for (monitor in id) {
    file_name <- sprintf("%03d.csv", monitor)
    file_path <- file.path(directory, file_name)
    data <- read.csv(file_path)
    complete_cases <- sum(complete.cases(data))
    results <- rbind(results, data.frame(id = monitor, nobs = complete_cases))
  }
  
  return(results)
}

# Function to compute correlation between sulfate and nitrate based on the threshold
corr <- function(directory, threshold = 0) {
  correlations <- c()  # Initialize a vector to store correlations
  
  for (monitor in 1:332) {
    file_name <- sprintf("%03d.csv", monitor)
    file_path <- file.path(directory, file_name)
    data <- read.csv(file_path)
    
    # Filter complete cases
    complete_data <- data[complete.cases(data), ]
    
    # Check if the number of complete cases is greater than the threshold
    if (nrow(complete_data) > threshold) {
      # Compute correlation between sulfate and nitrate
      correlation <- cor(complete_data$sulfate, complete_data$nitrate)
      correlations <- c(correlations, correlation)
    }
  }
  
  return(correlations)
}

# Example usage:
# source("combined_script.R")
# pollutantmean("specdata", "sulfate", 1:10)
# complete("specdata", c(2, 4, 8, 10, 12))
# corr("specdata", 150)
