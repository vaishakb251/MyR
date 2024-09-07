# Load the data and plot 30-day mortality rates for heart attacks
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# Convert the heart attack mortality rates to numeric (column 11)
suppressWarnings(outcome[, 11] <- as.numeric(outcome[, 11]))

# Plot a histogram of the 30-day mortality rates for heart attacks
hist(outcome[, 11], main="30-day Mortality Rates for Heart Attack", 
     xlab="Mortality Rate", ylab="Frequency")

# Function to find the best hospital in a state
best <- function(state, outcome_name) {
  # Load the data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check if state is valid
  if (!state %in% data$State) {
    stop("invalid state")
  }
  
  # Define column number based on the outcome
  outcome_col <- ifelse(outcome_name == "heart attack", 11,
                        ifelse(outcome_name == "heart failure", 17,
                               ifelse(outcome_name == "pneumonia", 23, NA)))
  
  if (is.na(outcome_col)) {
    stop("invalid outcome")
  }
  
  # Filter data by state
  state_data <- data[data$State == state, ]
  
  # Convert outcome to numeric and suppress warnings about NAs
  suppressWarnings(state_data[, outcome_col] <- as.numeric(state_data[, outcome_col]))
  state_data <- state_data[!is.na(state_data[, outcome_col]), ]
  
  # Get the hospital with the lowest outcome
  best_hospital <- state_data$Hospital.Name[which.min(state_data[, outcome_col])]
  return(best_hospital)
}

# Function to rank hospitals by outcome in a state
rankhospital <- function(state, outcome_name, num = "best") {
  # Load the data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check if state is valid
  if (!state %in% data$State) {
    stop("invalid state")
  }
  
  # Define column number based on the outcome
  outcome_col <- ifelse(outcome_name == "heart attack", 11,
                        ifelse(outcome_name == "heart failure", 17,
                               ifelse(outcome_name == "pneumonia", 23, NA)))
  
  if (is.na(outcome_col)) {
    stop("invalid outcome")
  }
  
  # Filter data by state
  state_data <- data[data$State == state, ]
  
  # Convert outcome to numeric and remove NAs
  suppressWarnings(state_data[, outcome_col] <- as.numeric(state_data[, outcome_col]))
  state_data <- state_data[!is.na(state_data[, outcome_col]), ]
  
  # Rank hospitals
  state_data <- state_data[order(state_data[, outcome_col], state_data$Hospital.Name), ]
  
  # Handle num argument
  if (num == "best") {
    return(state_data$Hospital.Name[1])
  } else if (num == "worst") {
    return(state_data$Hospital.Name[nrow(state_data)])
  } else if (num > nrow(state_data)) {
    return(NA)
  } else {
    return(state_data$Hospital.Name[num])
  }
}

# Function to rank hospitals across all states
rankall <- function(outcome_name, num = "best") {
  # Load the data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Define column number based on the outcome
  outcome_col <- ifelse(outcome_name == "heart attack", 11,
                        ifelse(outcome_name == "heart failure", 17,
                               ifelse(outcome_name == "pneumonia", 23, NA)))
  
  if (is.na(outcome_col)) {
    stop("invalid outcome")
  }
  
  # Convert outcome to numeric and remove NAs
  suppressWarnings(data[, outcome_col] <- as.numeric(data[, outcome_col]))
  
  # Split data by state
  split_data <- split(data, data$State)
  
  # Function to rank hospitals within each state
  rank_hospital_in_state <- function(state_data) {
    state_data <- state_data[!is.na(state_data[, outcome_col]), ]
    state_data <- state_data[order(state_data[, outcome_col], state_data$Hospital.Name), ]
    
    if (num == "best") {
      return(state_data$Hospital.Name[1])
    } else if (num == "worst") {
      return(state_data$Hospital.Name[nrow(state_data)])
    } else if (num > nrow(state_data)) {
      return(NA)
    } else {
      return(state_data$Hospital.Name[num])
    }
  }
  
  # Apply function to all states and return results
  result <- sapply(split_data, rank_hospital_in_state)
  return(data.frame(hospital = result, state = names(result)))
}
