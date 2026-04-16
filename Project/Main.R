install.packages("dplyr")
install.packages("caTools")
library(dplyr)
library(caTools)

#importing datasets
# Circuits
circuits <- read.table("D:\\Github\\Intro-To-Data-Science2526\\Project\\datasets\\circuits.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(circuits)

# Constructor Results
constructor_results <- read.table("D:\\Github\\Intro-To-Data-Science2526\\Project\\datasets\\constructor_results.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(constructor_results)

# Constructor Standings
constructor_standings <- read.table("D:\\Github\\Intro-To-Data-Science2526\\Project\\datasets\\constructor_standings.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(constructor_standings)

# Constructors
constructors <- read.table("D:\\Github\\Intro-To-Data-Science2526\\Project\\datasets\\constructors.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(constructors)

# Driver Standings
driver_standings <- read.table("D:\\Github\\Intro-To-Data-Science2526\\Project\\datasets\\driver_standings.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(driver_standings)

# Drivers
drivers <- read.table("D:\\Github\\Intro-To-Data-Science2526\\Project\\datasets\\drivers.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(drivers)

# Lap Times
lap_times <- read.table("D:\\Github\\Intro-To-Data-Science2526\\Project\\datasets\\lap_times.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(lap_times)

# Pit Stops
pit_stops <- read.table("D:\\Github\\Intro-To-Data-Science2526\\Project\\datasets\\pit_stops.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(pit_stops)

# Qualifying
qualifying <- read.table("D:\\Github\\Intro-To-Data-Science2526\\Project\\datasets\\qualifying.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(qualifying)

# Races
races <- read.table("D:\\Github\\Intro-To-Data-Science2526\\Project\\datasets\\races.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(races)

# Results
results <- read.table("D:\\Github\\Intro-To-Data-Science2526\\Project\\datasets\\results.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(results)

# Seasons
seasons <- read.table("D:\\Github\\Intro-To-Data-Science2526\\Project\\datasets\\seasons.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(seasons)

# Sprint Results
sprint_results <- read.table("D:\\Github\\Intro-To-Data-Science2526\\Project\\datasets\\sprint_results.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(sprint_results)

# Status
status <- read.table("D:\\Github\\Intro-To-Data-Science2526\\Project\\datasets\\status.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(status)

#changing the null type for missing values to identify them properly
#function to change them
convert_N_to_NA <- function(df) {
  df[df == "\\N"] <- NA
  df[df == ""] <- NA
  df[df == "NULL"] <- NA
  return(df)
}
# Applying the function to all data frames
circuits <- convert_N_to_NA(circuits)
constructor_results <- convert_N_to_NA(constructor_results)
constructor_standings <- convert_N_to_NA(constructor_standings)
constructors <- convert_N_to_NA(constructors)
driver_standings <- convert_N_to_NA(driver_standings)
drivers <- convert_N_to_NA(drivers)
lap_times <- convert_N_to_NA(lap_times)
pit_stops <- convert_N_to_NA(pit_stops)
qualifying <- convert_N_to_NA(qualifying)
races <- convert_N_to_NA(races)
results <- convert_N_to_NA(results)
seasons <- convert_N_to_NA(seasons)
sprint_results <- convert_N_to_NA(sprint_results)
status <- convert_N_to_NA(status)

#Checking for missing values
colSums(is.na(circuits))
colSums(is.na(constructor_results))
colSums(is.na(constructor_standings))
colSums(is.na(constructors))
colSums(is.na(driver_standings))
colSums(is.na(drivers))
colSums(is.na(lap_times))
colSums(is.na(pit_stops))
colSums(is.na(qualifying))
colSums(is.na(races))
colSums(is.na(results))
colSums(is.na(seasons))
colSums(is.na(sprint_results))
colSums(is.na(status))

#function to plot missing values 
plot_missing <- function(df, title) {
  missing_data <- colSums(is.na(df))
  
  colors <- ifelse(missing_data > 0, "salmon", "skyblue")
  
  barplot(
    missing_data,
    main = paste("Missing Values per Column -", title),
    xlab = "Columns",
    ylab = "Count",
    col = colors,
    border = "gray40",
    las = 2,    
    cex.names = 0.8,  
    cex.axis = 0.8
  )
  
  abline(h = pretty(missing_data), col = "lightgray", lty = "dotted")
}

# Plotting missing values for each dataset
plot_missing(circuits, "Circuits")
plot_missing(constructor_results, "Constructor Results")
plot_missing(constructor_standings, "Constructor Standings")
plot_missing(constructors, "Constructors")
plot_missing(driver_standings, "Driver Standings")
plot_missing(drivers, "Drivers")
plot_missing(lap_times, "Lap Times")
plot_missing(pit_stops, "Pit Stops")
plot_missing(qualifying, "Qualifying")
plot_missing(races, "Races")
plot_missing(results, "Results")
plot_missing(seasons, "Seasons")
plot_missing(sprint_results, "Sprint Results")
plot_missing(status, "Status")

#Replacing missing values with the mean for numeric columns and mode for categorical columns
# Function to replace missing values
replace_missing <- function(df, method = "median") {
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      if (method == "mean") {
        df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
      } 
      else if (method == "median") {
        df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
      }
    } 
    else {
      mode_value <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
      df[[col]][is.na(df[[col]])] <- mode_value
    }
  }
  return(df)
}
# Applying the function to all datasets
circuits <- replace_missing(circuits)
constructor_results <- replace_missing(constructor_results)
constructor_standings <- replace_missing(constructor_standings)
constructors <- replace_missing(constructors)
driver_standings <- replace_missing(driver_standings)
drivers <- replace_missing(drivers)
lap_times <- replace_missing(lap_times)
pit_stops <- replace_missing(pit_stops)
qualifying <- replace_missing(qualifying)
races <- replace_missing(races)
results <- replace_missing(results)
seasons <- replace_missing(seasons)
sprint_results <- replace_missing(sprint_results)
status <- replace_missing(status)

#checking for missing values again after replacement
colSums(is.na(circuits))
colSums(is.na(constructor_results))
colSums(is.na(constructor_standings))
colSums(is.na(constructors))
colSums(is.na(driver_standings))
colSums(is.na(drivers))
colSums(is.na(lap_times))
colSums(is.na(pit_stops))
colSums(is.na(qualifying))
colSums(is.na(races))
colSums(is.na(results))
colSums(is.na(seasons))
colSums(is.na(sprint_results))
colSums(is.na(status))

#Changing the Driver id's to Surname intitials
drivers$code <- toupper(substr(drivers$surname, 1, 3))

#Handling invalid data values
#Lap Times (q1, q2, q3, fastestLapTime)
convert_lap_time <- function(x) {
  if (is.na(x) || x == "\\N" || x == "" || x == "300") return(NA_real_)
  
  parts <- unlist(strsplit(x, ":"))
  minutes <- as.numeric(parts[1])
  seconds <- as.numeric(parts[2])
  
  return(minutes * 60 + seconds)
}

# Apply to qualifying times
qualifying$q1 <- sapply(qualifying$q1, convert_lap_time)
qualifying$q2 <- sapply(qualifying$q2, convert_lap_time)
qualifying$q3 <- sapply(qualifying$q3, convert_lap_time)
results$fastestLapTime <- sapply(results$fastestLapTime, convert_lap_time)
lap_times$time <- sapply(lap_times$time, convert_lap_time)

#Race/Sprint Times (milliseconds)
#Capping values to a reasonable range
results$milliseconds <- pmin(pmax(results$milliseconds, 1000), 10000000)
sprint_results$milliseconds <- pmin(pmax(sprint_results$milliseconds, 1000), 1000000)

#Performance Metrics (fastestLapSpeed)
#Cap speeds to realistic F1 ranges (150–380 km/h)
results$fastestLapSpeed <- pmin(pmax(results$fastestLapSpeed, 150), 380)

#Laps Completed
#Cap laps to valid range (0–80):
results$laps <- pmin(pmax(results$laps, 0), 80)

#checking data types for all datasets
str(circuits)
str(constructor_results)
str(constructor_standings)
str(constructors)
str(driver_standings)
str(drivers)
str(lap_times)
str(pit_stops)
str(qualifying)
str(races)
str(results)
str(seasons)
str(sprint_results)
str(status)

#fixing data types for drivers numbers
drivers$number <- as.integer(drivers$number)
#fixing data type for drivers' date of birth and formatting it to a standard date format
drivers$dob <- as.Date(drivers$dob, format = "%Y-%m-%d")
#fixing data types for qualifying times (q1, q2, q3)
qualifying$q1 <- as.numeric(qualifying$q1)
qualifying$q2 <- as.numeric(qualifying$q2)
qualifying$q3 <- as.numeric(qualifying$q3)
#fixing data type for results' fastest lap time and speed
results$fastestLapTime <- as.numeric(results$fastestLapTime)
results$fastestLapSpeed <- as.numeric(results$fastestLapSpeed)
#fixing data type for results' milliseconds and laps
results$milliseconds <- as.integer(results$milliseconds)
results$laps <- as.integer(results$laps)
results$position <- as.integer(results$position)
results$rank <- as.integer(results$rank)
#fixing data type for sprint results'
sprint_results$milliseconds <- as.integer(sprint_results$milliseconds)
sprint_results$position <- as.integer(sprint_results$position)
sprint_results$positionOrder <- as.integer(sprint_results$positionOrder)
sprint_results$laps <- as.integer(sprint_results$laps)
#fixing data type for pit stops' duration and milliseconds
pit_stops$duration <- as.numeric(pit_stops$duration)

#results - fixing data types
results$number <- as.integer(results$number)
results$fastestLap <- as.integer(results$fastestLap)

# Races - fixing data type for date and time
races$date <- as.Date(races$date, format = "%Y-%m-%d")
races$time <- as.POSIXct(races$time, format = "%H:%M:%S")
races$fp1_date <- as.Date(races$fp1_date, format = "%Y-%m-%d")
races$fp1_time <- as.POSIXct(races$fp1_time, format = "%H:%M:%S")
races$fp2_date <- as.Date(races$fp2_date, format = "%Y-%m-%d")
races$fp2_time <- as.POSIXct(races$fp2_time, format = "%H:%M:%S")
races$fp3_date <- as.Date(races$fp3_date, format = "%Y-%m-%d")
races$fp3_time <- as.POSIXct(races$fp2_time, format = "%H:%M:%S")

#Finding and removing duplicate rows
# Function to remove duplicates
remove_duplicates <- function(df) {
  duplicates <- df[duplicated(df), ]
  if (nrow(duplicates) > 0) {
    cat("Duplicate rows found:\n")
    print(duplicates)
  } else {
    cat("No duplicate rows found.\n")
  }
  df <- df[!duplicated(df), ]
  print(dim(df))
  return(df)
}


# Applying the function to all datasets
circuits <- remove_duplicates(circuits)
constructor_results <- remove_duplicates(constructor_results)
constructor_standings <- remove_duplicates(constructor_standings)
constructors <- remove_duplicates(constructors)
driver_standings <- remove_duplicates(driver_standings)
drivers <- remove_duplicates(drivers)
lap_times <- remove_duplicates(lap_times)
pit_stops <- remove_duplicates(pit_stops)
qualifying <- remove_duplicates(qualifying)
races <- remove_duplicates(races)
results <- remove_duplicates(results)

#Handling outliers is not needed as we have already capped the values to reasonable ranges for the relevant columns
#some skew values are needed for analysis and predicting certain outcomes

#Summarizing the cleaned datasets
# Function to summarize datasets
summarize_dataset <- function(df, name) {
  cat("Summary of", name, "dataset:\n")
  cat("Number of rows:", nrow(df), "\n")
  cat("Number of columns:", ncol(df), "\n")
  str(df)
  summary(df)
}

# Applying the function to all datasets
summarize_dataset(circuits, "Circuits")
summarize_dataset(constructor_results, "Constructor Results")
summarize_dataset(constructor_standings, "Constructor Standings")
summarize_dataset(constructors, "Constructors")
summarize_dataset(driver_standings, "Driver Standings")
summarize_dataset(drivers, "Drivers")
summarize_dataset(lap_times, "Lap Times")
summarize_dataset(pit_stops, "Pit Stops")
summarize_dataset(qualifying, "Qualifying")
summarize_dataset(races, "Races")
summarize_dataset(results, "Results")
summarize_dataset(seasons, "Seasons")
summarize_dataset(sprint_results, "Sprint Results")
summarize_dataset(status, "Status")

#Filtering the datasets to include only the most recent 10 seasons (2014-2023)
valid_races <- subset(races, year >= 2014 & year <= 2024)

valid_results    <- subset(results,    raceId %in% valid_races$raceId)
valid_qualifying <- subset(qualifying, raceId %in% valid_races$raceId)
valid_sprint_results <- subset(sprint_results, raceId %in% valid_races$raceId)
valid_pit_stops <- subset(pit_stops, raceId %in% valid_races$raceId)
valid_lap_times <- subset(lap_times, raceId %in% valid_races$raceId)
valid_driver_standings <- subset(driver_standings, raceId %in% valid_races$raceId)
valid_constructor_standings <- subset(constructor_standings, raceId %in% valid_races$raceId)
valid_constructor_results <- subset(constructor_results, raceId %in% valid_races$raceId)
valid_constructors <- subset(constructors, constructorId %in% valid_constructor_results$constructorId)
valid_drivers <- subset(drivers, driverId %in% valid_results$driverId)
valid_circuits <- subset(circuits, circuitId %in% valid_races$circuitId)
valid_status <- status
valid_seasons <- subset(seasons, year >= 2014 & year <= 2024)

#filtering the results dataset to include only if 80% laps were completed as lower could skew some predictions and analysis related to sc probability
valid_results <- subset(valid_results, laps >= 0.8 * max(laps))

#filter the pit stops dataset to include only stops that were less than 30 seconds as higher could indicate a major issue and skew analysis related to pit stop performance
valid_pit_stops <- subset(valid_pit_stops, duration < 30 & duration > 12)

#filter the lap times dataset to include only lap times that were less than 2 minutes as higher could indicate a major issue and skew analysis related to lap time performance
valid_lap_times <- subset(valid_lap_times, time < 105 & time > 65)

#fitlering the results dataset to include only valid statuses to ensure the driver actually finished the race 
valid_results <- subset(valid_results, statusId %in% valid_status$statusId %in% c(1, 11, 12))

#normalizing the datasets' numeric columns using min-max normalization to ensure all features are on the same scale for analysis and modeling
normalize_minmax <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Applying normalization to relevant numeric columns in the results dataset
valid_lap_times$time <- normalize_minmax(valid_lap_times$time)
valid_lap_times$milliseconds <- normalize_minmax(valid_lap_times$milliseconds)
valid_pit_stops$duration <- normalize_minmax(valid_pit_stops$duration)
valid_pit_stops$milliseconds <- normalize_minmax(valid_pit_stops$milliseconds)
valid_qualifying$q1 <- normalize_minmax(valid_qualifying$q1)
valid_qualifying$q2 <- normalize_minmax(valid_qualifying$q2)
valid_qualifying$q3 <- normalize_minmax(valid_qualifying$q3)
valid_results$fastestLapTime <- normalize_minmax(valid_results$fastestLapTime)
valid_results$fastestLapSpeed <- normalize_minmax(valid_results$fastestLapSpeed)
valid_results$milliseconds <- normalize_minmax(valid_results$milliseconds)
valid_results$laps <- normalize_minmax(valid_results$laps)
valid_sprint_results$milliseconds <- normalize_minmax(valid_sprint_results$milliseconds)
valid_sprint_results$fastestlap <- normalize_minmax(valid_sprint_results$fastestlap)

#descriptive statistics by target class
valid_results %>%
  group_by(constructorId) %>%
  summarise(
    count = n(),
    avg_fastestLapTime = mean(fastestLapTime, na.rm = TRUE),
    avg_fastestLapSpeed = mean(fastestLapSpeed, na.rm = TRUE),
    avg_milliseconds = mean(milliseconds, na.rm = TRUE),
    avg_laps = mean(laps, na.rm = TRUE)
  ) %>%
  arrange(constructorId)

#comparing averages between 3 constructors based on average fastest lap time
valid_results %>%
  filter(constructorId %in% c(1, 2, 3)) %>%
  group_by(constructorId) %>%
  summarise(avg_fastestLapTime = mean(fastestLapTime, na.rm = TRUE)) %>%
  arrange(avg_fastestLapTime)

#comparing variation of position and laptimes across constructors
valid_results %>%
  filter(constructorId %in% c(1, 9, 131)) %>%
  group_by(constructorId) %>%
  summarise(position_variation = var(position, na.rm = TRUE), fastestLapTime_variation = mean(fastestLapTime, na.rm = TRUE)) %>%
  arrange(position_variation)

#splitting the results dataset into training and testing sets for modeling
# Create a logical vector to split data: TRUE for training, FALSE for testing
set.seed(123)  # for reproducibility
split <- sample.split(valid_results$position, SplitRatio = 0.8)
# Subset the data into training and testing sets
train_results <- subset(valid_results, split == TRUE)
test_results <- subset(valid_results, split == FALSE)
#checking the number of rows in training and testing sets
cat("Number of rows in training set:", nrow(train_results), "\n")
cat("Number of rows in testing set:", nrow(test_results), "\n")