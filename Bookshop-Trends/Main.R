#importing libraries
library(dplyr)
library(rvest)
library(stringr)
library(future.apply)
library(ggplot2)
library(tidyr)
library(caret)
library(randomForest)

plan(multisession, workers = 5) # Setting up parallel processing with 5 workers

#Storing the urls in a variable
link1t5 <- paste0("https://www.bookowlsbd.com/collections/frontpage?page=", 1:5)
link6t10 <- paste0("https://www.bookowlsbd.com/collections/frontpage?page=", 6:10)
link11t15 <- paste0("https://www.bookowlsbd.com/collections/frontpage?page=", 11:15)
link16t20 <- paste0("https://www.bookowlsbd.com/collections/frontpage?page=", 16:20)
link21t25 <- paste0("https://www.bookowlsbd.com/collections/frontpage?page=", 21:25)
link26t30 <- paste0("https://www.bookowlsbd.com/collections/frontpage?page=", 26:30)

#Defining a function to read the webpage and extract the relevant nodes
read_webpage <- function(link) {
  page <- read_html(link)

  #Extracting the book titles
  book_titles <- page %>% 
    html_nodes("h3.card__heading a") %>% 
    html_text(trim = TRUE)
  #printing the book titles
  #print(book_titles)

  # Extract prices directly from div class price__container
  prices <- page %>% 
    html_nodes("div.price__container") %>% 
    html_text(trim = TRUE)
  # Printing the extracted prices
  #print(prices)

  # Extract author names only if "by" exists
  authors <- ifelse(str_detect(book_titles, " by "),
                    str_extract(book_titles, "(?<=by ).*$"),
                    NA)   # assign NA if no "by"
  # Printing the extracted author names
  #print(authors)

  #Extracting Urls of the books
  urls <- page %>% 
    html_nodes("h3.card__heading a") %>% 
    html_attr("href")

  # Make full URLs (prepend domain)
  urls <- paste0("https://www.bookowlsbd.com", urls)

  # Printing the extracted URLs
  #print(urls)

  # Combining the extracted data into a data frame
  data.frame(
    title = book_titles,
    price = prices,
    authors = authors,
    url = urls,
    stringsAsFactors = FALSE)
}

#Binding the data frames from each page into one data frame
books_data1 <- bind_rows(future_lapply(link1t5, read_webpage))
books_data2 <- bind_rows(future_lapply(link6t10, read_webpage))
books_data3 <- bind_rows(future_lapply(link11t15, read_webpage))
books_data4 <- bind_rows(future_lapply(link16t20, read_webpage))
books_data5 <- bind_rows(future_lapply(link21t25, read_webpage))
books_data6 <- bind_rows(future_lapply(link26t30, read_webpage))

+# Removing non-books (rows with NA author)
books_clean1 <- books_data1 %>% 
  filter(!is.na(authors))
books_clean2 <- books_data2 %>% 
  filter(!is.na(authors))
books_clean3 <- books_data3 %>%
  filter(!is.na(authors))
books_clean4 <- books_data4 %>%
  filter(!is.na(authors))
books_clean5 <- books_data5 %>%
  filter(!is.na(authors))
books_clean6 <- books_data6 %>%
  filter(!is.na(authors))

#Function to get book details from the product page
get_book_details <- function(product_url) {
  page <- read_html(product_url)
  
  # Extract all <p> tags inside product description
  desc <- page %>% 
    html_nodes("div.product__description p") %>% 
    html_text(trim = TRUE)
  
  # Pages (case-insensitive)
  pages <- str_extract(desc, "(?i)\\d+\\s*pages")
  pages <- if(length(pages[!is.na(pages)]) > 0) pages[!is.na(pages)][1] else NA
  
  #Extracting details of the book
   details <- page %>% 
    html_nodes("div.product__description p") %>% 
    html_text(trim = TRUE) %>% 
    paste(collapse = " ")
  
  return(data.frame(
    url = product_url,
    pages = pages,
    details = details,
    stringsAsFactors = FALSE
  ))
}

#Looping through each book URL to get no of pages and details
details_list1 <- future_lapply(books_clean1$url, get_book_details)
details_list2 <- future_lapply(books_clean2$url, get_book_details)
details_list3 <- future_lapply(books_clean3$url, get_book_details)
details_list4 <- future_lapply(books_clean4$url, get_book_details)
details_list5 <- future_lapply(books_clean5$url, get_book_details)
details_list6 <- future_lapply(books_clean6$url, get_book_details)

#Binding all the batches into separate data frames
details_df1 <- bind_rows(details_list1)
details_df2 <- bind_rows(details_list2)
details_df3 <- bind_rows(details_list3)
details_df4 <- bind_rows(details_list4)
details_df5 <- bind_rows(details_list5)
details_df6 <- bind_rows(details_list6)

# Merging page column and details column back to cleaned data frames
# Deduplicate details data frames before joining
details_df1 <- details_df1 %>% distinct(url, .keep_all = TRUE)
details_df2 <- details_df2 %>% distinct(url, .keep_all = TRUE)
details_df3 <- details_df3 %>% distinct(url, .keep_all = TRUE)
details_df4 <- details_df4 %>% distinct(url, .keep_all = TRUE)
details_df5 <- details_df5 %>% distinct(url, .keep_all = TRUE)
details_df6 <- details_df6 %>% distinct(url, .keep_all = TRUE)

#join safely
books_clean1 <- books_clean1 %>% left_join(details_df1, by = "url")
books_clean2 <- books_clean2 %>% left_join(details_df2, by = "url")
books_clean3 <- books_clean3 %>% left_join(details_df3, by = "url")
books_clean4 <- books_clean4 %>% left_join(details_df4, by = "url")
books_clean5 <- books_clean5 %>% left_join(details_df5, by = "url")
books_clean6 <- books_clean6 %>% left_join(details_df6, by = "url")


#Combining all the cleaned data frames into one final data frame
books_clean <- bind_rows(books_clean1, books_clean2, books_clean3, books_clean4, books_clean5, books_clean6)
books_clean <- books_clean %>% distinct() #removing duplicate rows

#removing unnecessary data frames and variables to free up memory
rm(books_data1, books_data2, books_data3, books_data4, books_data5, books_data6)
rm(books_clean1, books_clean2, books_clean3, books_clean4, books_clean5, books_clean6)
rm(details_df1, details_df2, details_df3, details_df4, details_df5, details_df6)
rm(link1t5, link6t10, link11t15, link16t20, link21t25, link26t30)

#Keeping only the numeric value in the price column
books_clean$price <- as.numeric(str_extract(books_clean$price, "\\d+\\.?\\d*"))

library(stringr)

# Function to get the start position of the 2nd "pages"
get_pages_start <- function(details){
  details_lower <- tolower(details)
  
  # Find all positions of "pages"
  pages_positions <- str_locate_all(details_lower, "pages")[[1]]
  
  # If fewer than 2 occurrences, return NA
  if (nrow(pages_positions) < 2) {
    return(NA)
  }
  
  return(pages_positions[2, 1])  # start position of 2nd "pages"
}

# Function to extract the page number before the 2nd "pages"
get_pages <- function(details) {
  details_lower <- tolower(details)
  
  # Look for "pages amount" followed by a number
  page_num <- str_extract(details_lower, "(?<=pages amount )\\d+")
  
  if (is.na(page_num)) {
    return(NA)
  }
  
  return(as.numeric(page_num))
}

# Applying the function to get pages data from details column and filling in the pages column where it is NA
books_clean$pages <- ifelse(is.na(books_clean$pages), sapply(books_clean$details, get_pages), books_clean$pages)
  
#Keeping only the numeric value in the pages column
books_clean$pages <- as.numeric(str_extract(books_clean$pages, "\\d+"))

# Extracting paperback and hardcover prices and storing them in separate columns
books_clean <- books_clean %>%
  group_by(title, authors, url) %>%
  mutate(
    price_paperback = min(price, na.rm = TRUE),
    price_hardcover = max(price, na.rm = TRUE),
    price = paste0(min(price, na.rm = TRUE), "–", max(price, na.rm = TRUE)) #making the price column show the range of prices
  ) %>%
  ungroup()

#removing duplicate rows after grouping and summarizing the price columns
books_clean <- books_clean %>% distinct()

#getting average price for each book and storing it in a new column
books_clean <- books_clean %>%
  rowwise() %>%
  mutate(price_avg = mean(c(price_paperback, price_hardcover), na.rm= TRUE))
  

#Adding the ranking column based on their sorting order on the website(by best selling)
books_clean$rank <- seq_along(books_clean$title)   # bestseller rank based on order


#Extracting the summary of the book from the details column
get_summary_start <- function(details){
  #Finding the position of the word summary in the details
  summary_start <- str_locate(details, "Summary -")[1, 2] + 1
  if(is.na(summary_start)) {
    summary_start <- str_locate(details, "summary -")[1, 2] + 1
  }
  if(is.na(summary_start)) {
    summary_start <- str_locate(details, "Summary :")[1, 2] + 1
  }
  if(is.na(summary_start)) {
    summary_start <- str_locate(details, "Summary")[1, 2] + 1
  }
  if(is.na(summary_start)) {
    summary_start <- str_locate(details, "summary :")[1, 2] + 1
  }
  if(is.na(summary_start)) {
    return(NA) # Return NA if no summary marker is found
  }
  return(summary_start)
}

get_summary <- function(details) {
  
  summary_start <- get_summary_start(details)
  
  #Extracting the summary text
  summary <- str_sub(details, summary_start)
  return(summary)
}
# Applying the function to extract summaries and adding them as a new column
books_clean$summary <- sapply(books_clean$details, get_summary)

get_genre <- function(details) {
  # Define keywords properly as character strings
  keywords <- c(
    "Fiction", "Science Fiction", "Fantasy", "Mystery", "Thriller", "Romance",
    "Biography", "History", "Self Help","Self-Help", "Young Adult", "Horror",
    "Classic", "Poetry", "Graphic Novel", "Novel", "Short Story", "Essay",
    "Memoir", "Travel", "Philosophy", "Religion", "Humor", "Business", "Health",
    "Historical Fiction", "Science Fiction", "Biography/Memoir", "Business & Economics",
    "War and Politics", "Religion and Spirituality", "Spiritual","Psychology", "Adventure",
    "Action", "Comedy", "Dystopian", "Children's Literature", "Classic Literature",
    "Journal", "Islamic Literature","Crime", "Letters"
  )
  
  # Convert details to lowercase
  details_lower <- tolower(details)
  
  # Convert keywords to lowercase
  keywords_lower <- tolower(keywords)
  
  # Check each keyword
  matches <- keywords[sapply(keywords_lower, function(k) str_detect(details_lower, fixed(k)))]
  
  # Collapse into a string if found
  if (length(matches) > 0) {
    return(paste(unique(matches), collapse = ", "))
  } else {
    return(NA)
  }
}

# Applying the function to extract genres and adding them as a new column
books_clean$genre <- sapply(books_clean$details, get_genre)

#Function to reformat the title by removing the author name from the title if it exists
reformat_title <- function(title, author) {
  # Check if the title contains " by " followed by the author name
  if(str_detect(title, paste0(" by ", author))) {
    # Remove the " by " and the author name from the title
    title <- str_replace(title, paste0(" by ", author), "")
  }
  return(title)
}
# Applying the function to reformat titles
books_clean$title <- mapply(reformat_title, books_clean$title, books_clean$authors)

# Reordering columns to have rank first
books_clean <- books_clean %>%
  select(rank, title, authors, genre, price, price_avg, price_paperback, price_hardcover, pages, summary, details, url)

#Summarize the data frame to get an overview of the data
summary(books_clean)

#Plotting missing values in the data frame
plot_missing <- function(df) {
  missing_counts <- sapply(df, function(x) sum(is.na(x)))
  missing_df <- data.frame(column = names(missing_counts), missing_count = missing_counts)
  
  ggplot(missing_df, aes(x = reorder(column, -missing_count), y = missing_count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Missing Values Count by Column", x = "Column", y = "Count of Missing Values") +
    theme_minimal()
}

# Calling the function to plot missing values
colSums(is.na(books_clean)) # Print the count of missing values for each column
plot_missing(books_clean)

#Handling missing values by filling in the missing values
# Filling missing genres with "Unknown"
books_clean$genre[is.na(books_clean$genre)] <- "Unknown"

books_clean <- books_clean %>%
  filter(!is.na(pages)) # Removing rows where pages is NA, as it's a crucial piece of information for analysis

#redoing the ranking after removing rows with NA pages
books_clean$rank <- seq_along(books_clean$title) # Reassigning rank based on the new order after filtering

#Showing missing values after handling them
colSums(is.na(books_clean)) # Print the count of missing values for each column
plot_missing(books_clean)

# Printing the final data frame
print(books_clean)

#handleing outliers by capping the values at the 95th percentile
cap_values <- function(x, lower_quantile = 0.05, upper_quantile = 0.95) {
  qnt <- quantile(x, probs = c(lower_quantile, upper_quantile), na.rm = TRUE)
  x[x < qnt[1]] <- qnt[1]
  x[x > qnt[2]] <- qnt[2]
  return(x)
}

books_clean$price_avg <- cap_values(books_clean$price_avg)
books_clean$pages     <- cap_values(books_clean$pages)

#applying log transformation to the price and pages columns to reduce skewness
books_clean$price_log <- log1p(books_clean$price_avg)
books_clean$pages_log <- log1p(books_clean$pages)

#normalization of price and pages columns for better visualization
# Define normalization function
normalize <- function(vec) {
  return((vec - min(vec, na.rm = TRUE)) / (max(vec, na.rm = TRUE) - min(vec, na.rm = TRUE)))
}

# Applying to pages and prices columns
books_clean$price_norm <- normalize(books_clean$price_avg)
books_clean$pages_norm <- normalize(books_clean$pages)

#encoding the genre variable using one-hot encoding
books_genre_split <- books_clean %>%
  separate_rows(genre, sep = ",\\s*")

#creating dummy variables for the genre variable
dummies <- dummyVars(~ genre, data = books_genre_split)
genre_encoded <- predict(dummies, newdata = books_genre_split)

#combining the encoded genre variables with the original data frame
books_encoded <- cbind(books_genre_split %>% select(-genre), genre_encoded)
books_encoded$pages <- as.numeric(books_encoded$pages)

head(books_clean)


#creating categorical variable for price and pages
books_clean$pages_bin <- cut(
  books_clean$pages,
  breaks = c(-Inf, 150, 300, 500, Inf),
  labels = c("Short", "Medium", "Long", "Very Long")
)

books_clean$price_bin <- cut(
  books_clean$price_avg,
  breaks = c(-Inf, 300, 600, 1000, Inf),
  labels = c("Low", "Affordable", "Premium", "Luxury")
)

#price distribution
ggplot(books_clean, aes(x = price_avg)) +
  geom_histogram(binwidth = 50, fill = "purple", color = "black") +
  labs(title = "Distribution of Average Book Prices", x = "Average Price (BDT)", y = "Count") +
  theme_minimal()

#Pages vs Price
ggplot(books_clean, aes(x = pages_bin, y = price_norm, fill = pages_bin)) +
  geom_boxplot() +
  labs(title = "Normalized Price by Book Length Category",
       x = "Book Length Category", y = "Normalized Price")

#Genre vs Average Price
books_clean %>%
  # Split multiple genres into separate rows
  separate_rows(genre, sep = ",\\s*") %>%
  group_by(genre) %>%
  summarise(avg_price = mean(price_norm, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(genre, avg_price), y = avg_price, fill = genre)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Average Price by Genre",
       x = "Genre", y = "Average Price")

#Price vs Rank
ggplot(books_clean, aes(x = price_bin, y = rank, fill = price_bin)) +
  geom_boxplot() +
  labs(title = "Bestseller Rank by Price Category",
       x = "Price Category", y = "Rank")

#Genre Popularity by rank
books_clean%>%
  separate_rows(genre, sep = ",\\s*") %>%   # split multi-genre books
  group_by(genre) %>%
  summarise(avg_rank = mean(rank, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(genre, avg_rank), y = avg_rank, fill = genre)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Average Bestseller Rank by Genre",
       x = "Genre",
       y = "Average Rank")

#Selected features for predicting price are pages, genre, and rank. We will use these features to build a linear regression model to predict the average price of the books.

#encoding the genre variable using one-hot encoding
books_genre_split <- books_clean %>%
  separate_rows(genre, sep = ",\\s*")

#creating dummy variables for the genre variable
dummies <- dummyVars(~ genre, data = books_genre_split)
genre_encoded <- predict(dummies, newdata = books_genre_split)

#combining the encoded genre variables with the original data frame
books_encoded <- cbind(books_genre_split %>% select(-genre), genre_encoded)
books_encoded$pages <- as.numeric(books_encoded$pages)

#spiltting the data into training and testing
set.seed(123) # Setting seed for reproducibility
train_index <- createDataPartition(books_encoded$price_avg, p = 0.7, list = FALSE) # 70% for training
train_data <- books_encoded[train_index, ]
test_data <- books_encoded[-train_index, ]

# Fit linear regression model
lm_model <- lm(rank ~ price_norm + pages_norm + genreAction + genreAdventure + genreFantasy + genrePhilosophy + genreRomance, 
               data = train_data)

# Predictions
lm_preds <- predict(lm_model, newdata = test_data)

# Compare actual vs predicted
results_rank <- data.frame(
  Actual = test_data$rank,
  Predicted = lm_preds
)

# Performance metrics
lm_rmse <- sqrt(mean((test_data$rank - lm_preds)^2))
lm_mae  <- mean(abs(test_data$rank - lm_preds))
lm_r2   <- 1 - sum((test_data$rank - lm_preds)^2) / sum((test_data$rank - mean(test_data$rank))^2)

lm_rmse
lm_mae
lm_r2

#visualization of actual vs predicted values for rank
ggplot(results_rank, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted Bestseller Rank(Linear Regression)",
       x = "Actual Rank",
       y = "Predicted Rank")

#Building a random forest model to predict the rank of the books based on the same features
set.seed(123)
rf_model <- randomForest(
  rank ~ price_avg + pages + genreRomance + genreFantasy + genreThriller,
  data = train_data,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

# Predictions
rf_preds <- predict(rf_model, newdata = test_data)

# Performance metrics
rf_rmse <- sqrt(mean((test_data$rank - rf_preds)^2))
rf_mae  <- mean(abs(test_data$rank - rf_preds))
rf_r2   <- 1 - sum((test_data$rank - rf_preds)^2) / sum((test_data$rank - mean(test_data$rank))^2)

rf_rmse
rf_mae
rf_r2

#Compare actual vs predicted
results_rf <- data.frame(
  Actual = test_data$rank,
  Predicted = rf_preds)

#visualization of actual vs predicted values for rank
ggplot(results_rf, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted Bestseller Rank(Random Forest)",
       x = "Actual Rank",
       y = "Predicted Rank")

#Comparing the performance of the two models
performance_comparison <- data.frame(
  Model = c("Linear Regression", "Random Forest"),
  RMSE = c(lm_rmse, rf_rmse),
  MAE = c(lm_mae, rf_mae),
  R2 = c(lm_r2, rf_r2))
print(performance_comparison)

#visualization of model performance comparison
performance_comparison_long <- performance_comparison %>%
  pivot_longer(cols = c(RMSE, MAE, R2), names_to
                   = "Metric", values_to = "Value")
ggplot(performance_comparison_long, aes(x = Model, y = Value, fill = Model)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Model Performance Comparison",
       x = "Model",
       y = "Value") +
  theme_minimal()

#The random forest model performed better than the linear regression model 
#in predicting the bestseller rank of the books, as it had a lower RMSE and MAE, 
#and a higher R-squared value. This suggests that the relationship between the features and the target variable 
#is likely non-linear, which is better captured by the random forest model.


#rm(list)
