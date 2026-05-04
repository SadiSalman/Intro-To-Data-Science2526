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
link31t35 <- paste0("https://www.bookowlsbd.com/collections/frontpage?page=", 31:35)
link36t40 <- paste0("https://www.bookowlsbd.com/collections/frontpage?page=", 36:40)
link41t45 <- paste0("https://www.bookowlsbd.com/collections/frontpage?page=", 41:45)
link46t50 <- paste0("https://www.bookowlsbd.com/collections/frontpage?page=", 46:50)

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
books_data7 <- bind_rows(future_lapply(link31t35, read_webpage))
books_data8 <- bind_rows(future_lapply(link36t40, read_webpage))
books_data9 <- bind_rows(future_lapply(link41t45, read_webpage))
books_data10 <- bind_rows(future_lapply(link46t50, read_webpage))

# Removing non-books (rows with NA author)
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
books_clean7 <- books_data7 %>%
  filter(!is.na(authors))
books_clean8 <- books_data8 %>%
  filter(!is.na(authors))
books_clean9 <- books_data9 %>%
  filter(!is.na(authors))
books_clean10 <- books_data10 %>%
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
details_list7 <- future_lapply(books_clean7$url, get_book_details)
details_list8 <- future_lapply(books_clean8$url, get_book_details)
details_list9 <- future_lapply(books_clean9$url, get_book_details)
details_list10 <- future_lapply(books_clean10$url, get_book_details)

#Binding all the batches into separate data frames
details_df1 <- bind_rows(details_list1)
details_df2 <- bind_rows(details_list2)
details_df3 <- bind_rows(details_list3)
details_df4 <- bind_rows(details_list4)
details_df5 <- bind_rows(details_list5)
details_df6 <- bind_rows(details_list6)
details_df7 <- bind_rows(details_list7)
details_df8 <- bind_rows(details_list8)
details_df9 <- bind_rows(details_list9)
details_df10 <- bind_rows(details_list10)

# Merging page column and details column back to cleaned data frames
books_clean1 <- books_clean1 %>%
  left_join(details_df1, by = "url")
books_clean2 <- books_clean2 %>%
  left_join(details_df2, by = "url")
books_clean3 <- books_clean3 %>%
  left_join(details_df3, by = "url")
books_clean4 <- books_clean4 %>%
  left_join(details_df4, by = "url")
books_clean5 <- books_clean5 %>%
  left_join(details_df5, by = "url")
books_clean6 <- books_clean6 %>%
  left_join(details_df6, by = "url")
books_clean7 <- books_clean7 %>%
  left_join(details_df7, by = "url")
books_clean8 <- books_clean8 %>%
  left_join(details_df8, by = "url")
books_clean9 <- books_clean9 %>%
  left_join(details_df9, by = "url")
books_clean10 <- books_clean10 %>%
  left_join(details_df10, by = "url")

#Combining all the cleaned data frames into one final data frame
books_clean <- bind_rows(books_clean1, books_clean2, books_clean3, books_clean4, books_clean5, books_clean6, books_clean7, books_clean8, books_clean9, books_clean10)
books_clean <- books_clean %>% distinct() #removing duplicate rows

#removing unnecessary data frames and variables to free up memory
rm(books_data1, books_data2, books_data3, books_data4, books5, books6, books7, books8, books6, books9, books10)
rm(books_clean1, books_clean2, books_clean3, books_clean4, books_clean5, books_clean6, books_clean7, books_clean8, books_clean9, books_clean10)
rm(details_df1, details_df2, details_df3, details_df4, details_df5, details_df6, details_df7, details_df8, details_df9, details_df10)
rm(link1t5, link6t10, link11t15, link16t20, link21t25, link26t30, link31t35, link36t40, link41t45, link46t50)

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
    "Biography", "History", "Self Help","Self-Help", "Children's", "Young Adult", "Horror",
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

#Pages vs Price Scatter Plot
ggplot(books_clean, aes(x = pages, y = price_avg)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Pages vs Average Price", x = "Number of Pages", y = "Average Price (BDT)") +
  theme_minimal()

#Boxplot of Price by Format
books_clean_long <- books_clean %>%
  pivot_longer(cols = c(price_paperback, price_hardcover), names_to = "format", values_to = "pivoted_price")
ggplot(books_clean_long, aes(x = format, y = pivoted_price)) +
  geom_boxplot(fill = c("lightblue", "lightgreen")) +
  labs(title = "Price Distribution by Format", x = "Format", y = "Price (BDT)") +
  theme_minimal()

#Genre vs Average Price
books_clean %>%
  # Split multiple genres into separate rows
  separate_rows(genre, sep = ",\\s*") %>%
  group_by(genre) %>%
  summarise(avg_price = mean(price_avg, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(genre, avg_price), y = avg_price, fill = genre)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Average Price by Genre",
       x = "Genre", y = "Average Price")

#Price vs Rank Scatter Plot
books_clean %>%
  # Split multiple genres into separate rows
  separate_rows(genre, sep = ",\\s*") %>%
  ggplot(aes(x = pages, y = rank, color = genre)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Pages vs Bestseller Rank",
       x = "Pages",
       y = "Rank")

#Genre Popularity by rank
books_rank_ready %>%
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
train_index <- createDataPartition(books_encoded$price_avg, p = 0.8, list = FALSE) # 80% for training
train_data <- books_encoded[train_index, ]
test_data <- books_encoded[-train_index, ]

# Fit linear regression model
model_price <- lm(
  price_avg ~ pages + rank + genreAction + genreAdventure + genreRomance + genreThriller,
  data = train_data
)

summary(model_price)

# Predictions
pred_price <- predict(model_price, newdata = test_data)

# Check the first few predictions
head(pred_price)

# Compare predicted vs actual
results <- data.frame(
  Actual = test_data$price_avg,
  Predicted = pred_price
)

head(results)

#Performance metrics
# RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))

# MAE (Mean Absolute Error)
mae <- mean(abs(results$Actual - results$Predicted))

# R-squared (on test data)
sst <- sum((results$Actual - mean(results$Actual))^2)
sse <- sum((results$Actual - results$Predicted)^2)
rsq <- 1 - sse/sst

rmse
mae
rsq

#graphical representation of predicted vs actual values
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted Prices",
       x = "Actual Price",
       y = "Predicted Price")

#Selected features for predicting rank are pages, genre, and price.
# Fit linear regression for rank prediction
model_rank <- lm(
  rank ~ price_avg + pages + genreAction + genreAdventure + genreRomance + genreThriller,
  data = train_data
)

summary(model_rank)

# Predict on test data
pred_rank <- predict(model_rank, newdata = test_data)

# Compare actual vs predicted
results_rank <- data.frame(
  Actual = test_data$rank,
  Predicted = pred_rank
)


head(results_rank)
# RMSE
rmse_rank <- sqrt(mean((results_rank$Actual - results_rank$Predicted)^2))

# MAE
mae_rank <- mean(abs(results_rank$Actual - results_rank$Predicted))

# R-squared
sst <- sum((results_rank$Actual - mean(results_rank$Actual))^2)
sse <- sum((results_rank$Actual - results_rank$Predicted)^2)
rsq_rank <- 1 - sse/sst

rmse_rank
mae_rank
rsq_rank

ggplot(results_rank, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted Bestseller Rank",
       x = "Actual Rank",
       y = "Predicted Rank")


#rm(list = ls())
