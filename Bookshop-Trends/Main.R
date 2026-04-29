#importing libraries
library(dplyr)
library(rvest)
library(stringr)
library(future.apply)
plan(multisession, workers = 7) # Setting up parallel processing with 5 workers

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

#Combining all the cleaned data frames into one final data frame
books_clean <- bind_rows(books_clean1, books_clean2, books_clean3, books_clean4, books_clean5, books_clean6)
books_clean <- books_clean %>% distinct() #removing duplicate rows

#removing unnecessary data frames and variables to free up memory
rm(books_data1, books_data2, books_data3, books_data4, books5, books6, 
   books_clean1, books_clean2, books_clean3, books_clean4, books_clean5, books_clean6,
   details_df1, details_df2, details_df3, details_df4, details_df5, details_df6)
rm(link1t5, link6t10, link11t15, link16t20, link21t25, link26t30)

#Keeping only the numeric value in the price column
books_clean$price <- as.numeric(str_extract(books_clean$price, "\\d+\\.?\\d*"))

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
  keywords <- c("Fiction", "Non-Fiction", "Science Fiction", "Fantasy", "Mystery", 
                "Thriller", "Romance", "Biography", "History", "Self Help", 
                "Children's", "Young Adult", "Horror", "Classic", "Poetry", 
                "Graphic Novel", "Novel", "Short Story", "Essay", "Memoir", 
                "Travel", "Science", "Philosophy", "Religion", "Humor", 
                "Business", "Health", "Cooking", "Photography")
  
  # Convert both details and keywords to lowercase for comparison
  details_lower <- tolower(details)
  keywords_lower <- tolower(keywords)
  
  # Check each keyword individually
  matches <- keywords[sapply(keywords_lower, function(k) str_detect(details_lower, fixed(k)))]
  
  # Collapse into a string if found, else NA
  if(length(matches) > 0) {
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

# Printing the final data frame
print(books_clean)

#rm(list = ls())