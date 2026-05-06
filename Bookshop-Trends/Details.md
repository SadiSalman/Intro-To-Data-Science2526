Scraping and modeling bestseller book data — Random Forest beats Linear Regression in predicting rank

Project Description
This project builds a complete pipeline to scrape, clean, and model book data from Book Owls BD, an online bookstore. The workflow begins with automated web scraping using rvest and parallel processing (future.apply) to collect book titles, authors, prices, pages, and product details across multiple pages. The raw data is then cleaned and enriched:

1. Extracted page counts, summaries, and genres from product descriptions.
2. Normalized and transformed numeric features (price_avg, pages) with log scaling and outlier capping.
3. Encoded categorical variables (genres) using one‑hot encoding.
4. Created additional categorical bins for price ranges and page length categories.
5. Exploratory data analysis (EDA) was performed with ggplot2 to visualize distributions, genre popularity, and relationships between price, pages, and bestseller rank.
6. For predictive modeling, the dataset was split into training (70%) and testing (30%) sets. Two models were applied:
7. Linear Regression → provided interpretable coefficients but explained only ~9% of variance in rank, with high residual error.
8. Random Forest → captured non‑linear relationships, achieving R² ≈ 0.685, RMSE ≈ 30.2, and MAE ≈ 23.4, significantly outperforming regression.
9. Visualizations of actual vs predicted ranks and model performance comparison confirmed that Random Forest is the most suitable model for predicting bestseller rank.