This project is essentially an Exploratory Data Analysis (EDA) exercise on Formula 1 performance data. The team worked with the Kaggle "Formula 1 World Championship (1950–2020)" dataset, focusing on the hybrid-engine era (2014–2024). To make the project more challenging, intentional data quality issues were introduced—such as missing values disguised as \N, duplicates, and unrealistic outliers—which required cleaning before analysis.

The workflow involved:
1. Importing datasets into R using read.table() and organizing them into data frames.
2. Installing and using packages like dplyr for manipulation and caTools for train-test splitting.
3. Handling missing values by converting placeholders (\N, empty strings, NULL) into proper NA values.
4. Checking data integrity with functions like colSums(is.na()) to quantify missingness across tables.

In short, this stage of the project was about data preparation and exploration—ensuring the raw F1 datasets were cleaned, structured, and ready for deeper statistical analysis and modeling. It sets the foundation for later predictive tasks by validating the dataset’s reliability.
