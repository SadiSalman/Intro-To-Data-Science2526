a <- 100
a <- c(100, 200, 300)

b <- c(5, 6, 7)
sumAB <- a+b
pdctAB <- a*b

summary(b)
mean(a)
median(c(500, 600, 700))
var(b)
sqrt(3546)
sd(b)
length(b)


add_numbers <- function(a, b) 
{ 
  return(a + b) 
}
add_numbers(5,6)


abs(-474)
round(34734.34357378)
log(1)
exp(2)

seq(a)
rep(b)
rep(a)
seq(b)
sort(c(432, 652, 192, 5632, 7, 892))

u<- c("a", "b", "c", "d")
u <- c(u, "e", "f", "g")
append(u, "i", after = 11)


num_vector <- c(10, 20, 30, 40, 50)
print(num_vector[3])

num_vector[3] <- 35
num_vector[c(2,5)] <- c(25,55)

num_vector[num_vector>25]
num_vector[which(num_vector>25)]                                                                          




#Lab2_26-2-2026__________________________________________

#sorting, sequencing, repeating
ranVector <- c(223, 32, 234, 242, 565, 895, 23)
sortedVector <- sort(ranVector, decreasing = FALSE)

seq_vec <- seq(11, 100, 2)

rep_vector <- c(1, rep(2,2), rep(3, 3), rep(4, 4), rep(5, 5))
rep(c(1,2,3,4,5),times = c(1,2,3,4,5))
rep(c(1:5),times = c(1:5))

#matrices
mat1 <- matrix(seq(3, 27,3), nrow =3, ncol = 3, byrow = TRUE)
matrix(c(3, 6, 9, 12, 15, 18, 21, 24, 27))

#changing row and col names
rownames(mat1) <- c("R1", "R2", "R3")
colnames(mat1) <- c("C1", "C2", "C3")

#Accessing matrix elements
mat1[2,3]
(mat1[2,3] + 2) /10

mat1[c(2,3), c(2,3)]
mat1[c(1,3), c(1,3)]

mat1[1,]
mat1[,3]

#matrix arithmetic
mat2 <- matrix(seq(1,9,3), nrow =3, ncol = 3, byrow = TRUE)

mat1 + mat2
mat1 * mat2
mat1 %% mat2

#transpose and inverse of matrices


#Arrays
array(1:50, dim = c (3, 3, 2))

#Lab3_05-03-2026__________________________________________

#Layering
ar2 <- array(1:50, dim = c(2, 2, 4))

ar2[ ,2,3]
ar2[c(1,2),2, 3]
ar2[1:2, 2, 3]

#applying operations on specific rows and columns

#apply(arrayName, MARGIN = 1 or 2, FUN = operation)
apply(ar2, MARGIN = 1, FUN = sum)

#data frames
df <- data.frame(
  ID= c(1, 2, 3, 4, 5),
  Name = c("Alice", "Bob", "Charlie", "David", "Eve"),
  Age = c(25, 30, 35, 40, 45),
  Score = c(85, 90, 43, 80, 46),
  Passed = c(TRUE, TRUE, FALSE, TRUE, FALSE)
)
print(df)

#accessing data frames
mean(df$Age)
print(df$Name)
summary(df$Score)

#filtering records
print(df[2,])

print(df[df$Age > 35,])
print(df[df$Age > mean(df$Age),])

print(df[df$Age > 35,c(2,4)])
print(df[df$Age > 35,c("Name","Score")])

print(df[df$Passed == TRUE,])

#adding new cols or rows
df$Grade <- c("B", "A", "F", "B", "F")

#ordering/sorting
print(df[order(df$Age)])
print(df[order(-df$Age)])
print(df[order(df$Age, decreasing = TRUE)])

#modifying data
df$Score[df$Name == "Bob"] <- 80
df$Grade[df$Name == "Bob"] <- "B"

df[df$Name == "Bob", "Score"] <- 80

df$Score <- df$Score + 5


#--------------------
#16-04-2026


#using the iris dataset
mean(iris$Sepal.Length[iris$Species == "setosa"])
median(iris$Sepal.Length[iris$Species == "setosa"])
var(iris$Sepal.Length[iris$Species == "setosa"])
sd(iris$Sepal.Length[iris$Species == "setosa"])

library(dplyr)

#using pipe operator to calculate summary stats for each species
iris %>%
  group_by(Species) %>%
  summarise(
    count = n(),
    mean_sepal_length = mean(Sepal.Length),
    median_sepal_length = median(Sepal.Length),
    var_sepal_length = var(Sepal.Length),
    sd_sepal_length = sd(Sepal.Length),
    
    
    mean_petal_length = mean(Petal.Length),
    median_petal_length = median(Petal.Length),
    var_petal_length = var(Petal.Length),
    sd_petal_length = sd(Petal.Length)
  )

pairs(iris[,1:4], main = "Scatterplot", col = iris$Species, pch = 19)

#scrapping data from web
install.packages("rvest")
library(rvest)
url <- "https://books.toscrape.com/"
webpage <- read_html(url)

#imdb <- read_html("https://www.imdb.com/chart/top")-----Blocked

#scrapping using node selector
title <- webpage %>%
  html_nodes(".product_pod h3 a") %>%
  html_attr("title")

#scrpping using element selector
price <- webpage %>%
  html_element(".price_color") %>%
  html_text()

#______________23-04-2026__________________________

#using mtcars dataset
install.packages("ggplot2")
library(ggplot2)

#using base R to create a scatter plot
print(mtcars)
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  labs(title = "Horsepower vs Miles Per Gallon",
       x = "Horsepower (hp)",
       y = "Miles Per Gallon (mpg)")+
  theme_minimal()

#using  a box plot function
#{ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
 # geom_boxplot() +
 # labs(title = "Miles Per Gallon by Number of Cylinders",
    #   x = "Number of Cylinders",
  #     y = "Miles Per Gallon (mpg)")+
  #fill = "lightblue"}
  
#using ggcorrplot to visualize correlation

install.packages("ggcorrplot")
library(ggcorrplot)
cor_matrix <- cor(mtcars[, sapply(mtcars, is.numeric)])
ggcorrplot(cor_matrix, method = "circle", type = "lower", lab = TRUE, title = "Correlation Matrix of mtcars Dataset")


#handling missing values
library(dplyr)
cleaned_mtcars <- na.omit(mtcars)

cleaned_mtcars <- mtcars %>%
  na.omit()

#data wrangling with dplyr
#filtering records
mtcars_filtered <- cleaned_mtcars %>%
  filter(mpg> 20 & hp > 100)

#selecting specific columns
mtcars_selected <- cleaned_mtcars %>%
  select(mpg, hp, disp, carb)

#using both select and filter together
mtcars_filtered_selected <- cleaned_mtcars %>%
  filter(hp > 100) %>%
  select(mpg, hp, wt, vs)

#using mutate to create new columns
mtcars_mutated <- mtcars %>%
  mutate(mpg_per_hp = mpg / hp, 
         weight_class = ifelse(wt > 3, "Heavy", "Light"))

#scaling data

  