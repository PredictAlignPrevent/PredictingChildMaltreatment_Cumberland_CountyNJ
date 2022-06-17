# Session 1: R Basics

# Exercise 1: Basic data structures
## A. create a vector of integers from 1 to 2022 and sum the result

a <- seq(1, 2022)
sum(a)

## B. create a 3x3 matrix of i^3 for i=1 to 9 and sum the third row

i <- 1:9
b <- matrix(i^3, nrow=3, byrow=TRUE)
sum(b[3,])

# Exercise 2: Conditionals and loops
## A. Assign a positive numeric value to variable x, and write an 
##    if/else statement that will multiply x by 10 if x is less than 
##    100 or by 3 if x is greater than 100. Assign the value to a new variable.

x <- 20
if(x < 100){
  y <- x * 10
}else{
  y <- x * 3
}

## B. Write a code snippet that prints integers 1 to 25, and prints “fizz” 
##    if the integer is divisible by 3, “buzz” if the integer is divisible by 5, 
##    and “fizzbuzz” if the integer is divisible by both 3 and 5.

for(i in 1:25){
  if((i %% 3) == 0){
    if((i %% 5) == 0){
      print(i)
      print('fizzbuzz')
    }else{
      print(i)
      print('buzz')
    }
  }else if((i %% 5) == 0){
    print(i)
    print('fizz')
  }
}

# Exercise 3: Modifying vectors
## A. Create a vector with 10 even numbers using the seq() function, then 
##    create a new vector with 10 odd numbers by modifying the first vector

evens <- seq(4, 10, 2)
odds <- evens + 1

## B. Use a for-loop to combine your two vectors from part A, and sort the values

all <- c()
for(i in 1:length(evens)){
  all <- c(all, evens[i])
  all <- c(all, odds[i])
}

all_2 <- sort(c(evens, odds))

# Exercise 4: Working with dataframes
## A. Load the iris dataset and convert to a tbl_df

library(tidyverse)
data(iris)
head(iris) # no row names, so we don't need to make a new column
iris <- tibble(iris)

## B. Make a summary of the dataset
?iris # check the help for info -- all columns are numeric
# get descriptive stats summaries for numeric data
summary(iris)

## C. Make a pairwise scatterplot
iris %>% dplyr::select(-Species) %>% pairs(pch=19)

## D. Pivot the data and make faceted histograms
iris_long <- iris %>% pivot_longer(!Species, names_to="variable", values_to="value")
ggplot(data=iris_long, aes(value)) +
  geom_histogram(bins=10) +
  facet_wrap(~variable, scales="free")

