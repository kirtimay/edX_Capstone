## Kirtimay Pendse
## MovieLens Project Submission
## HarvardX: PH125.9x Data Science: Capstone
## https://github.com/kirtimay/edX_Capstone/

################################
# 1. Create edx set, validation set 
################################

#Loading required packages
library(lubridate)
if(!require(ggthemes)) 
  install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(scales)) 
  install.packages("scales", repos = "http://cran.us.r-project.org")
library(dplyr)
library(knitr)
library(ggplot2)
library(dslabs)
library(lubridate)


#(the following code was given by the edX staff)
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") #use set.seed(1) if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% #validation will be our test set
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed) #edx is training set

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Splitting the edx dataset into a train set and test set using p=0.1
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

#To ensure userId and movieId in the test set are also in the train set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Adding rows removed from the test set back into the train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)
rm(test_index, temp, removed)

################################
# 2. Methods and Analysis
################################

#Exploratory Data Analysis

head(edx) %>% print.data.frame() #to inspect the data briefly, and get a 'feel'

sapply(edx, class) #ascertain the class of each variable

summary(edx) #get summary stats for the individual variables

### Inspect the variables in more detail

#Date

edx %>% mutate(year = year(as_datetime(timestamp, origin="1970-01-01"))) %>%
  ggplot(aes(x=year)) +
  geom_histogram(color = "black", bins = 30) + 
  ggtitle("Distribution of Ratings per Year") +
  xlab("Year") +
  ylab("# Ratings") +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(hjust = 0.5))

#Time

edx %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth() +
  xlab("Year")+
  ylab("Rating")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Distribution of Ratings across Years")

#Movies

edx %>% group_by(movieId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "black", bins = 30) +
  scale_x_log10() + 
  ggtitle("Distribution of Movies") +
  xlab("# Ratings") +
  ylab("# Movies") +
  theme(plot.title = element_text(hjust = 0.5))

#Users

edx %>% group_by(userId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "black", bins = 30) +
  scale_x_log10() + 
  ggtitle("Distribution of Users") +
  xlab("# Ratings") +
  ylab("# Users") + 
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(hjust = 0.5))

#Ratings

edx %>% group_by(rating) %>% 
  summarise(count=n()) %>%
  ggplot(aes(x=rating, y=count)) + 
  geom_line() +
  geom_point() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  ggtitle("Distribution of Ratings") + 
  xlab("Rating") +
  ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5)) 

################################
# 3. Results
################################

#Defining the RMSE, our tool for evaluation

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Predicting Randomly

set.seed(43, sample.kind = "Rounding")

#To create a probability distribution of each rating
p <- function(x, y) mean(y == x)
rating <- seq(0.5,5,0.5)

#Using a MC simulation to estimate the probability
B <- 10000
M <- replicate(B, {
  z <- sample(train_set$rating, 100, replace = T)
  sapply(rating, p, y=z)
})
prob_1 <- sapply(1:nrow(M), function(x) mean(M[x,]))

#To randomly predict ratings
y_hat <- sample(rating, size = nrow(test_set), 
                replace = TRUE, prob = prob_1)

#Store results in a table
results_table <- tibble('Method of Analysis' = "Desired RMSE", RMSE = 0.8649)
results_table <- bind_rows(results_table, 
                           tibble('Method of Analysis' = "Predicting Randomly", 
                                  RMSE = RMSE(test_set$rating, y_hat)))
results_table

#Linear Models

##Average Rating

#Calculating the mean of observed values in the train_set
mu <- mean(train_set$rating)

#Adding the results to the RMSE table  
results_table <- bind_rows(results_table, 
                           tibble('Method of Analysis'= "Using the Average Rating", 
                                  RMSE = RMSE(test_set$rating, mu)))
results_table

##Movie Effect

#Calculating bi (the movie effect)
bi <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

#Predicting user rating with mean & bi  
y_hat_bi <- mu + test_set %>% 
  left_join(bi, by = "movieId") %>% 
  .$b_i

#Add the results
results_table <- bind_rows(results_table, 
                           tibble('Method of Analysis' = "Including Movie Effect", 
                                  RMSE = RMSE(test_set$rating, y_hat_bi)))
results_table

##User Effect

#Calculating bu (the user effect)
bu <- train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))

#Predicting user rating with mean & bu  
y_hat_bu <- mu + test_set %>% 
  left_join(bu, by = "userId") %>% 
  .$b_u

#Add the results
results_table <- bind_rows(results_table, 
                           tibble('Method of Analysis' = "Including User Effect", 
                                  RMSE = RMSE(test_set$rating, y_hat_bu)))
results_table

##Movie and User Effects

#Predicting user rating with mu, bi and bu
y_hat_bi_bu <- test_set %>% 
  left_join(bi, by='movieId') %>%
  left_join(bu, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

#Add the results
results_table <- bind_rows(results_table, 
                           tibble('Method of Analysis' = "Including Movie and User Effects", 
                                  RMSE = RMSE(test_set$rating, y_hat_bi_bu)))
results_table

#Regularization

#finding optimal lambda
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

#Pick the lambda that minimizes rmse
lambda <- lambdas[which.min(rmses)]
lambda

#This can be confirmed via the following plot as well:
qplot(lambdas, rmses) 

#Creating a regularized model

##Calculating mu again
mu <- mean(train_set$rating)

###Calculating regularized bi (the regularized movie effect)
b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

###Calculating regularized bu (the regularized user effect)
b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

#Predict a regularized estimate of y_hat
reg_y_hat <- test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

#Add the results
results_table <- bind_rows(results_table, 
                           tibble('Method of Analysis' = "Regularized Movie and User Effects", 
                                  RMSE = RMSE(test_set$rating, reg_y_hat)))
results_table

#Final Validation

#calculate the mean of the edx set
mu_edx <- mean(edx$rating)

#Calculating bi (the movie effect) on the edx dataset
b_i_edx <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_edx)/(n()+lambda))

#Calculating bu (the user effect) on the edx dataset
b_u_edx <- edx %>% 
  left_join(b_i_edx, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_edx)/(n()+lambda))

#Predict a regularized estimate of y_hat
y_hat_edx <- validation %>% 
  left_join(b_i_edx, by = "movieId") %>%
  left_join(b_u_edx, by = "userId") %>%
  mutate(pred = mu_edx + b_u + b_i) %>%
  pull(pred)

#Add the results
results_table <- bind_rows(results_table, 
                           tibble('Method of Analysis' = "Regularized Movie and User Effects- Final Validation", 
                                  RMSE = RMSE(validation$rating, y_hat_edx)))
results_table 

#Appendix
print("Operating System:")
version









  
