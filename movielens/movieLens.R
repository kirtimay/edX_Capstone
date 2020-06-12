## Kirtimay Pendse
## MovieLens Project Submission
## HarvardX: PH125.9x Data Science: Capstone
## https://github.com/kirtimay/edX_Capstone/

#####################################################
# 1. Create edx set, validation set (the following code was given by the edX staff)
#####################################################

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

################################
# 2. Exploratory Data Analysis
################################

head(edx) %>% print.data.frame() #to inspect the data briefly, and get a 'feel'

sapply(edx, class) #ascertain the class of each variable

summary(edx) #get summary stats for the individual variables

### Inspect the variables in more detail

#Date

library(lubridate)

if(!require(ggthemes)) 
  install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(scales)) 
  install.packages("scales", repos = "http://cran.us.r-project.org")

edx %>% mutate(year = year(as_datetime(timestamp, origin="1970-01-01"))) %>%
  ggplot(aes(x=year)) +
  geom_histogram(color = "black", bins = 30) + 
  ggtitle("Distribution of Ratings across Years") +
  xlab("Year") +
  ylab("# Ratings") +
  scale_y_continuous(labels = comma) 

#Movies

edx %>% group_by(movieId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "black", bins = 30) +
  scale_x_log10() + 
  ggtitle("Distribution of Movies") +
  xlab("# Ratings") +
  ylab("# Movies") 

#Users

edx %>% group_by(userId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "black", bins = 30) +
  scale_x_log10() + 
  ggtitle("Distribution of Users") +
  xlab("# Ratings") +
  ylab("# Users") + 
  scale_y_continuous(labels = comma) 

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
  ylab("Count") 

################################
# 3. Building the Model
################################

#Defining the RMSE, our tool for evaluation

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}









  
