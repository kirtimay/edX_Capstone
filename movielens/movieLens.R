## Kirtimay Pendse
## MovieLens Project Submission
## HarvardX: PH125.9x Data Science: Capstone
## https://github.com/kirtimay/edX_Capstone/

################################
# 1. Create edx set, validation set
################################

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
set.seed(1, sample.kind="Rounding")
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

#How many rows and columns are there in the edx dataset?
dim(edx)

#How many zeros were given as ratings in the edx dataset?
edx %>% filter(rating == 0) %>% tally()

#How many threes were given as ratings in the edx dataset?
edx %>% filter(rating == 3) %>% tally()

#How many different movies are in the edx dataset?
n_distinct(edx$movieId)

#How many different users are in the edx dataset?
n_distinct(edx$userId)

#How many movie ratings are in each of the following genres in the edx dataset?
#Drama
drama <- edx %>% filter(str_detect(genres,"Drama"))
paste('Drama has', nrow(drama), 'movies')
#Comedy
comedy <- edx %>% filter(str_detect(genres, "Comedy"))
paste('Comedy has', nrow(comedy), 'movies')
#Thriller
thriller <- edx %>% filter(str_detect(genres, "Thriller"))
paste('Thriller has', nrow(thriller), 'movies')
#Romance
romance <- edx %>% filter(str_detect(genres, "Romance"))
paste('Romance has', nrow(romance), 'movies')

#Which movie has the greatest number of ratings?
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#What are the five most given ratings in order from most to least?
edx %>% 
  group_by(rating) %>% 
  summarize(count = n()) %>% 
  top_n(5) %>%
  arrange(desc(count))

#True or False- half star ratings are less common than whole star ratings
edx %>% group_by(rating) %>% summarize(count = n()) #T



  
