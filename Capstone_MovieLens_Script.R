##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


##################################################################################
# Exploratory data analysis (EDA) & Feature engineering as needed

#Loading of necessary libraries
library(scales)

#Exploratory Data Analysis (EDA) is performed to understand the data and determine the direction of model building.
#First, characterize the data set from various angles
head(edx)
str(edx)

#Add a year column
edx <- edx %>%
  mutate(Movie_year = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))),title = str_remove(title, "[/(]\\d{4}[/)]$"))

#See how the whole thing is going.
edx %>% 
  summarise(Movies_n = n(),
            Avg_Rating = mean(rating))


#Check the distribution of ratings
edx %>% ggplot(aes(rating))+
  geom_histogram()+
  scale_x_continuous(breaks = seq(0.0,5.0,0.5))+
  scale_y_continuous(breaks = seq(0,30000000,500000),labels = label_comma())+
  labs(title = "Distribution by Rating")


#Check the latest and oldest release years
summary(edx$Movie_year)

#Check ratings by release year
edx %>% 
  group_by(Movie_year) %>% 
  summarise(Avg_rating = mean(rating)) %>% 
  ggplot(aes(Movie_year,Avg_rating))+
  geom_point()+
  labs(title = "Avg Rating by Movie Release Year")+
  scale_y_continuous(breaks = seq(3.0,5.0,0.5),limits = c(3.0,5.0))

#Check the number of movies by release years
edx %>% 
  group_by(Movie_year) %>% 
  summarise(n_Movies = n()) %>% 
  ggplot(aes(Movie_year,n_Movies))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Number of Movies by Release years")+
  scale_y_continuous(breaks = seq(0,800000,100000),limits = c(0,800000),labels = label_comma())+
  scale_x_continuous(breaks = seq(1915,2008,5),limits = c(1915,2008))

#Check the Unique number of movies
n_distinct(edx$movieId)

#Check the number of views and average rating
edx %>% group_by(movieId,title) %>% 
  summarise(Review_times = n(),
            Avg_rating = mean(rating)) %>% 
  arrange(desc(Review_times))

#Check the number of viewers by movie
#The following checks confirm that no one film is seen more than once by the same person
edx %>% group_by(movieId,title) %>% 
  summarise(Views = n()) %>% 
  arrange(desc(Views)) %>% 
  ggplot(aes(Views))+
  geom_histogram()+
  scale_x_continuous(breaks = seq(0,32000,5000),labels = label_comma())+
  labs(title = "Histogram of number of views")

edx %>% group_by(userId) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

edx %>% group_by(userId,movieId) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

edx_views_summarise <- edx %>% group_by(movieId,title) %>% 
  summarise(Views = n(),
            Avg_rating = mean(rating)) %>% 
  arrange(desc(Views)) %>% 
  ungroup() %>% 
  mutate(Total_Views = sum(Views),
         Proportion = Views / Total_Views)

summary(edx_views_summarise$Views)
sd(edx_views_summarise$Views)

edx_views_summarise


#Add a column for elapsed years when the latest release year is 2008 and that is the base year.
edx <- edx %>% 
  mutate(Elapsed_year = 2008 - Movie_year)

summary(edx$Elapsed_year)

edx %>% 
  mutate(Elapsed_year = 2008 - Movie_year) %>% 
  group_by(Elapsed_year) %>% 
  summarise(n_Movies = n()) %>% 
  ggplot(aes(Elapsed_year,n_Movies))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Number of Movies by Elapsed years")+
  scale_y_continuous(breaks = seq(0,800000,100000),limits = c(0,800000),labels = label_comma())+
  scale_x_continuous(breaks = seq(0,95,5),limits = c(0,95))


#Check the number of movies and ratings by genre
edx %>% 
  group_by(genres) %>% 
  summarise(Movies_n = n(),
            Avg_Rating = mean(rating)) %>% 
  arrange(desc(Movies_n))

#Unique genre check
edx %>% 
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n(),
            Avg_rating = mean(rating)) %>%
  arrange(desc(Avg_rating))

#Number of movies and ratings by year.
edx %>% group_by(Movie_year) %>% 
  summarise(Movies_n =n(),
            Avg_Rating = mean(rating)) %>% 
  ggplot(aes(Movie_year,Avg_Rating))+
  geom_line()

edx %>% group_by(Movie_year) %>% 
  summarise(Movies_n =n(),
            Avg_Rating = mean(rating)) %>% 
  ggplot(aes(Movie_year,Movies_n))+
  geom_line()+
  scale_y_continuous(breaks = seq(0,800000,100000),limits = c(0,800000),labels = label_comma())+
  labs(title = "Number of movies by year")


##################################
#Now that I have a general understanding of the data set, I will move on to the modeling phase.
#First, I will separate the dataset from edx into train set and test set.
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

#Ensure that the userId and movieId from the test set are also included in the training set.
test_set <- temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Next, the rows removed from the test set are added to the training set.
removed <- anti_join(temp, test_set)

train_set <- rbind(train_set, removed)

#Delete temporary files to keep the environment tidy
rm(test_index, temp, removed)




##################################
#Creating RMSE Functions
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Doing model development in the spirit of trial and error.

#1　Simple Average Model
mu <- mean(train_set$rating)
mu

Model_1_RMSE <- RMSE(test_set$rating,mu)
Model_1_RMSE



#2　Building a model that takes into account User effects and Movie effects

#Movie Effects
ME <- train_set %>% 
  group_by(movieId) %>% 
  summarise(ME = mean(rating - mu))

ME

#Distribution Visualization
ME %>% ggplot(aes(x = ME)) + 
  geom_histogram(bins=10, col = I("green3")) +
  ggtitle("Movie Effect Distribution (ME) ") +
  theme(panel.grid = element_blank(),axis.title.y = element_blank())+
  scale_y_continuous(breaks = seq(0,4000,500),limits = c(0,4000),labels = label_comma())


#User Effects
UE <- train_set %>%
  left_join(ME,by = "movieId") %>% 
  group_by(userId) %>% 
  summarise(UE = mean(rating - mu - ME))

UE

#Distribution Visualization
UE %>% ggplot(aes(x = UE)) + 
  geom_histogram(bins=10, col = I("orange3")) +
  ggtitle("User Effect Distribution (UE) ") +
  theme(panel.grid = element_blank(),axis.title.y = element_blank())+
  scale_y_continuous(breaks = seq(0,40000,5000),limits = c(0,40000),labels = label_comma())

#Confirmation of RMSE in this model
predicted_ratings <- test_set %>% 
  left_join(ME, by="movieId") %>%
  left_join(UE, by="userId") %>%
  mutate(prediction = mu + ME + UE) %>%
  .$prediction

summary(predicted_ratings)

Model_2_RMSE <- RMSE(test_set$rating,predicted_ratings)
Model_2_RMSE

#3　Further regularized model by taking into account User effects and Movie effects
#The term "regularization" is derived from the regular matrix of linear algebra.
#The purpose of regularization in machine learning is to avoid over-fitting.
#Simpler explanation is that it has the effect of simplifying complex models.
#Lambda is also the most versatile method that can be used to prevent over-learning in any analysis method.
#Lambda: regularization parameter This adjusts the influence of the regularization term

#Create a sequence of values for lambda ranging from 0 to 10 with 0.25 increments
lambda <- seq(0, 10, 0.25)

#After building the regularization model, the ratings are predicted and the RMSE at each lambda value is calculated.
RMSES <- sapply(lambda, function(l){
  ME <- train_set %>%
    group_by(movieId) %>%
    summarise(ME = sum(rating - mu)/(n()+l))
  UE <- train_set %>%
    left_join(ME, by="movieId") %>%
    group_by(userId) %>%
    summarise(UE = sum(rating - ME - mu)/(n()+l))
  predicted_ratings <- test_set %>%
    left_join(ME, by="movieId") %>%
    left_join(UE, by="userId") %>%
    mutate(pred = mu + ME + UE) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})

#Assign optimal regularization parameters, i.e., lambda
lambda <- lambda[which.min(RMSES)]
lambda

#Apply and validate the regularized model against the validation data set
ME <- edx %>%
  group_by(movieId) %>%
  summarise(ME = sum(rating - mu)/(n()+lambda))

UE <- edx %>%
  left_join(ME, by="movieId") %>%
  group_by(userId) %>%
  summarise(UE = sum(rating - ME - mu)/(n()+lambda))

#Ratings prediction for "final_holdout_test" indicated in the course
predicted_ratings <- final_holdout_test %>%
  left_join(ME, by="movieId") %>%
  left_join(UE, by="userId") %>%
  mutate(pred = mu + ME + UE) %>%
  pull(pred)

#Calculate RMSE and evaluate accuracy
rmse_valid_result <- RMSE(final_holdout_test$rating, predicted_ratings)
rmse_valid_result

##################################################################
# Comments
##################################################################
#I managed to achieve my RMSE score through trial and error.
#I would have liked to try the matrix factorization approach, but due to the looming deadline, I avoided it this time.
#I would like to try that approach someday.

##################################################################
# References
##################################################################
#1.https://atmarkit.itmedia.co.jp/ait/articles/2108/27/news013.html
#2.https://qiita.com/yo_fuji/items/56a22c9829d40ce7a3ff
#3.https://note.com/ryuichiro/n/n3ef1fc1026f6
#4.https://data-viz-lab.com/overfitting
#5.https://qiita.com/c60evaporator/items/784f0640004be4eefc51

