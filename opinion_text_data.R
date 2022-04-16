##### Text Data Analysis #####

##### Method 1: Look at text from across all reviews to view topics ####

file_list <- list.files(path = )

df <- data.frame() # empty dataframe

for (i in length(file_list)){
  add_data <- read.delim(file_list[i], head= F)
  df <- rbind(df, add_data)
} # for loop to read in data from folder, iterating through each file in the folder, and union to end of the empty data frame

# end up with 89 records of reviews for various products

### Clean data ###
df[3,] # look at a few different reviews
df[16,]

txt_tbl <- tibble(df) # convert to tibble
colnames(txt_tbl) <- c("text") # add column label to text
txt_tbl$review_index <- 1:nrow(txt_tbl) # add text record index

### Exploratory Data Analysis/Topic Discovery ###

## have a two-column tibble, which is good for looking at each record of text but we can try to analyze text across products
library(stringr) # text cleaning and regular expressions
library(tidytext) # provides additional text mining functions

txt_tbl <- txt_tbl %>% 
  unnest_tokens(word, text)

txt_tbl %>% 
  count(word, sort = T) # word frequency

## can see that participles and other common words are at the top of the list, not necessarily helpful
# can fix with the anti_join functionality of the tidytext package

txt_tbl %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T) # can see that there is a lot of variance in the words

# look at most common words by review
txt_tbl %>% 
  anti_join(stop_words) %>% 
  group_by(review_index) %>% 
  count(word, sort = T) # not very informative

txt_tbl %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T) %>% 
  mutate(text_order = nrow(.):1) %>% # order from last appearance of word in dataframe to first
  ggplot(aes(reorder(word, text_order), n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "none") # graph is quite uninformative

##### Method 2: Look at text in each review to view topics #####

library(tm, SnowballC) # required packages

reviews_raw <- VCorpus(DirSource()) # create a corpus of the reviews text files

## clean the text data in the corpus
reviews_clean <- reviews_raw %>% 
  tm_map(content_transformer(tolower)) %>%  # make all words lowercase 
  tm_map(stripWhitespace) %>% # remove whitespace from reviews
  tm_map(removePunctuation) %>% # remove punctuation from words
  tm_map(removeNumbers) %>% # remove numbers (good to look at in context but hard for analysis)
  tm_map(removeWords, stopwords("English"))

content(reviews_clean[[2]]) # review the full text of the 2nd review in corpus to check

dtm_review <- DocumentTermMatrix(reviews_clean)
dtm_review # create document-term matrix to show for each document, the frequency of the words that show up across thedocuments

dtm_review_mat <- as.matrix(dtm_review) # convert into matrix form for analysis

### Data Analysis/Topic Discovery

## Topic Discovery Step 1: Term Frequency-Inverse Document Frequency
# An algorithm that decreases the weight of frequency of commonly used words and increases
# the weight of frequency of less commonly used words.
# The algorithm is helpful for identifying distinct words used to describe things.

dtm_review_tfidf <- weightTfIdf(dtm_review)
dtm_review_tfidf_mat <- as.matrix(dtm_review_tfidf)

# look at a few examples

# 1. the food at the Holiday Inn in London
head(sort(dtm_review_tfidf_mat[14,], decreasing = T), n = 10) # not super informative except "Indian"

# 2. the interior design of a 2008 Honda Accord
head(sort(dtm_review_tfidf_mat[18,], decreasing = T), n = 10) # "Dash", "roomy", "Leather" appear, could dig deeper

# 3. the staff at the Swissotel in Chicago
head(sort(dtm_review_tfidf_mat[47,], decreasing = T), n = 10) # "Dash", "roomy", "Leather" appear, could dig deeper # sounds like generally good reviews, except a 1% review rate showing "rude"!

# the value of this method is that by pointing out distinctive words, can focus the review of the messages by customers and learn how to improve operations

## Topic Discovery Step 2: Clustering
# Clustering is an unsupervised machine learning method to identify subgroups of operations within a dataset.
# With inputs by the analyst, the algorithm assigns centroids around which topics are grouped according to similar characteristics identified by the algorithm 
# (by minimizing the distance between the data point (in this case, words) and the centroid).
# From the clusters, we can do further analysis/categorization.

library(cluster) # clustering algorithm
library(factoextra) # clustering algorithm and viz

k_rev <- kmeans(dtm_review_tfidf_mat, centers = 3) # from glancing at the corpus, we have 3 main categories (tech, cars, hotel/customer experience)
# runs the algorithm in 25 iterations around these 3 centers

colnames(k_rev$centers) <- colnames(dtm_review_tfidf_mat)

k <- 3 # number of centers

for (i in 1:k){
  cat("Cluster", i, "\n")
  cat("Top 10 words:\n") # labeling top 10 words
  print(head(sort(k_rev$centers[i, ], decreasing = T), n = 10))
  cat("Reviews classified:\n")
  print(rownames(dtm_review_tfidf_mat)[k_rev$cluster == i]) # each review name
} # for loop shows the cluster, the top 10 words in each cluster, and which review files were grouped in each cluster

# looks like the clusters identified three groups: reviews about the battery life of tech, reviews about the interiors of cars, and everything else

# let's try running the k-means clusters again with 2 more clusters added

k_rev <- kmeans(dtm_review_tfidf_mat, centers = 5)

colnames(k_rev$centers) <- colnames(dtm_review_tfidf_mat)

k <- 5 # number of centers

for (i in 1:k){
  cat("Cluster", i, "\n")
  cat("Top 10 words:\n") # labeling top 10 words
  print(head(sort(k_rev$centers[i, ], decreasing = T), n = 10))
  cat("Reviews classified:\n")
  print(rownames(dtm_review_tfidf_mat)[k_rev$cluster == i]) # each review name
} # for loop shows the cluster, the top 10 words in each cluster, and which review files were grouped in each cluster

# interesting development — looks like the clusters identified reviews about the kindle features and battery life, the interior of various cars, and everything else

# let's try to add 3 to 4 more clusters to see differences 

k_rev <- kmeans(dtm_review_tfidf_mat, centers = 8) # 8 clusters

colnames(k_rev$centers) <- colnames(dtm_review_tfidf_mat)

k <- 8 # number of centers

for (i in 1:k){
  cat("Cluster", i, "\n")
  cat("Top 10 words:\n") # labeling top 10 words
  print(head(sort(k_rev$centers[i, ], decreasing = T), n = 10))
  cat("Reviews classified:\n")
  print(rownames(dtm_review_tfidf_mat)[k_rev$cluster == i]) # each review name
} # for loop shows the cluster, the top 10 words in each cluster, and which review files were grouped in each cluster

k_rev <- kmeans(dtm_review_tfidf_mat, centers = 9) # 9 clusters

colnames(k_rev$centers) <- colnames(dtm_review_tfidf_mat)

k <- 9 

for (i in 1:k){
  cat("Cluster", i, "\n")
  cat("Top 10 words:\n") # labeling top 10 words
  print(head(sort(k_rev$centers[i, ], decreasing = T), n = 10))
  cat("Reviews classified:\n")
  print(rownames(dtm_review_tfidf_mat)[k_rev$cluster == i]) # each review name
} # for loop shows the cluster, the top 10 words in each cluster, and which review files were grouped in each cluster

# this shows clusters around the battery life of the kindle, the service at various hotels, the Garmin GPS, and features on the 2007 Toyota Camry and 2008 Honda Accord

## alternatively, we could try clustering on just two centers

k_rev <- kmeans(dtm_review_tfidf_mat, centers = 2) 

colnames(k_rev$centers) <- colnames(dtm_review_tfidf_mat)

k <- 2

for (i in 1:k){
  cat("Cluster", i, "\n")
  cat("Top 10 words:\n") # labeling top 10 words
  print(head(sort(k_rev$centers[i, ], decreasing = T), n = 10))
  cat("Reviews classified:\n")
  print(rownames(dtm_review_tfidf_mat)[k_rev$cluster == i]) # each review name
} # for loop shows the cluster, the top 10 words in each cluster, and which review files were grouped in each cluster

# very strong at identifying the reviews about customer service at various hotels, and bucketing everything else

## Next step: we could pull just the data from the cluster about hotels and do sentiment analysis

#### Topic Discovery Step 3: Sentiment Analysis

## pull review data from corpus for just hotel reviews

hotel_review_list <- c("bathroom_bestwestern_hotel_sfo.txt.data", "food_holiday_inn_london.txt.data",
                       "food_swissotel_chicago.txt.data", "free_bestwestern_hotel_sfo.txt.data",
                       "location_bestwestern_hotel_sfo.txt.data", "location_holiday_inn_london.txt.data",
                       "parking_bestwestern_hotel_sfo.txt.data", "price_holiday_inn_london.txt.data",
                       "room_holiday_inn_london.txt.data", "rooms_bestwestern_hotel_sfo.txt.data", 
                       "rooms_swissotel_chicago.txt.data", "service_bestwestern_hotel_sfo.txt.data", 
                       "service_holiday_inn_london.txt.data", "service_swissotel_hotel_chicago.txt.data", 
                       "staff_bestwestern_hotel_sfo.txt.data", "staff_swissotel_chicago.txt.data")

tm_filter(reviews_clean, function(x)meta(x)[["id"]] %in% hotel_review_list) # test to see it works

hotel_review_corpus <- tm_filter(reviews_clean, function(x)meta(x)[["id"]] %in% hotel_review_list) # create filtered corpus

## put text into new dataframe for analysis

meta(hotel_review_corpus[[2]], tag= "id") # pull name of review

(test_2 <- tibble(review_name = meta(hotel_review_corpus[[2]], tag= "id"),
       text = content(hotel_review_corpus[[2]])) %>% 
  unnest_tokens(word,text)) # check that this will work for tibble

hotel_review_text = data.frame() # create empty data frame

for (i in seq_along(hotel_review_corpus)){
  add_data <- tibble(review_name = meta(hotel_review_corpus[[i]], tag = "id"),
                                        text = content(hotel_review_corpus[[i]]))
  
  hotel_review_text <- rbind(hotel_review_text, add_data)
} ## loop through corpus and add text from each review

hotel_review_text <- hotel_review_text %>% 
  unnest_tokens(word, text) # create tibble of ~32k words grouped by each hotel review

## join sentiments data to existing data frame

get_sentiments("nrc") # sentiment lexicon developed by Saif Mohammad and Peter Turney

hotel_review_text <- hotel_review_text %>% 
  left_join(get_sentiments("nrc")) # keep everything in the hotel review text dataframe and join in sentiment column where there is a match 

hotel_review_text %>% 
  count(sentiment, sort = T) # majority of sentiment are null, but the top three words to descibe reviews are "positive", "trust", "joy"

## visualize the sentiment data

hotel_review_text %>% 
  group_by(review_name) %>% # group by hotel review text file
  left_join(get_sentiments("nrc")) %>% # bring in sentiment data
  filter(!is.na(sentiment)) %>% # filter out null sentiment values
  count(sentiment) %>% # count sentiment type by hotel_review_text_file
  ungroup() %>% # put back into dataframe form
  spread(sentiment, n) # create columns for each type of sentiment by hotel review text

# call sentiment metric the total number of positive sentiments minus total negative metrics
# positive metrics: "anticipation", joy", "positive", "trust"
# negative metrics: "anger", "disgust", "fear", "negative", "sadness"

hotel_review_text %>% 
  group_by(review_name) %>% # group by hotel review text file
  left_join(get_sentiments("nrc")) %>% # bring in sentiment data
  filter(!is.na(sentiment)) %>% # filter out null sentiment values
  count(sentiment) %>% # count sentiment type by hotel_review_text_file
  ungroup() %>% # put back into dataframe form
  spread(sentiment, n) %>%  # create columns for each type of sentiment by hotel review text 
  rowwise() %>% # for aggregations across rows
  mutate(sentiment_metric = (sum(anticipation, joy, positive, trust) - sum(anger, disgust, fear, negative, sadness)),
         review_name = factor(review_name, levels = review_name)) %>% # creates sentiment metric for each review text file
  ggplot(aes(reorder(review_name, sentiment_metric), sentiment_metric, fill = review_name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = 0) # creates horizontal stacked bar chart ranking review text data on sentiment from highest to lowest
  
hotel_review_text %>% 
  group_by(review_name) %>% # group by hotel review text file
  left_join(get_sentiments("nrc")) %>% # bring in sentiment data
  filter(!is.na(sentiment)) %>% # filter out null sentiment values
  count(sentiment) %>% # count sentiment type by hotel_review_text_file
  ungroup() %>% # put back into dataframe form
  spread(sentiment, n) %>%  # create columns for each type of sentiment by hotel review text 
  rowwise() %>% # for aggregations across rows
  mutate(sentiment_metric = (sum(anticipation, joy, positive, trust) - sum(anger, disgust, fear, negative, sadness)),
         review_name = factor(review_name, levels = review_name)) %>% # creates sentiment metric for each review text file
  ggplot(aes(reorder(review_name, sentiment_metric), sentiment_metric)) + # remove fill to maybe remove confusion/less distracting
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = 0)

# try to group text by hotel location and compare

install.packages("qdapRegex") # regex package in R
library(qdapRegex)

hotel_review_text %>% 
  mutate(hotel_name = ex_between(review_name, "_", ".", extract = T)) %>% 
  group_by(hotel_name) %>% 
  left_join(get_sentiments("nrc")) %>% # bring in sentiment data
  filter(!is.na(sentiment)) %>% # filter out null sentiment values
  count(sentiment) %>% # count sentiment type by hotel_review_text_file
  ungroup() %>% # put back into dataframe form
  spread(sentiment, n) %>%  # create columns for each type of sentiment by hotel review text 
  rowwise() %>% # for aggregations across rows
  mutate(sentiment_metric = (sum(anticipation, joy, positive, trust) - sum(anger, disgust, fear, negative, sadness)),
         hotel_name = factor(hotel_name, levels = hotel_name)) %>% # creates sentiment metric for each review text file
  ggplot(aes(reorder(hotel_name, sentiment_metric), sentiment_metric, fill = hotel_name)) + # remove fill to maybe remove confusion/less distracting
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = 0) # groups reviews by hotel location, though Swissotel Chicago needs another layer of grouping
  
hotel_review_text %>% 
  mutate(hotel_name = ex_between(review_name, "_", ".", extract = T)) %>% 
  mutate(hotel_name = case_when(
    hotel_name == "bestwestern_hotel_sfo" ~ "Best Western — San Francisco",
    hotel_name == "holiday_inn_london" ~ "Holiday Inn — London",
    hotel_name %in% c("swissotel_chicago", "swissotel_hotel_chicago") ~ "Swissotel — Chicago")) %>% 
  group_by(hotel_name) %>% 
  left_join(get_sentiments("nrc")) %>% # bring in sentiment data
  filter(!is.na(sentiment)) %>% # filter out null sentiment values
  count(sentiment) %>% # count sentiment type by hotel_review_text_file
  ungroup() %>% # put back into dataframe form
  spread(sentiment, n) %>%  # create columns for each type of sentiment by hotel review text 
  rowwise() %>% # for aggregations across rows
  mutate(sentiment_metric = (sum(anticipation, joy, positive, trust) - sum(anger, disgust, fear, negative, sadness)),
         hotel_name = factor(hotel_name, levels = hotel_name)) %>% # creates sentiment metric for each review text file
  ggplot(aes(reorder(hotel_name, sentiment_metric), sentiment_metric, fill = hotel_name)) + # remove fill to maybe remove confusion/less distracting
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = 0) # groups reviews by hotel location, though Swissotel Chicago needs another layer of grouping



