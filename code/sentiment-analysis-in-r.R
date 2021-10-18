
# Sentiment Analysis in R
# Jenn Schilling
# 10/20/2021 - 10/22/2021

# Introduction

# This file contains the code for webinar: 
# [Sentiment Analysis in R](https://www.airweb.org/collaborate-learn/calendar/2021/10/20/event/sentiment-analysis-in-r)
# Presented for the Association for Institutional Research, October 20 & 22 2021.  

# Webinar Details 
# This webinar will teach participants how to complete a text analysis in R, including data processing and cleaning,
# visualization of word frequencies, and sentiment analysis. Sentiment analysis is useful for finding patterns in 
# text data from open response questions on surveys and course evaluations as well as evaluating social media posts.
# This series is ideal for higher education professionals who have some experience in R and want to add text analysis 
# to their R skills.

# As a result of this webinar, participants will be able to: 
# - Prepare data for a text analysis in R. 
# - Conduct text mining in R.
# - Complete sentiment analysis in R. 

# Materials developed by Jenn Schilling. 

#### Setup ####

library(here) # working directory
library(tidyverse) # data processing and plotting
library(tidytext) # text analysis
library(wordcloud) # word cloud 
library(scales) # number formatting

# Read data
text_data <- read_csv(here("data", "uarizona_tweets.csv"), show_col_types = FALSE) %>%
  filter(created_at <= "2021-09-29")


#### Data Processing ####


# First let's look at the data
View(text_data)

# Check the number of rows and columns
dim(text_data)

# View the data types of the columns
glimpse(text_data)




# Process Data
text_data_processed <- text_data %>%
  
# Lowercase
  mutate(text = str_to_lower(text)) %>%
  
# Expand contractions
  mutate(text = gsub("n't|n’t", " not", text),
         text = gsub("'ll|’ll", " will", text),
         text = gsub("'re|’re", " are", text),
         text = gsub("'ve|’ve", " have", text),
         text = gsub("'m|’m", " am", text),
         text = gsub("'d|’d", " would", text),
         text = gsub("it's|it’s", "it is", text), 
         text = gsub("'s|’s", "", text)) %>%
  
# Remove emojis
  mutate(text = gsub("\U0001", "", text)) %>%
  
# Remove links
  mutate(text = gsub("(https:|http:).*", "", text)) %>%
  
# Remove special characters
  mutate(text =  gsub("[^a-zA-Z0-9 ]", " ", text)) %>%
  
# Remove ampersand notation
  mutate(text = gsub("amp", "", text)) %>%
  
# Remove extra whitespace
  mutate(text = str_squish(text)) %>%
  
# Make identification column a character
  mutate(status_id = as.character(status_id))


#### Check Data ####

# View the columns and a few records
glimpse(text_data_processed)

# Plot number of tweets over time
tweets_time <- text_data_processed %>%
  mutate(created_at_date = as.Date(created_at, "%m/%d/%Y", tz = "UTC")) %>%
  group_by(created_at_date) %>%
  summarise(n = n(),
            .groups = "drop")

ggplot(data = tweets_time,
       mapping = aes(x = created_at_date,
                     y = n,
                     group = 1)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(tweets_time$created_at_date),
                                  max(tweets_time$created_at_date),
                                  "weeks")) +
  labs(x = "Date",
       y = "Number of Tweets") +
  theme_classic()


# Plot number of screen names over time
tweets_user <- text_data_processed %>%
  mutate(created_at_date = as.Date(created_at, "%m/%d/%Y", tz = "UTC")) %>%
  select(created_at_date, screen_name) %>%
  unique(.) %>%
  group_by(created_at_date) %>%
  summarise(n = n(),
            .groups = "drop")

ggplot(data = tweets_user,
       mapping = aes(x = created_at_date,
                     y = n,
                     group = 1)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(tweets_user$created_at_date),
                                  max(tweets_user$created_at_date),
                                  "weeks")) +
  labs(x = "Date",
       y = "Number of Users") +
  theme_classic()

# Plot histograms of favorite count and retweet count
ggplot(data = text_data_processed,
       mapping = aes(x = favorite_count)) +
  geom_histogram(bins = 10) +
  labs(x = "Favorite Count",
       y = "Count") +
  theme_classic()

ggplot(data = text_data_processed,
       mapping = aes(x = retweet_count)) +
  geom_histogram(bins = 10) +
  labs(x = "Retweet Count",
       y = "Count") +
  theme_classic()



#### Tokenizing ####


# Tokenize the text data to get each individual word
text_tokens <- text_data_processed %>%
  unnest_tokens(word, text)

# Let's see what the new table looks like
View(text_tokens)

dim(text_tokens)

# Let's take a look at the stop words list
View(stop_words)

table(stop_words$lexicon)

# Now remove the stop words
text_tokens <- anti_join(text_tokens, stop_words, by = "word")

# Look at the tokenized data frame again
View(text_tokens)

dim(text_tokens) # notice the drop in the row count now that stop words are gone



#### Word Frequencies ####

# Count number of times a word appears in each tweet
tweet_words <- text_tokens %>%
  count(status_id, word, sort = TRUE)

View(tweet_words)

# Count the number of words in each tweet
total_words <- tweet_words %>%
  group_by(status_id) %>%
  summarise(total_words = sum(n))

View(total_words)

# Put the counts together
total_tweet_words <- left_join(tweet_words, total_words, by = "status_id")

View(total_tweet_words)

# Count word frequencies overall
tweet_word_freq <- text_tokens %>%
  count(word, sort = TRUE)

View(tweet_word_freq)

# View top words
ggplot(data = tweet_word_freq %>% 
         top_n(15),
       mapping = aes(y = reorder(word, n),
                     x = n)) +
  geom_col() +
  labs(title = "Most Frequently Used Words",
       x = "Word Count",
       y = "") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic()

# Create word cloud
tweet_word_freq %>% 
  with(wordcloud(word, n, max.words = 100))


# Word frequency by day
day_words <- text_tokens %>%
  mutate(created_at_date = as.Date(created_at, "%m/%d/%Y", tz = "UTC")) %>%
  count(created_at_date, word, sort = TRUE) 

View(day_words)

# Plot word frequencies by day
ggplot(data = day_words %>%
         group_by(created_at_date) %>%
         top_n(5) %>%
         ungroup %>%
         mutate(created_at_date = as.factor(created_at_date),
                word = reorder_within(word, n, created_at_date)),
       mapping = aes(y = word,
                     x = n)) +
  geom_col() +
  facet_wrap(~created_at_date,
             scales = "free") +
  labs(title = "Most Frequently Used Words Each Day",
       x = "Word Count",
       y = "") +
  scale_y_reordered() +
  scale_x_continuous(limits = c(0, 45),
                     expand = c(0, 0)) +
  theme_classic()


#### Term Frequency - Inverse Document Frequency ####


View(total_tweet_words)

# Compute frequency of word by tweet
total_tweet_words <- total_tweet_words %>%
  mutate(freq = n / total_words)

View(total_tweet_words)

# Because there are so many individual tweets, we will first look at total words
# each day and the frequency of each word 
total_day_words <- day_words %>%
  group_by(created_at_date) %>%
  summarise(total_words = sum(n),
            .groups = "drop")

total_day_words <- left_join(day_words, total_day_words, by = "created_at_date") %>%
  mutate(freq = n / total_words) 

View(total_day_words)

# Create a plot of the total words in each day
ggplot(data = total_day_words,
       mapping = aes(x = freq)) +
  geom_histogram(bins = 30) +
  facet_wrap(~created_at_date,
              scales = "free") +
  scale_x_continuous(limits = c(0, 0.09)) +
  labs(x = "Word Frequency",
       y = "Count") +
  theme_classic()

# Compute tf, idf, and tf-idf
day_tf_idf <- day_words %>%
  bind_tf_idf(word, created_at_date, n)

View(day_tf_idf) 
# tf = term frequency
# idf = inverse document frequency
# tf_idf = term frequency * inverse document frequency 

# View words with high tf-idf
day_tf_idf %>%
  arrange(-tf_idf)

# Plot words with high tf-idf by day
ggplot(data = day_tf_idf %>%
         group_by(created_at_date) %>%
         top_n(5, tf_idf) %>%
         ungroup %>%
         mutate(created_at_date = as.factor(created_at_date),
                word = reorder_within(word, tf_idf, created_at_date)),
       mapping = aes(y = word,
                     x = tf_idf)) +
  geom_col() +
  facet_wrap(~created_at_date,
             scales = "free") +
  labs(title = "Important Words Each Day",
       x = "TF-IDF",
       y = "") +
  scale_y_reordered() +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic()



#### Sentiment Analysis ####

# View each lexicon
get_sentiments("afinn")

get_sentiments("bing")

get_sentiments("nrc") # note that words can have multiple sentiments in nrc


# Find common joy words in the tweets

nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

View(nrc_joy)

tweet_words_joy <- inner_join(tweet_word_freq,
                              nrc_joy,
                              by = "word")

View(tweet_words_joy)

# Find common disgust words in the tweets

nrc_disgust <- get_sentiments("nrc") %>%
  filter(sentiment == "disgust")

View(nrc_disgust)

tweet_words_disgust <- inner_join(tweet_word_freq,
                                  nrc_disgust,
                                  by = "word")

View(tweet_words_disgust)

# Plot joy and disgust together
tweet_words_joy_disgust <- bind_rows(tweet_words_joy, tweet_words_disgust) %>%
  group_by(sentiment) %>%
  top_n(5, n) %>%
  mutate(word = reorder_within(word, n, sentiment))

ggplot(data = tweet_words_joy_disgust,
       mapping = aes(x = n,
                     y = word)) +
  geom_col() +
  facet_wrap(~ sentiment,
             scales = "free") +
  labs(title = "Most Frequent Words with Disgust and Joy Sentiments",
       x = "Word Count",
       y = "") +
  scale_y_reordered() +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic()
  

# Looking and only positive and negative sentiment by word, we can evaluate the 
# sentiment of each tweet or of a day of tweets.

# First, let's get the sentiment of each word 
bing <- get_sentiments("bing")

sentiment_tweet_words <- inner_join(tweet_words,
                                    bing,
                                    by = "word")

View(sentiment_tweet_words)

# Next, we will count the number of positive and negative words per tweet
pos_neg_per_tweet <- sentiment_tweet_words %>%
  count(status_id, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n) %>%
  mutate(positive = ifelse(is.na(positive), 0 , positive),
         negative = ifelse(is.na(negative), 0, negative),
         sentiment = positive - negative)

View(pos_neg_per_tweet)

# Let's plot the sentiments by tweet
ggplot(data = pos_neg_per_tweet,
       mapping = aes(x = status_id,
                     y = sentiment)) +
  geom_col() +
  labs(title = "Sentiments by Tweet",
       x = "",
       y = "Sentiment") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# We can also look at each tweet over time
pos_neg_per_tweet_date <- text_data_processed %>%
  mutate(created_at_date = as.Date(created_at, "%m/%d/%Y", tz = "UTC")) %>%
  select(status_id, created_at_date) %>%
  right_join(pos_neg_per_tweet, by = "status_id")

# Now let's get the general sentiment each day
ggplot(data = pos_neg_per_tweet_date,
       mapping = aes(x = status_id,
                     y = sentiment)) +
  geom_col() +
  facet_wrap(~created_at_date,
             scales = "free") +
  labs(title = "Sentiments by Tweet",
       x = "",
       y = "Sentiment") +
  scale_y_continuous(limits = c(-3, 5),
                     expand = c(0, 0)) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

# Finally, let's find the general positive or negative sentiment for each day
sentiment_day_words <- inner_join(day_words,
                                  bing,
                                  by = "word")

View(sentiment_day_words)

pos_neg_per_day <- sentiment_day_words %>%
  count(created_at_date, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n) %>%
  mutate(positive = ifelse(is.na(positive), 0 , positive),
         negative = ifelse(is.na(negative), 0, negative),
         sentiment = positive - negative)

View(pos_neg_per_day)

# Plot sentiment by day
ggplot(data = pos_neg_per_day,
       mapping = aes(x = created_at_date,
                     y = sentiment)) +
  geom_col() +
  labs(title = "Sentiment by Day",
       x = "",
       y = "Sentiment") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme()


# A different way to compute sentiment per day
pos_neg_per_day <- sentiment_day_words %>%
  group_by(created_at_date, sentiment) %>%
  summarise(n = sum(n),
            .groups = "drop") %>%
  pivot_wider(names_from = sentiment,
              values_from = n) %>%
  mutate(positive = ifelse(is.na(positive), 0 , positive),
         negative = ifelse(is.na(negative), 0, negative),
         sentiment = positive - negative)

View(pos_neg_per_day)

# Plot sentiment by day
ggplot(data = pos_neg_per_day,
       mapping = aes(x = created_at_date,
                     y = sentiment)) +
  geom_col() +
  labs(title = "Sentiment by Day",
       x = "",
       y = "Sentiment") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme()

# Next let's look at the most common positive and negative words overall
sentiment_words <- sentiment_tweet_words %>%
  group_by(sentiment, word) %>%
  summarise(n = sum(n),
            .groups = "drop")

View(sentiment_words)

# Plot the top 10
sentiment_words_top <- sentiment_words %>%  
  group_by(sentiment) %>%
  top_n(10, n) %>% 
  ungroup() %>%
  mutate(word = reorder_within(word, n, sentiment))


ggplot(data = sentiment_words_top,
       mapping = aes(x = n,
                     y = word)) +
  geom_col() +
  facet_wrap(~ sentiment,
             scales = "free") +
  labs(title = "Most Frequent Negative and Positive Words",
       x = "Count",
       y = "") +
  scale_y_reordered() +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic()

# Let's compare the lexicons
afinn <- get_sentiments("afinn")

nrc <- get_sentiments("nrc")

table(bing$sentiment)

table(nrc$sentiment)

table(afinn$value)

tweet_words_afinn <- inner_join(afinn, tweet_words, by = "word") %>%
  group_by(status_id) %>%
  summarise(sentiment = sum(value),
            .groups = "drop") %>%
  mutate(method = "AFINN") %>%
  select(status_id, sentiment, method)

tweet_words_nrc <- inner_join(nrc %>% filter(sentiment %in% c("positive", "negative")),
                              tweet_words, by = "word") %>%
  count(status_id, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n) %>%
  mutate(positive = ifelse(is.na(positive), 0 , positive),
         negative = ifelse(is.na(negative), 0, negative),
         sentiment = positive - negative,
         method = "nrc") %>%
  select(status_id, sentiment, method)

tweet_words_bing <- inner_join(bing, tweet_words, by = "word") %>%
  count(status_id, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n) %>%
  mutate(positive = ifelse(is.na(positive), 0 , positive),
         negative = ifelse(is.na(negative), 0, negative),
         sentiment = positive - negative,
         method  = "bing") %>%
  select(status_id, sentiment, method)

tweet_words_all <- bind_rows(tweet_words_afinn,
                             tweet_words_nrc,
                             tweet_words_bing)

View(tweet_words_all)

ggplot(data = tweet_words_all,
       mapping = aes(x = status_id,
                     y = sentiment)) +
  geom_col() +
  labs(title = "Sentiments by Tweet",
       x = "",
       y = "Sentiment") +
  facet_wrap(~ method,
             scale = "free_y",
             ncol = 1) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Finally, let's look at the different feelings of each tweet
tweet_words_nrc_all <- inner_join(nrc %>% filter(sentiment != "positive" & sentiment != "negative"),
                                  tweet_words, by = "word") %>%
  count(status_id, sentiment)

View(tweet_words_nrc_all)

tweet_words_nrc_all_total <- tweet_words_nrc_all %>%
  count(sentiment) %>%
  mutate(sentiment = reorder(sentiment, n))

ggplot(data = tweet_words_nrc_all_total,
       mapping = aes(x = n,
                     y = sentiment)) +
  geom_col()  +
  labs(title = "Tweet Sentiments",
       x = "Number of Tweets",
       y = "Sentiment") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic() 

# Let's look at tweet sentiments by day
tweet_words_nrc_all_date <- text_data_processed %>%
  mutate(created_at_date = as.Date(created_at, "%m/%d/%Y", tz = "UTC")) %>%
  select(status_id, created_at_date) %>%
  right_join(tweet_words_nrc_all, by = "status_id") %>%
  count(created_at_date, sentiment) %>%
  mutate(sentiment = reorder_within(sentiment, n, created_at_date))

ggplot(data = tweet_words_nrc_all_date,
       mapping = aes(x = n,
                     y = sentiment)) +
  geom_col() +
  facet_wrap(~ created_at_date,
             scales = "free") +
  labs(title = "Sentiment by Day",
       x = "Number of Tweets",
       y = "") +
  scale_y_reordered() +
  scale_x_continuous(expand = c(0, 0),
                     labels = number_format(accuracy = 1)) +
  theme_classic()



#### Moving Beyond Words ####


# Get sets of two words
text_ngrams <- text_data_processed %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 2)

View(text_ngrams) # note that words are duplicated

dim(text_ngrams)

# Remove stop words
text_ngrams <- text_ngrams %>%
  separate(ngram, c("word_1", "word_2"), sep = " ") %>%
  anti_join(stop_words, by = c("word_1" = "word")) %>%
  anti_join(stop_words, by = c("word_2" = "word")) %>%
  unite(ngram, word_1, word_2, sep = " ")

View(text_ngrams) # note that words are duplicated

dim(text_ngrams)

# Get counts of n-grams
text_ngrams_freq <- text_ngrams %>%
  count(ngram, sort = TRUE)

View(text_ngrams_freq)

# Plot top n-grams
ggplot(data = text_ngrams_freq %>% 
         top_n(10),
       mapping = aes(y = reorder(ngram, n),
                     x = n)) +
  geom_col() +
  labs(title = "Most Frequently Used 2-Word Phrases",
       x = "Count",
       y = "") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic()

# Create word cloud
text_ngrams_freq %>% 
  with(wordcloud(ngram, n, max.words = 25, scale = c(1.5, .3)))


