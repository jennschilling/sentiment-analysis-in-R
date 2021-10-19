#### Setup ####

library(here) # working directory
library(tidyverse) # data processing and plotting
library(tidytext) # text analysis
library(scales) # number formatting

# Read data
text_data <- read_csv("https://raw.githubusercontent.com/bradweiner/nacac19/master/nacac19_final_data_for_git.csv")

# Original Tweet: https://twitter.com/brad_weiner/status/1177321556290211840
# Thread about keynote: https://twitter.com/byJoshMoody/status/1177278329096941569

# Filter to date of interest and exclude retweets
sep_26 <- text_data %>%
  filter(tweet_date == "2019-09-26") %>%
  filter(!is_retweet)

# Make data long
sep_26_long <- sep_26 %>%
  select(status_id, hour, anger:positive) %>%
  pivot_longer(anger:positive,
               names_to = "sentiment",
               values_to = "value") 


# Aggregate sentiment
sep_26_long_agg <- sep_26_long %>%
  group_by(hour, sentiment) %>%
  summarise(avg_sentiment = mean(value),
            .groups = "drop")


# Plot
ggplot(data = sep_26_long_agg %>%
              filter(hour <= 16 &
                       sentiment != "negative" & 
                       sentiment != "positive"),
       mapping = aes(x = hour,
                     y = avg_sentiment,
                     group = sentiment)) +
  geom_vline(xintercept = 5) +
  geom_line() +
  facet_wrap(~ sentiment,
             scales = "free_x") +
  scale_y_continuous(labels = number_format(1)) +
  labs(title = "Average sentiment score of #nacac19 by hour on September 26, 2019",
       x = "Tweet Hour",
       y = "Average Sentiment",
       caption = "Tweet data pulled by Brad Weiner using {rtweet} and #nacac19 and analyzed by Brad Weiner. Retweets are excluded.") +
  theme_classic()

ggplot(data = sep_26_long_agg %>%
         filter(hour <= 16 &
                  (sentiment == "negative" | 
                  sentiment == "positive")),
       mapping = aes(x = hour,
                     y = avg_sentiment,
                     group = sentiment)) +
  geom_vline(xintercept = 5) +
  geom_line() +
  facet_wrap(~ sentiment,
             scales = "free_x",
             ncol = 1) +
  scale_y_continuous(labels = number_format(1)) +
  labs(title = "Average sentiment score of #nacac19 by hour on September 26, 2019",
       x = "Tweet Hour",
       y = "Average Sentiment",
       caption = "Tweet data pulled by Brad Weiner using {rtweet} and #nacac19 and analyzed by Brad Weiner. Retweets are excluded.") +
  theme_classic()
