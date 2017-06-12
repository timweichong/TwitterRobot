library(twitteR)
library(tidytext)
library(dplyr)
library(lubridate)
library(magrittr)

consumer_key = "M6JyP8IbOA5TTWSnRo1Rtbw9e"
consumer_secret = "	VA6avx007gkzpxlFeCbAwK6TwVdQoSaLgSunIBBQOx3iDaTxAq"
access_token = "874294485559242757-jVkFEtGjfsJ7GkdCSuNpzJpEV6bo9M5"
access_secret = "WgfUmNxmN9zMuYh4OdtzdTe7wtTF3X9blFAaoXp7jsgnK"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets = searchTwitter("trump", n = 1000, lang = "en")
twitter.df = do.call(rbind, lapply(tweets, function(x) x$toDataFrame()))
twitter.text = sapply(tweets, function(x) x$getText())
twitter.text= gsub("[^[:print:]]", "", twitter.text)
twitter.text = gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", twitter.text)
twitter.text = gsub("[^[:space:]]*…$", "", twitter.text)
twitter.df$text = twitter.text
twitter.df = select(twitter.df, text) %>% mutate(tweet = 1:nrow(.))
twitter.tiny = twitter.df %>% unnest_tokens(word,text) %>% anti_join(stop_words) %>% 
  inner_join(get_sentiments("afinn")) %>% group_by(tweet) %>% mutate(sentsum = sum(score)) %>% 
  select(tweet, sentsum) %>% mutate(sentiment = if_else(sentsum>0, 'positive', 'negative'))

npositive = nrow(unique(twitter.tiny[twitter.tiny$sentiment =='positive',]))
nnegative = nrow(unique(twitter.tiny[twitter.tiny$sentiment =='negative',]))
nneutral = 1000 - npositive - nnegative


tweet(sprintf("1000 tweets about Trump today: %d positive, %d negative, & %d neutral.", npositive, nnegative, nneutral))
