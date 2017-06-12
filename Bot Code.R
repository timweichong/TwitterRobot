library(twitteR)
library(tidytext)
library(dplyr)
library(lubridate)

consumer_key = "kmzqLVmswwLbgLE0liXCCdhmi"
consumer_secret = "lrxTzTiY1s0Twuzy3u3CIIruIsbX2jDP25vzXnxvWIbYLpJGkD"
access_token = "389902757-l6jrNY4hhZRsBRYFjYMPbGAHnHrd4RSy58tdxzwN"
access_secret = "foo4jcyrLrwYfEocHdMjYJlVsrP9RgxSueprZSreP9pUB"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets = searchTwitter("trump", n = 1000, lang = "en", since = today())
twitter.df = do.call(rbind, lapply(tweets, function(x) x$toDataFrame()))
twitter.text = sapply(tweets, function(x) x$getText())
twitter.text= gsub("[^[:print:]]", "", twitter.text)
twitter.text = gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", twitter.text)
twitter.text = gsub("[^[:space:]]*…$", "", twitter.text)
twitter.df$text = twitter.text
twitter.df = select(twitter.df, text) %>% mutate(tweet = 1:nrow(.))
twitter.tiny = twitter.df %>% unnest_tokens(word,text) %>% anti_join(stop_words) %>% 
  inner_join(get_sentiments("afinn")) %>% group_by(tweet) %>% mutate(sentsum = sum(score)) %>% 
  select(tweet, sentsum) %>% mutate(sentiment = if_else(sentsum>0, 'positive', 'negative', missing = 'neutral'))

npositive = nrow(twitter.tiny[twitter.tiny$sentiment =='positive',])
nnegative = nrow(twitter.tiny[twitter.tiny$sentiment =='negative',])
nneutral = 1000 - npositive - nnegative


cat(sprintf("Of latest 1000 tweets about Trump today: %d are positive, %d are negative, and %d are neutral.", npositive, nnegative, nneutral))
