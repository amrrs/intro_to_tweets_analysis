library(rtweet)
#library(tidyverse)

consumer_key ="xxxx"

consumer_secret ="xxxx"

access_token="xxxx"

access_secret="xxxx"


twitter_token = create_token(consumer_key = consumer_key,
                             consumer_secret = consumer_secret,
                             access_token = access_token,
                             access_secret = access_secret)

keyword1 <- search_tweets('@hasanminhaj india', n = 5000, token = twitter_token, include_rts = FALSE)

write_as_csv(keyword1,"~//Documents//R Codes//hasanminhaj_india_noRT.csv")