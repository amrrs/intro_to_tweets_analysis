library(tidyverse)
library(rtweet)
library(lattice)
library(udpipe)

hasanIN <- read_twitter_csv("hasanminhaj_india_noRT.csv", unflatten = TRUE) 

glimpse(hasanIN)

# Top Twitter Accounts
hasanIN %>% count(screen_name) %>% arrange(desc(n)) %>% slice(1:10) %>% 
  knitr::kable()

# Top 20 Hashtags
hasanIN %>% 
  unnest(hashtags) %>% 
  count(hashtags = tolower(hashtags)) %>% 
  arrange(desc(n)) %>% 
  mutate(hashtags = fct_reorder(hashtags,-n, .desc = TRUE)) %>% 
  drop_na() %>% 
  slice(1:20) %>% 
  ggplot() + geom_bar(aes(hashtags,n), stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 20 Hashtags about Patriot Act's Indian Election Episode",
       subtitle = "Comdey Show by Hasan Mihnaj & Netflix",
       caption = "Data Source: Tweets mentioning `@hasanminhaj india`",
       y = "Count of Tweets",
       x = "Hashtags")



# Cleaning

#based on: https://stackoverflow.com/questions/51947268/remove-hashtags-from-beginning-and-end-of-tweets-in-r

hasanIN$text_nohashtag <- stringi::stri_replace_all_regex(hasanIN$text,"(?:\\s*#\\w+)+\\s*","")



#model <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = 'english-ewt-ud-2.3-181115.udpipe')

s <- udpipe_annotate(udmodel_english, hasanIN$text_nohashtag)

x <- data.frame(s)



stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "yellow", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

## NOUNS
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$lemma)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$lemma)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple", 
         main = "Most occurring adjectives", xlab = "Freq")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$lemma)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "gold", 
         main = "Most occurring Verbs", xlab = "Freq")

## Using RAKE
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")


