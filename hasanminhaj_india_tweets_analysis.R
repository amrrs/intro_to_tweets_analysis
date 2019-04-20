library(tidyverse)
library(rtweet)
library(lattice)
library(udpipe)
library(magick)
library(cowplot)
library(ggimage)
library(ggplot2)
library(grid)
library(ggthemes)



hasanIN <- read_twitter_csv("hasanminhaj_india_noRT.csv", unflatten = TRUE) 

#A glimpse of the data
glimpse(hasanIN)

# Top Twitter Accounts
hasanIN %>% 
  count(screen_name) %>% 
  arrange(desc(n)) %>% 
  slice(1:10) %>% 
  knitr::kable()


# Tweet Client Source
hasanIN %>% 
  count(source) %>% 
  arrange(desc(n)) 

# Top 20 Hashtags
hasanIN %>% 
  unnest(hashtags) %>% 
  count(hashtags = tolower(hashtags)) %>% 
  arrange(desc(n)) %>% 
  mutate(hashtags = fct_reorder(hashtags,-n, .desc = TRUE)) %>% 
  drop_na() %>% 
  slice(1:20) %>% 
  ggplot() + geom_bar(aes(hashtags,n), stat = "identity", fill = "#000080") +
  coord_flip() +
  ggplot2::theme_minimal()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = c('bold'),
                                   size = 14,
                                   color = "#000080")) +
  labs(title = "Top 20 Hashtags about Patriot Act's Indian Election Episode",
       subtitle = "Comdey Show by Hasan Mihnaj & Netflix",
       caption = "Data Source: Tweets mentioning `@hasanminhaj india`",
       y = "Count of Tweets",
       x = "Hashtags") -> top20_plot



#img <- "https://st1.latestly.com/wp-content/uploads/2019/03/03-8-784x441.jpg"
#img_new <- image_read(img)


# based on this SO answer: https://stackoverflow.com/a/39632532
# Indian Tricolor Gradient Background
# Src: https://www.schemecolor.com/indian-flag-colors.php

indflag <- c("#FF9933", "#FFFFFF", "#138808")
g <- rasterGrob(indflag, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
grid.newpage()
grid.draw(g)
print(top20_plot, newpage = FALSE)

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


stats %>% 
  filter(freq >= 5) %>% 
  arrange(desc(rake)) %>% 
  slice(1:30) %>% 
  mutate(keyword = fct_reorder(keyword,rake)) %>% 
  ggplot() + geom_bar(aes(keyword,rake), stat = "identity", fill = "red") +
  scale_y_log10() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 30 Topics about Patriot Act's Indian Election Tweets",
       subtitle = "Comdey Show by Hasan Mihnaj & Netflix",
       caption = "Data Source: Tweets mentioning `@hasanminhaj india`",
       y = "Log of RAKE Score (higher - better)",
       x = "Hashtags") -> topics

ggdraw() +
  draw_image("https://st1.latestly.com/wp-content/uploads/2019/03/03-8-784x441.jpg",
             x = 0.25, y = -0.25,
             scale = 0.4) +
  draw_plot(topics)


## Word cloud - required????
library(wordcloud)
stats %>% 
  filter(freq >= 5) %>% 
  arrange(desc(rake)) %>% 
  slice(1:30) %>% 
  mutate(keyword = fct_reorder(keyword,rake)) -> stats_df
  wordcloud::wordcloud(stats_df$keyword,stats_df$rake)
  
## Text Plot
  
wordcloud::textplot(log1p(stats_df$rake),log1p(stats_df$freq),stats_df$keyword,cex = 0.7)
