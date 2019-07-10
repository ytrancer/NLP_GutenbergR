library(gutenbergr)
gutenberg_works()
gutenberg_works(title=='The Wonderful Wizard of Oz')
wizard_of_oz <- gutenberg_download(55)


# Tokenising data ---------------------------------------------------------

library(tidytext)

tidy_wizard <- unnest_tokens(wizard_of_oz, word, text)


library(dplyr)

wizzard_of_oz %>% wizzard_of_oz %>% unnest_tokens(word, text)

# Word counts -------------------------------------------------------------

tidy_wizard <- tidy_wizard %>% count(word, sort=TRUE)

tidy_wizard %>% count(word)
tidy_wizard %>% filter(word %>% as.numeric() %>% is.na()) %>%
  count(word)


# Exersice page 1-4 -------------------------------------------------------

library(stringr)
gutenberg_works(str_detect(title, 'Treasure'))  

tidy_wizard %>% 
  filter(word %>% !stringr::str_detect(pattern='\\d', word)) # omit numeric, check out regex

wizard_counts <- tidy_wizard %>% filter(word %>% as.numeric() %>% is.na()) %>%
    count(word)



# stopwords removal -------------------------------------------------------

stop_words

wizard_no_sw <- wizard_counts %>% anti_join(stop_words)

main_chars <- data.frame(word=c('dorothy', 'scarecrow', 'woodman'))
wizard_no_sw <- wizard_no_sw %>% anti_join(main_chars)



# exersice page 1-5 -------------------------------------------------------


treasure_no_sw %>% count(word)
treasure_counts%>% anti_join(stop_words)


# Sentiment analysis ------------------------------------------------------

sentiments <- get_sentiments(lexicon = 'bing') # 'nrc'
  
wizard_sentiment <- wizard_no_sw %>% inner_join(sentiments)
wizard_sentiment


# 1-6 ---------------------------------------------------------------------

treasure_sentiments <- treasure_no_sw %>% inner_join(sentiments)

treasure_sentiments %>% group_by(sentiment) %>% count()
# or
table(treasure_sentiments$sentiment)


# Ngrams ------------------------------------------------------------------

wizard_of_oz%>% unnest_tokens(word, text, token = 'ngrams', n = 2) %>% 
  count(word, sort = TRUE)

# separate() can be used to split the 'word' column into columns
# then we can remove any rows which contain stopwords

# 1-7 ---------------------------------------------------------------------

treasure %>% unnest_tokens(word, text, token = 'ngrams', n=3)
count(word, sort=TRUE)


# visualising data --------------------------------------------------------

library(wordcloud)

wordcloud(wizard_no_sw$word, wizard_no_sw$n, max.words = 50)

wizard_of_oz%>% unnest_tokens(word, text, token = 'ngrams', n = 2) %>% 
  
  count(word, sort = TRUE) %>%
  separate(word, into =c('first_word', 'second_word', sep=' ') )%>% 
             anti_join(stop_words, by=c(first_word='word' ) ) %>%
            anti_join(stop_words, by=c(second_word='word' ) ) %>%
                        #joind columns again
              mutate(ngram = paste(first_word, second_word))
wizard_bigrams

wordcloud(wizard_bigrams$ngram, wizard_bigrams$n, max.words = 50)

#Comparison wordclouds

# could compare two documents

wizard_sentiment <- wizard_no_sw %>% inner_join(get_sentiments(lexicon = 'bing'))

compcloud_data <- wizard_sentiment %>% spread(sentiment, n, fill = 0) 
%>%
data.frame()



rownames(compcloud_data) <- compcloud_data$word

comparison.cloud(compcloud_data %>% select(word),
                 colors = c('darkred', 'darkgreen'),
                 max.words = 50)


# 1-9 ---------------------------------------------------------------------

negative_treasure <- treasure_sentiments %>% filter(sentiment == 'negative')
positive_treasure <- treasure_sentiments %>% filter(sentiment == 'positive')


wordcloud(negative_treasure$word, negative_treasure$n, max.words = 50)
wordcloud(positive_treasure$word, positive_treasure$n, max.words = 50)


