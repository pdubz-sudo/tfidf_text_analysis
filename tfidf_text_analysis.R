# All the information and details about this code are contained in the R Notebook.
# This script is just the code without the details. Refer to the R notebook called
# tfidf_notebook.Rmd for the summary and reasoning behind this code.

#install all the necessary packages and libraries
# install.packages("gutenbergr")
# install.packages("tidyverse")
# install.packages("tidytext")
# install.packages("wordcloud")
library(gutenbergr)
library(tidyverse)
library(tidytext)
library(wordcloud)

# The words we would like to investigate are first extracted from Project 
# Gutengurg which is a free e-books project. Any of their e-books can be used to 
# extract data by going to their website, choosing a book, taking the id 
# number in the url of that book, and placing it in the gutenberg argument.
# https://www.gutenberg.org/
# Books and Authors downloaded in this script:
# Alice's Adventures in Wonderland by Lewis Carroll
# The Jungle Book by Rudyard Kipling
# A Christmas Carol in Prose; Being a Ghost Story of Christmas by Charles Dickens
# The Princess and the Goblin by George MacDonald
# The King of the Golden River; or, the Black Brothers: A Legend of Stiria. by John Ruskin
children_novels <- gutenberg_download(c(11, 35997, 46, 34339, 33673), 
                                      meta_fields = "title") %>% 
  select(text, title)


# Manipulate this data so each word is its own row. We will now turn it into a 
# tidy text dataset
tidy_novels <- children_novels %>%
  unnest_tokens(word, text)


# group them by their book title.
word_count <- count(tidy_novels, title, word, sort = TRUE)

# word cloud of non-significant words
word_count %>%
  with(wordcloud(word, n, colors="dark green",max.words = 150))

# running tf_idf statistics to create weights for words and find important words
# for each children's book in the collection of children's books
weighted_words <- word_count %>%
  bind_tf_idf(word, title, n) %>%
  arrange(desc(tf_idf))

# plot prep: factor words and titles to keep order for the graphs
plot_prep <- weighted_words %>%
  mutate(word = factor(word, levels = unique(word))) %>%
  mutate(title = factor(title, levels = c("Alice's Adventures in Wonderland", 
                                          "The Jungle Book",
                                          "A Christmas Carol in Prose; Being a Ghost Story of Christmas", 
                                          "The Princess and the Goblin", 
                                          "The King of the Golden River; or, the Black Brothers: A Legend of Stiria.")))

# make plot of top 10 td_idf words for each book
plot_prep %>%
  group_by(title) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = title)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~title, ncol =2, scales = "free") +
  coord_flip()

# remove any other words in top 10 that do not have too much significance
remove_words <- data_frame(word = "she")  # you would make a vector with c("remove_word1", "remove_word2")
tidy_novels_removed_word <- anti_join(tidy_novels, remove_words, by = "word")

# re-plot after cleaning
tidy_novels_removed_word %>%
  count(title, word, sort = TRUE) %>%
  bind_tf_idf(word, title, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = unique(word))) %>%
  mutate(title = factor(title, levels = c("Alice's Adventures in Wonderland", "The Jungle Book","A Christmas Carol in Prose; Being a Ghost Story of Christmas", "The Princess and the Goblin", "The King of the Golden River; or, the Black Brothers: A Legend of Stiria.")))%>%
  group_by(title) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = title)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~title, ncol =2, scales = "free") +
  coord_flip()
