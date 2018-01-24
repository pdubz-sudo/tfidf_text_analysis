library(gutenbergr)
library(tidyverse)
library(tidytext)
library(wordcloud)

children_novels <- gutenberg_download(c(11, 35997, 46, 34339, 33673), 
                                      meta_fields = "title") %>% 
  select(text, title)


tidy_novels <- children_novels %>%
  unnest_tokens(word, text)


word_count <- count(tidy_novels, title, word, sort = TRUE)


weighted_words <- word_count %>%
  bind_tf_idf(word, title, n) %>%
  arrange(desc(tf_idf))

plot_prep <- weighted_words %>%
  mutate(word = factor(word, levels = unique(word))) %>%
  mutate(title = factor(title, levels = c("Alice's Adventures in Wonderland", 
                                          "The Jungle Book",
                                          "A Christmas Carol in Prose; Being a Ghost Story of Christmas", 
                                          "The Princess and the Goblin", 
                                          "The King of the Golden River; or, the Black Brothers: A Legend of Stiria.")))


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



remove_words <- data_frame(word = "she")  # you would make a vector with c("remove_word1", "remove_word2")
tidy_novels_removed_word <- anti_join(tidy_novels, remove_words, by = "word")



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


weighted_words %>%
  with(wordcloud(word, n, colors="dark green",max.words = 150))
