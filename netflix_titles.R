library(tidyverse)
library(tidymodels)
library(tidytext)
library(skimr)
library(lubridate)

netflix_titles = read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv")

netflix_titles %>%
  filter(type == 'Movie') %>%
  mutate(country = ifelse(str_detect(country, pattern = 'Korea'), "Korea", "Others")) %>%
  filter(!is.na(country)) %>%
  select(show_id, country, release_year, rating, duration) %>%
  group_by(release_year, country) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  ggplot(aes(as.factor(release_year), cnt, fill = country)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  scale_y_reordered() +
  facet_wrap(~country, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# it seems South Korean movie making trend is also following that of other countries
# as there's some landing bars from 2019 to the present year.

netflix_titles %>%
  filter(type == 'Movie') %>%
  mutate(country = ifelse(str_detect(country, pattern = 'Korea'), "Korea", "Others")) %>%
  filter(!is.na(country)) %>%
  select(show_id, country, release_year, rating, duration) %>%
  mutate(num_duration = parse_number(duration)) %>%
  group_by(country) %>%
  summarise(meanDuration = mean(num_duration),
            sdDuration = sd(num_duration)) %>%
  ggplot(aes(country, meanDuration)) +
  geom_bar(stat="identity", color="black", width = 0.5,
           position=position_dodge()) +
  geom_errorbar(aes(ymin = meanDuration-sdDuration, ymax = meanDuration+sdDuration), width=.2,
                position=position_dodge(.9)) 

# South Korean movies are not that different from other countries' movie running time
# though it is like 10 min longer than its counterpart.

netflix_titles %>%
  mutate(Countryclf =  ifelse(str_detect(country, pattern = 'Korea'), 1, 0)) %>%
  filter(!is.na(Countryclf)) %>%
  unnest_tokens(word, description) %>%
  count(Countryclf, word) %>%
  anti_join(get_stopwords()) %>%
  group_by(Countryclf) %>%
  arrange(desc(n)) %>%
  pivot_wider(names_from = 'Countryclf', values_from = 'n')

netflix_titles %>%
  unnest_tokens(ngram, description, token = "ngrams", n = 2) %>%
  count(type, ngram, sort = TRUE) %>%
  separate(ngram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite(ngram, c(word1, word2), sep = " ") %>%
  group_by(type) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(ngram = reorder_within(ngram, n, type)) %>%
  ggplot(aes(n, ngram, fill = type)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  scale_y_reordered()



netflix_titles %>%
  filter(type == 'Movie', country == 'United States', !is.na(cast)) %>%
  mutate(date_added = mdy(date_added)) %>%
  select(cast) %>%
  mutate(cast = strsplit(gsub("[][\"]", "", cast), ",")) %>%
  unnest(cast) %>%
  as.data.frame() %>%
  mutate(cast = str_trim(cast)) %>%
  group_by(cast) %>% 
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  as.data.frame() %>%
  top_n(n = 10) %>%
  ggplot(aes(x = as.factor(cast), y = n)) +
  geom_col() +
  coord_flip()

netflix_titles %>%
  mutate(row = row_number()) %>%
  select(type, description, row) %>%
  pivot_wider(names_from = 'type', values_from =  'description') %>%
  select(-row) # 미완성 row
