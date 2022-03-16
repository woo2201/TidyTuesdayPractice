library(tidyverse)
library(patchwork)
library(ggrepel)
train = read_csv("/Users/jungwonwoo/Desktop/program_file/TexasAustin/train.csv")

train %>%
  count(priceRange)

price_plot = train %>%
  mutate(priceRange = parse_number(priceRange)) %>%
  ggplot(aes(longitude, latitude, z = priceRange)) +
  stat_summary_hex(alpha = 0.8, bins = 50) +
  scale_fill_viridis_c() +
  labs(
    fill = "mean",
    title = "Price"
  )

plot_austin <- function(var, title) {
  train %>%
    ggplot(aes(longitude, latitude, z = {{ var }})) +
    stat_summary_hex(alpha = 0.8, bins = 50) +
    scale_fill_viridis_c() +
    labs(
      fill = "mean",
      title = title
    )
}

(price_plot + plot_austin(avgSchoolRating, "School rating")) /
  (plot_austin(yearBuilt, "Year built") + plot_austin(log(lotSizeSqFt), "Lot size (log)"))

library(tidytext)

austin_tidy <-
  train %>%
  mutate(priceRange = parse_number(priceRange) + 100000) %>%
  unnest_tokens(word, description) %>%
  anti_join(get_stopwords())

austin_tidy %>%
  count(word, sort = TRUE)

top_words <-
  austin_tidy %>%
  count(word, sort = TRUE) %>%
  filter(!word %in% as.character(1:5)) %>%
  slice_max(n, n = 100) %>%
  pull(word)

word_freqs <-
  austin_tidy %>%
  count(word, priceRange) %>%
  complete(word, priceRange, fill = list(n = 0)) %>%
  group_by(priceRange) %>%
  mutate(
    price_total = sum(n),
    proportion = n / price_total
  ) %>%
  ungroup() %>%
  filter(word %in% top_words)

word_mods <-
  word_freqs %>%
  nest(data = c(priceRange, n, price_total, proportion)) %>% # 컬럼을 리스트화함.
  mutate(
    model = map(data, ~ glm(cbind(n, price_total) ~ priceRange, ., family = "binomial")),
    model = map(model, tidy)
  ) %>%
  unnest(model) %>%
  filter(term == "priceRange") %>%
  mutate(p.value = p.adjust(p.value)) %>%
  arrange(-estimate)

word_mods

word_mods %>%
  ggplot(aes(estimate, p.value)) +
  geom_vline(xintercept = 0, lty = 2, alpha = 0.7, color = "gray50") +
  geom_point(color = "midnightblue", alpha = 0.8, size = 2.5) +
  scale_y_log10() +
  geom_text_repel(aes(label = word))
