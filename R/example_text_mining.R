# Load libraries ---------------------------------------------------------------

library(here)
library(gutenbergr)
library(stringr)
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(igraph)
library(ggraph)

# Retrieve data ----------------------------------------------------------------

# 'The Sportswoman's Library'
sportswomans <- gutenberg_download(41436)

# Clean data -------------------------------------------------------------------

sportswomans_clean <- sportswomans

# Remove records that include front matter (e.g., cover, TOC)
sportswomans_clean <- sportswomans_clean %>%
  mutate(dummy_num = 1,
         dummy_count = cumsum(dummy_num)) %>%
  filter(dummy_count > 168) %>%
  select(-dummy_num, -dummy_count)

# Tokenise bigrams
sportswomans_bigrams <- sportswomans_clean %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# Remove bigrams that include stop words
bigrams_separated <- sportswomans_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Count bigram frequency
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE) %>%
  mutate(bigram = paste0(word1, " ", word2))
# bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
# bigrams_united

# Plot bigram frequency --------------------------------------------------------

# Filter to include bigrams that appeared more than 10 times
bigram_counts_selected <- bigram_counts %>%
  filter(n > 10) %>%
  mutate(bigram = factor(bigram,
                         levels = rev(unique(bigram))))

# Build plot
p <- ggplot(bigram_counts_selected,
            aes(x = bigram, y = n, fill = n, label = n))
p <- p + geom_bar(stat = "identity")
p <- p + geom_text(size = 3.5, hjust = -0.3)
p <- p + labs(
  title = "Bigrams that appeared more than 10 times",
  subtitle = "In: 'The Sportswoman's Library' by Frances E. Slaughter",
  x = NULL, y = NULL)
p <- p + coord_flip()
p <- p + theme(
  text = element_text(size = 14),
  legend.position = "none")

# Export to PNG
ggsave(here("images/sportswomans_lib_frequency.png"),
       width = 16, height = 9, units = "cm", dpi = 300)

# Convert tidy data to graph data ----------------------------------------------

bigram_graph <- bigram_counts %>%
  filter(n > 7) %>%
  graph_from_data_frame()

# Plot bigram network ----------------------------------------------------------

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

# Build plot
set.seed(2017)
p <- ggraph(bigram_graph, layout = "fr")
p <- p + geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                        arrow = a, end_cap = circle(.07, 'inches'))
p <- p + geom_node_point(color = "#D55E00", size = 4)
p <- p + geom_node_text(aes(label = name), size = 4,
                        vjust = 1.1, hjust = 1.1)
p <- p + theme_void()

# Export plot to PNG
ggsave(here("images/sportswomans_lib_bigrams.png"),
       width = 16, height = 9, units = "cm", dpi = 300)