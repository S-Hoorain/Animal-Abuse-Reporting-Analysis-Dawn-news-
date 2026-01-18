library(tidyverse)
library(rvest)
library(httr)
library(robotstxt)
library(tidytext)
library(textstem)
library(lubridate)
library(stringr)
library(widyr)
library(igraph)
library(ggraph)
library(wordcloud)
library(RColorBrewer)
library(janitor)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# -------------------------
# 1. Config
# -------------------------
input_csv <- "Links.csv"
output_dir <- "output"
fig_dir <- file.path(output_dir, "figures")
dir.create(output_dir, showWarnings = FALSE)
dir.create(fig_dir, showWarnings = FALSE)

cache_file <- file.path(output_dir, "scrape_cache.rds")
ua <- httr::user_agent("HabibUniversity - animal-abuse project - student")

# -------------------------
# 2. Read dataset
# -------------------------
if(!file.exists(input_csv)) stop("Input CSV not found. Put your file named 'Links.csv' (or change input_csv).")

raw <- read_csv(input_csv, show_col_types = FALSE) %>% janitor::clean_names()

# Expect columns: articledate / article_date or articledate, url, articletitle
# Try to normalize column names:
if("articledate" %in% names(raw)) raw <- raw %>% rename(article_date = articledate)
if("article_date" %in% names(raw)) raw <- raw %>% rename(publication_date = article_date)
if("article_title" %in% names(raw)) raw <- raw %>% rename(article_title = article_title)  # no-op to be explicit
if(!("url" %in% names(raw))) stop("Input CSV must contain a column named 'URL' or 'url' (case-insensitive).")

# Parse dates when possible:
if("publication_date" %in% names(raw)) {
  raw <- raw %>% mutate(publication_date = parse_date_time(publication_date, orders = c("ymd","mdy","dmy","Y-m-d")))
} else if("articledate" %in% names(raw)) {
  raw <- raw %>% mutate(publication_date = parse_date_time(articledate, orders = c("ymd","mdy","dmy","Y-m-d")))
}

# Ensure url and title columns exist; keep original article title if present
if(!("article_title" %in% names(raw))) raw$article_title <- NA_character_
df_meta <- raw %>% select(publication_date, article_title, url)

message("Rows read: ", nrow(df_meta))

# -------------------------
# 3. Check robots.txt
# -------------------------
# check domain of first url (Dawn)
first_domain <- tryCatch({
  parsed <- httr::parse_url(df_meta$url[1])
  parsed$hostname
}, error = function(e) NA)

if(!is.na(first_domain)) {
  r_allowed <- robotstxt::paths_allowed(paths = df_meta$url, user_agent = "*")
  # r_allowed is logical vector; if any FALSE, show warning
  if(any(!r_allowed)) {
    warning("robots.txt indicates some paths may be disallowed. Proceed only with instructor permission.")
  } else {
    message("robots.txt seems OK for these URLs (no blanket disallow).")
  }
}

# -------------------------
# 4. Scraping function + caching
# -------------------------
# Primary selector: div.story__content p
# Fallbacks: article p  OR any <p> text
extract_article_text_from_html <- function(page) {
  # Try primary selector
  pts <- page %>% html_nodes("div.story__content p")
  if(length(pts) == 0) {
    # common Dawn alternative classes
    pts <- page %>% html_nodes("div.article__body p, div.article-body p, div.entry-content p, article p")
  }
  if(length(pts) == 0) {
    # last resort: all <p>
    pts <- page %>% html_nodes("p")
  }
  if(length(pts) == 0) return(NA_character_)
  # Remove empty paragraphs and ad placeholders
  texts <- pts %>% html_text(trim = TRUE) %>% keep(~ !str_detect(.x, "^\\s*$"))
  # Remove typical ad placeholders (these often empty or contain script markers)
  texts <- texts[!str_detect(texts, "(googletag|adsbygoogle|ad__wrapper|advert|— The|—The)")]
  full <- paste(texts, collapse = "\n\n")
  full %>% str_squish()
}

safe_fetch <- function(url_i, sleep = 1.5) {
  # return tibble(url, title, date_scraped, text)
  Sys.sleep(sleep)  # polite pause
  tryCatch({
    resp <- httr::GET(url_i, ua, httr::timeout(30))
    if(httr::status_code(resp) >= 400) {
      message("HTTP failure: ", httr::status_code(resp), " for URL: ", url_i)
      return(tibble(url = url_i, title = NA_character_, date_scraped = NA_character_, text = NA_character_))
    }
    pg <- read_html(resp)
    # title candidates
    title_node <- pg %>% html_nodes("h1, .entry-title, .story__title, title") %>% html_text(trim = TRUE)
    title_scraped <- if(length(title_node) > 0) title_node[1] else NA_character_
    # date candidate - try <time> or meta tags
    date_node <- pg %>% html_nodes("time, meta[property='article:published_time'], meta[name='pubdate'], span.date")
    date_val <- NA_character_
    if(length(date_node) > 0) {
      date_val <- date_node %>% html_attr("datetime") %>% na.omit() %>% .[1]
      if(is.na(date_val)) date_val <- date_node %>% html_text(trim = TRUE) %>% .[1]
    }
    text_full <- extract_article_text_from_html(pg)
    tibble(url = url_i, title = title_scraped, date_scraped = date_val, text = text_full)
  }, error = function(e) {
    message("Error fetching url: ", url_i, " - ", e$message)
    tibble(url = url_i, title = NA_character_, date_scraped = NA_character_, text = NA_character_)
  })
}

# Load cache (if exists) to avoid re-downloading
if(file.exists(cache_file)) {
  cache <- readRDS(cache_file)
  message("Loaded existing cache with ", nrow(cache), " entries.")
} else {
  cache <- tibble(url = character(), title = character(), date_scraped = character(), text = character(), fetched = as.Date(character()))
}

# Determine which URLs to fetch
to_fetch <- df_meta %>% filter(!url %in% cache$url)

if(nrow(to_fetch) > 0) {
  message("Fetching ", nrow(to_fetch), " URLs (this may take some minutes).")
  fetched_list <- map(to_fetch$url, ~ safe_fetch(.x, sleep = 1.5))
  fetched_tbl <- bind_rows(fetched_list) %>% mutate(fetched = Sys.Date())
  cache <- bind_rows(cache, fetched_tbl)
  saveRDS(cache, cache_file)
} else {
  message("All URLs already cached.")
}

# Build corpus by merging metadata with cache
corpus <- df_meta %>% left_join(cache %>% select(url, title, date_scraped, text, fetched), by = "url") %>%
  mutate(
    publication_date = coalesce(publication_date, parse_date_time(date_scraped, orders = c("ymd","Y-m-d","dmy","mdy"))),
    scraped_title = title
  ) %>%
  select(publication_date, article_title, scraped_title, url, text, fetched)

# Save raw scraped corpus
write_csv(corpus, file.path(output_dir, "articles_corpus_raw.csv"))
message("Raw corpus saved to ", file.path(output_dir, "articles_corpus_raw.csv"))

# -------------------------
# 5. Text cleaning & preprocessing
# -------------------------
# Cleaning function
clean_text <- function(x) {
  if(is.na(x)) return(NA_character_)
  x %>%
    # normalize apostrophes / quotes
    str_replace_all("\u2018|\u2019|\u201c|\u201d", "'") %>%
    # remove URLs, scripts
    str_replace_all("https?://\\S+\\s?", " ") %>%
    # remove non-ascii (keep basic punctuation temporarily)
    iconv(from = "UTF-8", to = "ASCII", sub = " ") %>%
    # remove punctuation and digits, keep whitespace
    str_replace_all("[[:punct:]]+", " ") %>%
    str_replace_all("[0-9]+", " ") %>%
    str_squish() %>%
    tolower()
}

corpus <- corpus %>% mutate(text_clean = map_chr(text, ~ safely(clean_text)(.x)[["result"]]))# Remove rows with no text
corpus <- corpus %>% filter(!is.na(text_clean) & str_trim(text_clean) != "")

# Save cleaned corpus
write_csv(corpus, file.path(output_dir, "articles_corpus_cleaned.csv"))
message("Cleaned corpus saved to ", file.path(output_dir, "articles_corpus_cleaned.csv"))

# -------------------------
# 6. Tokenize, stopwords, lemmatize
# -------------------------
data("stop_words", package = "tidytext")

tokens <- corpus %>%
  select(publication_date, article_title, url, text_clean) %>%
  unnest_tokens(word, text_clean) %>%
  # remove stopwords & short tokens
  anti_join(stop_words, by = "word") %>%
  filter(str_length(word) > 2) %>%
  # remove stray artefacts
  filter(!word %in% c("dawn","advertisement","image","photo")) %>%
  # lemmatize for readability (textstem)
  mutate(word = lemmatize_words(word))

# Word frequency
word_freq <- tokens %>% count(word, sort = TRUE)
write_csv(word_freq, file.path(output_dir, "word_freq.csv"))
message("Word frequency saved to ", file.path(output_dir, "word_freq.csv"))

# -------------------------
# 7. Visualizations
# -------------------------

# (A) Top words bar chart
top_n <- 25
top_words <- word_freq %>% slice_max(n, n = top_n)
p_top <- ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") + coord_flip() +
  labs(title = sprintf("Top %d words in Dawn articles (animal abuse)", top_n),
       x = "", y = "Frequency") +
  theme_minimal(base_size = 12)
ggsave(file.path(fig_dir, "top_words_bar.png"), p_top, width = 8, height = 6)
message("Saved top words bar plot.")

# (B) Wordcloud (top 150)
set.seed(123)
wpdf <- word_freq %>% filter(n > 1)  # threshold to remove singletons
png(file.path(fig_dir, "wordcloud.png"), width = 1000, height = 800)
wordcloud(words = wpdf$word, freq = wpdf$n, max.words = 150, random.order = FALSE,
          rot.per = 0.1, scale = c(4, 0.5), colors = brewer.pal(8, "Dark2"))
dev.off()
message("Saved wordcloud.")

# (C) Articles per year
corpus_yearly <- corpus %>% mutate(year = year(publication_date)) %>%
  group_by(year) %>% summarise(n_articles = n(), .groups = "drop") %>%
  filter(!is.na(year))

p_years <- ggplot(corpus_yearly, aes(x = year, y = n_articles)) +
  geom_col(fill = "darkred") + labs(title = "Number of Dawn articles per year (animal abuse)", x = "Year", y = "Number of articles") +
  theme_minimal()
ggsave(file.path(fig_dir, "articles_by_year.png"), p_years, width = 8, height = 5)
message("Saved articles by year plot.")

# -------------------------------------------------------------------------
# D1 Top 25 Words – Split into 3 Graphs (1-10, 11-20, 21-25)
# -------------------------------------------------------------------------

# 1. Get top 25 words by total frequency
top25_freq <- word_freq %>% slice_max(n, n = 25) %>% 
  mutate(rank = row_number())

# 2. Full year range
all_years <- 2010:2024

# 3. Function (fixed)
make_trend_plot <- function(data, title_suffix, filename) {
  p <- ggplot(data, aes(x = year, y = freq, color = word)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5) +
    scale_color_brewer(palette = "Paired", name = "Word") +
    scale_x_continuous(breaks = seq(2010, 2024, 2), minor_breaks = 2010:2024) +
    labs(
      title = paste("Yearly Frequency of", title_suffix),
      x = "Year", y = "Frequency in Articles"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  ggsave(file.path(fig_dir, filename), plot = p,
         width = 12, height = 6, dpi = 300)
  message("Saved: ", filename)
}

# 4. Prepare full data with zero-fill
trend_full <- tokens %>%
  mutate(year = year(publication_date)) %>%
  filter(!is.na(year), word %in% top25_freq$word) %>%
  count(year, word, name = "freq") %>%
  complete(year = all_years, word, fill = list(freq = 0)) %>%
  left_join(top25_freq %>% select(word, rank, total_n = n), by = "word") %>%
  arrange(rank, year)

# 5. Split into 3 groups and plot
trend_1_10   <- trend_full %>% filter(rank <= 10)
trend_11_20  <- trend_full %>% filter(rank > 10 & rank <= 20)
trend_21_25  <- trend_full %>% filter(rank > 20)

make_trend_plot(trend_1_10,  "Top 1–10 Most Common Words",  "term_trends_top1_10.png")
make_trend_plot(trend_11_20, "Top 11–20 Most Common Words", "term_trends_top11_20.png")
make_trend_plot(trend_21_25, "Top 21–25 Most Common Words", "term_trends_top21_25.png")

# -------------------------------------------------------------------------
# Save ALL words with IDF, document frequency, total count
# -------------------------------------------------------------------------
total_docs <- n_distinct(tokens$url)

idf_full <- tokens %>%
  count(word, name = "doc_freq") %>%
  mutate(
    idf = log(total_docs / doc_freq),
    total_freq = map_dbl(word, ~ sum(tokens$word == .x))
  ) %>%
  arrange(desc(idf))

# Save full table
write_csv(idf_full, file.path(output_dir, "all_words_idf_full.csv"))

# Also save top 100 for quick reference
write_csv(idf_full %>% slice_max(idf, n = 100),
          file.path(output_dir, "top100_idf_words.csv"))

message("Saved: all_words_idf_full.csv (", nrow(idf_full), " words)")
message("Saved: top100_idf_words.csv")

# -------------------------------------------------------------------------
# (D2) IDF vs. Frequency Scatter – Semantic Landscape
# -------------------------------------------------------------------------
set.seed(42)
plot_data <- idf_full %>%
  mutate(log_freq = log10(total_freq + 1)) %>%
  arrange(desc(idf))

# Highlight top 20 IDF + top 20 freq
highlight_idf <- plot_data %>% slice_max(idf, n = 20)
highlight_freq <- plot_data %>% slice_max(total_freq, n = 20)

p_scatter <- ggplot(plot_data, aes(x = log_freq, y = idf)) +
  geom_point(color = "gray70", alpha = 0.6, size = 1.2) +
  geom_point(data = highlight_idf, color = "red", size = 3, shape = 17) +
  geom_point(data = highlight_freq, color = "blue", size = 3, shape = 15) +
  geom_text(data = highlight_idf %>% slice_max(idf, n = 10),
            aes(label = word), vjust = -1, size = 3.5, color = "red") +
  geom_text(data = highlight_freq %>% slice_max(total_freq, n = 8),
            aes(label = word), vjust = 1.8, size = 3.5, color = "blue") +
  labs(
    title = "Semantic Landscape: IDF vs. Log(Frequency)",
    subtitle = "Red = Most Distinctive | Blue = Most Common",
    x = "Log10(Total Frequency + 1)", y = "IDF Score"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(fig_dir, "idf_vs_frequency_scatter.png"), p_scatter,
       width = 10, height = 7, dpi = 300)

message("Saved: idf_vs_frequency_scatter.png")

# -------------------------------------------------------------------------
# 8. Sentiment Trajectory (using Bing Lexicon)
# -------------------------------------------------------------------------
all_years <- 2010:2024

sent_full <- tokens %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  mutate(year = year(publication_date)) %>%
  count(year, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>%
  mutate(net = positive - negative) %>%
  
# Fill missing years with 0
complete(year = all_years,
         fill = list(positive = 0, negative = 0, net = 0))

p_sent <- ggplot(sent_full, aes(x = year, y = net)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(breaks = all_years,       # <-- every year
                     labels = all_years) +
  labs(title = "Net Sentiment Over Time (Bing Lexicon)",
       subtitle = "All years 2010–2024 shown (missing years = net 0)",
       x = "Year", y = "Positive − Negative") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(fig_dir, "sentiment_trend_full.png"),
       plot = p_sent, width = 13, height = 5, dpi = 300)

message("Saved: sentiment_trend_full.png")

# -------------------------------------------------------------------------
# 9. ANIMAL FREQUENCY TRENDS – Top 10
# -------------------------------------------------------------------------

# 1. Define animal list (add/remove as needed)
animal_list <- c(
  "dog", "stray", "puppy", "canine",
  "cat", "kitten", "feline",
  "donkey", "mule", "ass",
  "horse", "mare", "foal",
  "cow", "bull", "calf", "cattle",
  "goat", "sheep", "lamb",
  "camel", "elephant", "monkey", "bear", "tiger", "lion",
  "bird", "parrot", "chicken", "rooster",
  "snake", "crocodile", "turtle", "fish", "rabbit"
)

# 2. Filter tokens for animals
animal_tokens <- tokens %>%
  filter(word %in% animal_list) %>%
  mutate(year = year(publication_date)) %>%
  filter(!is.na(year))

# 3. Get top 30 animals by total frequency
top30_animals <- animal_tokens %>%
  count(word, name = "total_freq") %>%
  arrange(desc(total_freq)) %>%
  slice_max(total_freq, n = 30) %>%
  mutate(rank = row_number())

# 4. Full yearly data with zero-fill
all_years <- 2010:2024

trend_full <- animal_tokens %>%
  count(year, word, name = "freq") %>%
  complete(year = all_years, word, fill = list(freq = 0)) %>%
  right_join(top30_animals %>% select(word, rank), by = "word") %>%
  arrange(rank, year)

# 5. Function to plot one group
make_animal_plot <- function(data, title_suffix, filename) {
  p <- ggplot(data, aes(x = year, y = freq, color = word)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.8) +
    scale_color_brewer(palette = "Set3", name = "Animal") +  # HIGH-CONTRAST
    scale_x_continuous(breaks = 2010:2024, labels = 2010:2024) +
    labs(
      title = paste("Animal Mentions:", title_suffix),
      subtitle = "Frequency in Dawn articles (2010–2024)",
      x = "Year", y = "Frequency"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
  
  ggsave(file.path(fig_dir, filename), plot = p,
         width = 13, height = 6.5, dpi = 300)
  message("Saved: ", filename)
}

# 6. Split into 3 groups
trend_1_10  <- trend_full %>% filter(rank <= 10)

# 7. Generate plots
make_animal_plot(trend_1_10,  "Top 1–10 Animals",  "animal_trends_top1_10.png")

# 8. Save full data
write_csv(trend_full, file.path(output_dir, "animal_yearly_top10.csv"))
message("Saved: animal_yearly_top30.csv")

# -------------------------------------------------------------------------
# 10. Co-occurrence Network
# -------------------------------------------------------------------------

# NODES: one word from the top 60 most frequent terms
# EDGES: undirected edge exists between two words if they appear together in the same article
# EDGE WEIGHT: number of UNIQUE ARTICLES in which both words appear (max = 68)

# Create a binary article-word matrix (each word appears at most once per article)
article_words <- tokens %>% 
  distinct(url, word)  # This is correct - one row per article-word pair

# Now compute pairwise co-occurrence counting DOCUMENTS, not word instances
pairwise <- article_words %>% 
  pairwise_count(item = word, feature = url, sort = TRUE)

# The 'n' column now represents the number of ARTICLES where both words appear
# Maximum possible value = 68 (total number of articles)

# Filter to top K words
top_k <- 60
top_k_words <- word_freq %>% slice_max(n, n = top_k) %>% pull(word)

# Filter pairs to only include top K words
pairs_filtered <- pairwise %>% 
  filter(item1 %in% top_k_words, item2 %in% top_k_words)

# Verify edge weights are reasonable
message("Edge weight range: ", min(pairs_filtered$n), " to ", max(pairs_filtered$n))
message("Maximum should be <= 68 (total articles)")

# Build graph (undirected)
g <- graph_from_data_frame(pairs_filtered, directed = FALSE)
E(g)$weight <- pairs_filtered$n

# Simplify & filter weak edges
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
edge_thresh <- 2  # keep edges appearing in at least 2 articles
g_plot <- delete_edges(g, E(g)[weight < edge_thresh])
g_plot <- delete_vertices(g_plot, V(g_plot)[degree(g_plot) == 0])

# Basic metrics
V(g_plot)$degree <- degree(g_plot)
deg_df <- tibble(term = V(g_plot)$name, degree = V(g_plot)$degree) %>% 
  arrange(desc(degree))
write_csv(deg_df, file.path(output_dir, "node_degrees.csv"))

# Plot network
png(file.path(fig_dir, "cooccurrence_network.png"), width = 1200, height = 1000)
set.seed(42)
ggraph(g_plot, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.7, color = "gray70") +
  scale_edge_width(range = c(0.3, 2), name = "Shared Articles") +
  geom_node_point(aes(size = degree), color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  scale_size_continuous(range = c(3, 8), name = "Degree") +
  theme_void() +
  labs(
    title = sprintf("Word Co-occurrence Network (Top %d words)", top_k),
    subtitle = sprintf("Edge weight = # articles where both words appear (threshold ≥ %d)", edge_thresh),
    caption = sprintf("Max possible edge weight: %d articles", total_articles)
  )
dev.off()
message("Saved cooccurrence network image.")

# Save graph for Gephi
write_graph(g_plot, file.path(output_dir, "cooccurrence_network.graphml"), format = "graphml")
message("Saved cooccurrence graphml.")

# -------------------------------------------------------------------------
# 10-B. ARTICLE-LEVEL CO-OCCURRENCE NETWORK
# -------------------------------------------------------------------------

# Nodes = articles (url), Edge = share at least one word

# 1. Total articles check
total_articles <- n_distinct(tokens$url)
message("Total articles: ", total_articles)  # Should print 68

# 2. Build binary article × word matrix
article_word_matrix <- tokens %>%
  distinct(url, word) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = word, values_from = present, values_fill = 0)

article_ids <- article_word_matrix$url
word_matrix <- article_word_matrix %>% select(-url) %>% as.matrix()
rownames(word_matrix) <- article_ids

# 3. Compute shared words (dot product)
shared_words <- tcrossprod(word_matrix)
diag(shared_words) <- 0

# 4. Long format
pairs_articles <- as.data.frame(as.table(shared_words)) %>%
  rename(article1 = Var1, article2 = Var2, n_shared = Freq) %>%
  filter(n_shared >= 2) %>%        # At least 2 shared words
  arrange(desc(n_shared))

# 5. Use ALL articles (68 is small → no need to filter!)
pairs_filtered_art <- pairs_articles %>%
  mutate(article1 = as.character(article1), article2 = as.character(article2))

# 6. Build graph
g_art <- graph_from_data_frame(pairs_filtered_art, directed = FALSE)
E(g_art)$weight <- pairs_filtered_art$n_shared

# 7. Add year
article_year <- corpus %>%
  select(url, publication_date) %>%
  mutate(year = year(publication_date)) %>%
  filter(url %in% V(g_art)$name)

V(g_art)$year <- article_year$year[match(V(g_art)$name, article_year$url)]
V(g_art)$year <- ifelse(is.na(V(g_art)$year), "Unknown", as.character(V(g_art)$year))

# 8. Degree
V(g_art)$degree <- degree(g_art)

# 9. Plot
set.seed(123)
p_art_net <- ggraph(g_art, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.6, color = "gray60") +
  geom_node_point(aes(size = degree, color = year), alpha = 0.9) +
  scale_size_continuous(range = c(3, 10), name = "Connections") +
  scale_color_viridis_d(name = "Year") +
  geom_node_text(aes(label = year), repel = TRUE, size = 3.5, fontface = "bold") +
  theme_void() +
  labs(
    title = "Article Co-occurrence Network (All 68 Articles)",
    subtitle = "Edge = ≥2 shared words | Node size = connections | Color = year",
    caption = paste("Total nodes:", vcount(g_art), "| Total edges:", ecount(g_art))
  ) +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Save
png(file.path(fig_dir, "article_cooccurrence_68.png"), width = 1200, height = 1000, res = 300)
print(p_art_net)
dev.off()
message("Saved: article_cooccurrence_68.png")

# Save for Gephi
V(g_art)$label <- paste("Art", seq_along(V(g_art)), sep = "_")
write_graph(g_art, file.path(output_dir, "article_cooccurrence_68.graphml"), format = "graphml")
message("Saved: article_cooccurrence_68.graphml")

# -------------------------------------------------------------------------
# 10-C. DOCUMENT FREQUENCY BAR CHART: Top 25 Words by Number of Articles
# -------------------------------------------------------------------------

# Count how many UNIQUE articles each word appears in
doc_freq <- tokens %>%
  distinct(url, word) %>%  # One row per article-word pair
  count(word, name = "n_articles", sort = TRUE)

# Get top 25 by document frequency
top25_doc_freq <- doc_freq %>% 
  slice_max(n_articles, n = 25)

# Save this data
write_csv(top25_doc_freq, file.path(output_dir, "top25_words_by_articles.csv"))
message("Saved: top25_words_by_articles.csv")

# Create bar chart
p_doc_freq <- ggplot(top25_doc_freq, aes(x = reorder(word, n_articles), y = n_articles)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 25 Words by Article Frequency",
    subtitle = "Number of articles in which each word appears at least once",
    x = "Word",
    y = "Number of Articles (out of 68 total)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank()
  )

ggsave(file.path(fig_dir, "top25_doc_frequency_bar.png"), p_doc_freq, 
       width = 10, height = 8, dpi = 300)
message("Saved: top25_doc_frequency_bar.png")

# -------------------------
# Save key outputs
# -------------------------
write_csv(tokens, file.path(output_dir, "tokens.csv"))
write_csv(pairwise, file.path(output_dir, "pairwise_full.csv"))
write_csv(pairs_filtered, file.path(output_dir, "pairwise_filtered.csv"))
write_csv(corpus %>% select(publication_date, article_title, scraped_title, url, text), file.path(output_dir, "final_corpus_text.csv"))

# Save session info for reproducibility
capture.output(sessionInfo(), file = file.path(output_dir, "sessionInfo.txt"))

message("Pipeline finished. Outputs and figures are in the '", normalizePath(output_dir), "' directory.")
message("If some pages returned NA text, open '", cache_file, "' to inspect fetch results and debug selectors for problem URLs.")
