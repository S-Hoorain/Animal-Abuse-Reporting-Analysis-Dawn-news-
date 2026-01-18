# Animal Abuse Reporting Analysis (Dawn News) 

## Overview

This R project performs an end-to-end text mining pipeline to analyze media coverage of animal abuse. Specifically tailored for **Dawn News** articles, the script scrapes content, cleans the text, and performs advanced sentiment analysis, trend tracking, and network visualization.

## Key Features

### 1. Robust Web Scraping
* **Polite Scraping:** Includes `robotstxt` checks and `Sys.sleep` delays to respect server load.
* **Caching System:** Saves scraped data to `output/scrape_cache.rds` to prevent re-downloading articles during subsequent runs.
* **Selector Logic:** Custom HTML selectors designed to extract text specifically from Dawn News article structures.

### 2. Text Preprocessing
* **Cleaning:** Removes ads, boilerplate text, and URLs.
* **Normalization:** Converts text to lowercase and removes punctuation/digits.
* **Lemmatization:** Uses `textstem` to reduce words to their root form (e.g., "running" -> "run").
* **Stopwords:** Filters out common English words and specific scrape artifacts (e.g., "image", "photo").

### 3. Analytics & Visualizations
The script generates high-resolution figures in `output/figures/`:
* **Word Frequency:** Top words bar charts and Wordclouds.
* **Temporal Trends:** Articles per year and frequency trends for the top 25 words.
* **Semantic Landscape:** A scatter plot comparing **IDF** (Uniqueness) vs. **Frequency** (Commonality).
* **Sentiment Analysis:** Tracks net sentiment (Positive - Negative) over time using the Bing lexicon.
* **Animal Specific Trends:** Tracks mentions of 35+ specific animals (e.g., dogs, donkeys, lions) over the years.

### 4. Network Analysis
* **Word Co-occurrence:** A network graph showing which words frequently appear together in the same article.
* **Article Similarity:** A network graph linking articles that share significant vocabulary, colored by publication year.

---

## Prerequisites

Ensure you have R and RStudio installed. Install the required packages by running:

```r
install.packages(c(
  "tidyverse", "rvest", "httr", "robotstxt", "tidytext", 
  "textstem", "lubridate", "stringr", "widyr", "igraph", 
  "ggraph", "wordcloud", "RColorBrewer", "janitor", "readr", 
  "dplyr", "tidyr", "ggplot2"
))

---

## Input Data Requirements

The script requires a file named Links.csv in the project root directory.
Required Columns:
1. url: The direct link to the article.
2. articledate (or publication_date): Date format should be parseable (e.g., YYYY-MM-DD, DD-MM-YYYY).
3. articletitle: The headline of the article.

---

## How to Run
Place your Links.csv in the project folder.
1. Open the R script in RStudio.
2. Run the entire script (Ctrl + Alt + R or Cmd + Shift + R).
3. Check the console for progress messages regarding scraping and processing.

---

## Output Files
The script automatically creates an output/ directory containing:
GraphMl files, Figures and Metrics CSV's
