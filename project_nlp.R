datasets <- list()
datasets$languages <- c("en_US", "de_DE", "fi_FI", "ru_RU")
datasets$sources <- c("blogs", "news", "twitter")

dt <- matrix(nrow = 3, ncol = 1)
for (lang in datasets$languages) {
    col <- c()
    for (src in datasets$sources) {
        col <- c(col, paste(lang, src, 'txt', sep = '.'))
    }
    dt <- cbind(dt, col)
}
dt <- dt[1:3, 2:5]

source("dataset_tools.R")


## Creating N-Grams
library(tidyverse)
library(tidytext)

fetch_ngrams <- function(lines, result) {
    tb <- data_frame(txt = lines)
    df <- tb %>%
          unnest_tokens(ngram, txt, token = "ngrams", n = result$n) %>%
          arrange(ngram) %>%
          group_by(ngram) %>%
          count
    if (is.null(result$ngrams)) {
        result$ngrams <- df
    } else {
        merged <- merge(result$ngrams, df, by = "ngram", all = "TRUE")
        merged[is.na(merged)] <- 0
        result$ngrams <- merged %>%
                         select(ngram, n.x, n.y) %>%
                         mutate(n = n.x + n.y) %>%
                         select(ngram, n)
    }
    result
}

create_ngrams <- function(filenames, ngram_type=2) {
    for (file in filenames) {
        rdata = paste(file, "RData", sep = ".")
        if (!file.exists(rdata)) {
            ngrams <- checkLines(file,
                                  fetch_ngrams,
                                  initial_value=list(ngrams=NULL, n=ngram_type),
                                  nrec=100000)
            df <- ngrams$ngrams %>%
                  group_by(ngram) %>%
                  summarise(n = sum(n)) %>%
                  arrange(desc(n))
            save(df, file = rdata)
        }
    }
}

create_ngrams(dt[3], 3)