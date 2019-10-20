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
    rows <- 1000000
    tb <- tibble(txt = lines)
    df <- tb %>%
          unnest_tokens(ngram, txt, token = "ngrams", n = result$n)
    df <- df[sapply(df$ngram, function (row) {grepl("^[a-z ]+$", row)}),]
    df <- df %>%
          group_by(ngram) %>%
          count %>%
          arrange(desc(n))
    if (is.null(result$ngrams)) {
        save(df, file = paste0("file_", result$counter, ".RData"))
        #result$ngrams <- df
        result$counter <- result$counter + 1
    } else {
        merged <- merge(result$ngrams, df, by = "ngram", all = "TRUE")
        merged[is.na(merged)] <- 0
        result$ngrams <- merged %>%
                         select(ngram, n.x, n.y) %>%
                         mutate(n = n.x + n.y) %>%
                         select(ngram, n) %>%
                         arrange(desc(n)) #%>%
                         #head(rows)
    }
    result
}

create_ngrams <- function(filenames, ngram_type=2, nrec=50000) {
    for (file in filenames) {
        rdata = paste(file, "RData", sep = ".")
        if (!file.exists(rdata)) {
            ngrams <- checkLines(file,
                                 fetch_ngrams,
                                 initial_value=list(counter=1, ngrams=NULL, n=ngram_type),
                                 nrec=nrec)
            df <- ngrams$ngrams %>%
                  group_by(ngram) %>%
                  summarise(n = sum(n)) %>%
                  arrange(desc(n))
            save(df, file = rdata)
        }
    }
}
