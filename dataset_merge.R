library(rlist)
library(tidyverse)

folders <- c("trigrams") #, "trigrams")
languages <- c("en") #, "de", "fi", "ru")
for (folder in folders) {
    ngram_files <- list.files(folder)
    for (language in languages) {
        files <- grep(paste0("^", language), ngram_files)
        merged <- NULL
        for (file in ngram_files[files]) {
            filename <- paste(folder, file, sep = .Platform$file.sep)
            load(filename)
            if (is.null(merged)) {
                merged <- df
            } else {
                merged <- merge(merged, df, by='ngram', all=TRUE)
                merged[is.na(merged)] <- 0
                merged <- merged %>%
                          mutate(n = n.x + n.y) %>%
                          arrange(desc(n)) %>%
                          select(ngram, n) #%>%
                          #head(100000)
            }
        }
        #g <- ggplot(merged, aes(x='ngrams', y=n)) + geom_violin() +
        #     ggtitle(paste(folder, language, sep = " | "))
        #print(g)
        print(summary(merged))
    }
}

