library(tidyverse)

tb1 <- data_frame(txt=c('A', 'B', 'C'), n=c(1, 2, 3))
tb2 <- data_frame(txt=c('A', 'B', 'D'), n=c(5, 6, 7))
merged <- merge(tb1, tb2, by='txt', all = T)
merged[is.na(merged)] <- 0
df <- merged %>%
      select(txt, n.x, n.y) %>%
      mutate(n = n.x + n.y) %>%
      select(txt, n)

print(df)
