# Library with functions to read the datasets

readFile <- function(filename, numLines=5, con=NULL, rootFolder="final/en_US/") {
    if (is.null(con))
        con <- file(paste0(rootFolder, filename), "r")
    lines <- readLines(con, numLines)
    list(connection=con, lines=lines)
}

checkLines <- function(filename, search_function, initial_value=NULL, nrec=1000) {
    ret <- readFile(filename, nrec)
    result <- search_function(ret$lines, initial_value)
    while (length(ret$lines) == nrec) {
        ret <- readFile(filename, nrec, ret$connection)
        result <- search_function(ret$lines, result)
    }
    close(ret$connection)
    result
}

mag10 <- function(number) {
    exp10 <- 0
    remainder <- number
    while (remainder > 10) {
        exp10 <- exp10 + 1
        remainder <- number / 10 ^ exp10
    }
    10 ^ exp10
}

read_datasets <- function(dataset, counter_function) {
    total <- list(lines=c(), words=c())
    for (fileName in dataset) {
        count <- checkLines(fileName, counter_function, list(lines=0, words=0))
        total$lines <- c(total$lines, count$lines)
        total$words <- c(total$words, count$words)
    }
    total
}

#Plots the total number of the given metric from the
#datasets. Displays the language, in xx_XX format.
plot_total <- function(datasets, total, metric) {
    total_scaled <- round(total/1e6, 2)
    ynudge <- -.1 * mag10(max(total_scaled))
    yLab <- paste0(metric, " Count (in millions)")
    dt_info <- sapply(datasets, function(name) {str_split(name, "\\.")[[1]]})
    dt <- dt_info[2,]
    plot_title <- paste0("Number Of ", metric, "s In ", dt_info[1, 1])
    g <- ggplot(mapping=aes(dt, total_scaled))
    g + geom_col(aes(fill=dt)) +
        xlab("Datasets") +
        ylab(yLab) +
        ggtitle(plot_title) +
        geom_text(aes(label=total_scaled), nudge_y = ynudge) +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
}