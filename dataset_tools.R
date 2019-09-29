# Library with functions to read the datasets

# Receives the filename, the chunck size numLines, with an optional
# connection object and root folder. Returns a connection and the 
# number of lines read from filename, given the chunk numLines.
# The files are located in rootFolder.
readFile <- function(filename, numLines=5, con=NULL, rootFolder="final") {
    folder <- paste(rootFolder, str_split(filename, "\\.")[[1]][1], sep = .Platform$file.sep)
    if (is.null(con))
        con <- file(paste(folder, filename, sep = .Platform$file.sep), "r")
    lines <- readLines(con, numLines)
    list(connection=con, lines=lines)
}

# Receives a filename, a search function, an initial value, and
# the number of records to read. Returns the result of the search function,
# considering the initial values, after reading nrec lines from filename.
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

# Magnitude of the number, in terms of exponent of 10.
# For example, 34 ~ 10 ^ 1, 342 ~ 10 ^ 2.
mag10 <- function(number) {
    exp10 <- 0
    remainder <- number
    while (remainder > 10) {
        exp10 <- exp10 + 1
        remainder <- number / 10 ^ exp10
    }
    10 ^ exp10
}

# Receives a dataset_files and a counter_function. Returns the total number
# of lines and words in the dataset_files, per file.
read_datasets <- function(dataset_files, counter_function) {
    total <- list(lines=c(), words=c())
    for (file_name in dataset_files) {
        count <- checkLines(file_name, counter_function, list(lines=0, words=0))
        total$lines <- c(total$lines, count$lines)
        total$words <- c(total$words, count$words)
    }
    total
}

# Plots the total number of the given metric from the
# datasets. Displays the language, in xx_XX format.
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
