# Step 2: Create ngram RDS files
# for each small file created in step 1
# clean file and create ngrams (bigrams and trigrams)
# output each ngram file in a directory
# unigrams won't be needed because from EDA we know the top results are "the", "to", and "and"

library(quanteda)

# download and read in profanity list as a character vector
if (!dir.exists("./profanity")) {
    dir.create("./profanity")
}

if (!file.exists("./profanity/bad-words.zip")) {
    url <- "https://www.freewebheaders.com/wordpress/wp-content/uploads/full-list-of-bad-words-banned-by-google-txt-file.zip"
    download.file(url, dest = "./profanity/bad-words.zip", mode = "wb") 
    unzip("./profanity/bad-words.zip", exdir = "./profanity")
}

if (!exists("profanity")) {
    profanity <- readLines("./profanity/full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt", skipNul = TRUE, warn = FALSE)
    profanity <- iconv(profanity, "latin1", "ASCII", sub = "")
}

# get list of files created in step 1
file_names <- list.files("./text")

# create directory for ngram output
if (!dir.exists("./ngrams")) {
    dir.create("./ngrams")
}

# function to clean files and output ngrams
create_ngrams <- function(file_name) {
    
    # read in a file
    file <- readRDS(paste0("./text/", file_name))

    # tokenize file
    words <- tokens(char_tolower(file, keep_acronyms = TRUE), what = "word", 
                 remove_numbers = TRUE, remove_punct = TRUE,
                 remove_symbols = TRUE, remove_hyphens = TRUE,
                 remove_twitter = TRUE, remove_url = TRUE,
                 ngrams = 1L)
    rm(file)

    # remove profanity
    words <- tokens_remove(words, profanity)

    # create bigrams and trigrams
    ngrams <- unlist(tokens_ngrams(words, n = 2:3, concatenator = " "), use.names = FALSE)
    rm(words)

    # save to rds file
    saveRDS(ngrams, paste0("./ngrams/", file_name))
    rm(ngrams)
}

# create ngrams for each file created in step 1
system.time(sapply(file_names, create_ngrams))
rm(list = ls())
