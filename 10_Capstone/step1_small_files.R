# Step 1: Create smaller character vectors
# download, read in and sample raw data
# then split corpus into directory of smaller text files for easier processing
# smaller files should help computationally-intensive tokenization in step 2

library(readr)
library(quanteda)

# download and unzip the specific files needed
if (!file.exists("Coursera-SwiftKey.zip")) {
    url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    download.file(url, dest = "Coursera-SwiftKey.zip", mode = "wb") 
    
    # create a list of only the english language files
    en_files <- grep("^final/en_US/en_US", 
                     unzip("Coursera-SwiftKey.zip", 
                           list = TRUE)$Name, value = TRUE)
    
    # unzip only that list
    unzip("Coursera-SwiftKey.zip", files = en_files, exdir = "./")
}

# read files into single character vector
all_data <- unlist(lapply(list.files("./final/en_US", full.names = TRUE), read_lines))

# take a sample
set.seed(417)
sample <- sample(all_data, length(all_data)*0.15)
rm(all_data)

# remove foreign characters
sample <- iconv(sample, "latin1", "ASCII", sub = "")

# remove very short strings that won't help in prediction
sample <- char_trim(sample, what = "sentences", min_ntoken = 3)

# splits corpus into a list of character vectors each length of 100
split_text <- split(sample, (seq(length(sample)) - 1) %/% 100)
rm(sample)

# create directory for output of files
if (!dir.exists("./text")) {
    dir.create("./text")
}

# loop over list creating smaller files
system.time(for (name in names(split_text)) {
    saveRDS(split_text[[name]], paste0("./text/t", names(split_text[name]), ".rds"))
})

# remove objects from workspace
rm(list = ls())
