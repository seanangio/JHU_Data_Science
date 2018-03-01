# Step 4 Query Data Table
# build two functions needed for the shiny app
# first to process user input so it can be compared to bases of final_app_data
# second to query final_app_data using simple backoff approach
# algorithm first looks in table for matching bigram and delivers top predictions if found
# if bigram can't be found, search is performed on user's last word
# if also not found, algorithm returns most common words in corpus

library(quanteda)
library(stringr)

# load data for the app
app_data <- readRDS("final_app_data.rds")

# reload profanity
if (!exists("profanity")) {
    profanity <- readLines("./profanity/full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt", skipNul = TRUE, warn = FALSE)
    profanity <- iconv(profanity, "latin1", "ASCII", sub = "")
}

# function to clean user input
process_input <- function(user_input) {
    # remove foreign characters
    user_input <- iconv(user_input, "latin1", "ASCII", sub = "")
   
    # lowercase and remove special characters
    user_input <- tokens(char_tolower(user_input, keep_acronyms = TRUE), what = "word", 
                   remove_numbers = TRUE, remove_punct = TRUE,
                   remove_symbols = TRUE, remove_hyphens = TRUE,
                   remove_twitter = TRUE, remove_url = TRUE,
                   ngrams = 1L)
    
    # remove profanity
    user_input <- tokens_remove(user_input, profanity)

    # get last two words if necessary
    if (length(user_input[[1]]) > 2) { user_input <- tail(user_input[[1]], 2)}
    
    # collapse string
    user_input <- paste0(user_input, collapse = " ")
    user_input
}

# function to query data table
query_dt <- function(input) {
    # grab prediction for given base input
    prediction <- app_data[base == input, pred]
    
    if (length(prediction) == 0) {
        # that means bigram base wasn't found so extract last word and query again
        shortInput <- word(input, start = -1)
        prediction <- app_data[base == shortInput, pred]
        if (length(prediction) == 0) {
            # it's not found anywhere so just give predictions 
            prediction <- c("the to and")
        }
    } 
    print(prediction)
}
