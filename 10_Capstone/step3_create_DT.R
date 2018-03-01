# Step 3: Create final app data table
# construct a single data.table from list of ngram files
# output final app data after aggregating counts and subsetting top results

library(data.table)
library(stringr)

# get list of ngram files
file_names <- list.files("./ngrams", full.names = TRUE)

# initialize an empty data table
DT <- data.table(ngram = character())

# successively add each file in list to create one data table of ngrams
system.time(for (file in file_names) {
    newDT <- data.table(ngram = readRDS(file))
    DT <- rbindlist(list(DT, newDT))
    rm(newDT)
})

# add count frequency and filter by minimum appearance threshold
threshold <- 2
DT <- DT[,.N, by = ngram][N >= threshold]

# separate string into base and prediction by reference; remove original column
system.time(DT[, `:=`(base = word(ngram, start = 1, end = -2, sep = " "),
          pred = word(ngram, start = -1, sep = " "),
          ngram = NULL)])

# subset table to get top 3 predictions for each base
DT <- setorder(DT, base, -N)[, indx := seq_len(.N), by = base][indx <= 3]

# remove no-longer needed columns N and indx
DT[, N := NULL]
DT[, indx := NULL]

# aggregate predictions for each ngram
DT <- data.table(aggregate(pred ~ base, data = DT, paste, collapse = " "))

# add entry to DT for base == NA based on top 3 unigrams
unigram_DT <- data.table(base = NA, pred = c("the to and"))
DT <- rbindlist(list(unigram_DT, DT))

# output the final data.table as an rds file
saveRDS(DT, "final_app_data.rds")
rm(list = ls())
