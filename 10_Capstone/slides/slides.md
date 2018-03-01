JHU Data Science Specialization Capstone: Next Word Prediction
========================================================
author: Sean Angiolillo
date: 18 Feb 2018
autosize: true
transition: fade

Project Overview
========================================================



The capstone project for the JHU Data Science Specialization on [Coursera]("https://www.coursera.org/specializations/jhu-data-science") calls for a next word prediction Shiny app. As laid out in my [milestone report]("https://rpubs.com/seanangio/milestone"), I broke the assignment into the following four R scripts and the Shiny app itself.

- Create smaller files from large text corpus to better handle memory restrictions
- Create ngrams (bigrams and trigrams) after cleaning each file of text
- Create a data table with up to 3 predictions for each ngram
- Create functions to process user input and query data table
- Shiny App to take user input and output predictions

Sample Rows of Final Data Table
========================================================


|base         |pred                       |
|:------------|:--------------------------|
|a barrier    |to                         |
|a bartab     |drink                      |
|a bartender  |at and                     |
|a base       |of salary hit              |
|a baseball   |cap bat game               |
|a bases      |loaded                     |
|a basic      |level understanding income |
|a basis      |for in                     |
|a basket     |of with and                |
|a basketball |game team player           |

***

- Starting with a 15% sample of the original data, the final data table had 369,575 unique ngrams.

- Predictions saved in one character vector and then split as needed if queried in the app.


Algorithm Summary
========================================================

The app incorporates a very simple backoff style algorithm. In a table of bigrams and trigrams, the algorithm first processes the user input to accept at most two words. 

Once this text has been cleaned in the same manner as the data, the algorithm searches the data table for its accompanying predictions. 

If it is not found, it will attempt to locate only the last word given. 

If that too is not found, it simply gives the result of the most common unigrams.

Try it Out!
========================================================

Try the app for yourself at the link below:
https://seanangio.shinyapps.io/next_word_app/

The code can be found on [Github]().