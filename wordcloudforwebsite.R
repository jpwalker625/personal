library(wordcloud2)
library(tm)
library(Hmisc)
library(tidyverse)

words <- c("Analytics", "Coding", "R", "Welcome","Programming", "Tutorials", "Data Science", "Machine Learning", "Algorithms", 
           "Wrangling", "Statistics", "SQL", "Web Development", "Visualization", "Python",  "Modeling", "Text Mining", "C++", "F#", "Binary", "Bit", 
           "Byte", "Block", "Chunk", "Data", "Blog", "Bug", "Function", "Conditional", "Computation", "Random Forest", "Boosting", "Bagging", "Trees", 
           "Classification", "Regression", "Markdown", "Inference", "For Loop", "Data Frame", "Tidyverse", "Github", "GGPLOT2", "Supervised Learning", 
           "Unsupervised Learning", "Text", "Mining", "Sampling", "Import", "List", "Numeric", "Logical", "Character", "Beta", "String", "Vector", 
           "Integer", "NA", "NULL", "NaN", "Time-Series", "Pandas", "Datatable", "Cluster", "KNN", "Dplyr", "Matplotlib", "Forcats", "Stringr", 
           "Hello World!", "Print", "Cat", "Plot", "Histogram", "Matrix", "Array", "Naive Bayes", "Decision Tree", "Probability","Tuning", "Caret", 
           "HTML", "JavaScript", "CSS", "Expression", "To", "Bootstrap", "Cross-Validation")

rep <- words

all_words <- c(words, rep)

freq <- c(4, 7, 7, 26, 3, 9, 4, 7, 7, 3, 4, 9, 7, 4, 3, 4, 4, 3, 4, 9, 7, 3, 
          7, 4, 66, 7, 4, 3, 4, 3, 7, 4, 3, 4, 4, 4, 9, 4, 4, 7, 4, 4, 9, 9, 
          9, 4, 3, 9, 8, 4, 7, 4, 9, 36, 4, 3, 4, 4, 9, 4, 3, 3, 3, 4, 7, 3, 4, 
          3, 4, 3, 7, 9, 4, 3, 4, 3, 4, 7, 3, 4, 3, 4, 4, 4, 3, 12, 4, 4, 3, rep.int(3, 87))

words_df <- data.frame(words = all_words, freq = freq) 

words_df <- words_df %>%
  arrange(desc(freq))


colors <- if_else(words_df$words %in% c("Data", "Beta", "Welcome", "To"), true =  "#3d5d86", false = "#2C2C2C")

weight <- if_else(words_df$words %in% c("Data", "Beta", "Welcome", "To"), true = "Bold", false = "normal")

wordcloud2(data = words_df, fontWeight = 600, color = colors, fontFamily = "Raleway",   maxRotation = -pi/5, rotateRatio = .4)

wordcloud2(data=words_df, fontFamily = "Courier New", fontWeight = 600, color = colors, ellipticity = .15, size = .88, shuffle = FALSE) 

wordcloud2(data=words_df, fontFamily = "Courier New", fontWeight = 600, color = colors)
