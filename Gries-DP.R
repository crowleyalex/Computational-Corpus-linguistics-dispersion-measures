# Script to calculate Gries' DP dispersion measurement. 
# Final output values close to 0 indicate a word is more evenly dispersed

library(tidyverse)
library('readr')

options(max.print = 100000)

corpus_filenames <- list.files(pattern = '[[:alnum:]].txt')
corpus_data <- lapply(corpus_filenames, read_file)
corpus_data <- lapply(corpus_data, function(x) gsub(pattern = '\n', '', x))


clean_text <- function(corpus_list){
  # Remove punctuation, digits and make all lowercase
  corpus <- lapply(corpus_list, function(y) gsub('[[:punct:]]', ' ', y))
  corpus <- lapply(corpus, function(z) gsub('[[:digit:]]', '', z))
  corpus <- lapply(corpus, function(a) tolower(a))
  
  # Split words into individual strings 
  corpus <- lapply(corpus, function(b) strsplit(b, ' '))
  
  # Remove singular character strings other than 'a' from corpus
  corpus <- rapply(corpus, function(d) gsub('\\b[^\\Wa]\\b', '', d), how = 'list')
  
  # Remove any empty strings left over after cleaning 
  corpus <- rapply(corpus, function(e) e[!is.na(e) & e != ''], how = 'list')
  
  # Convert from list of lists to one list, 
  # where every element is a character vector of words in each article
  corpus <- unlist(corpus, recursive = F)
  
  print(corpus)
  return(corpus)
}

input_corpus <- clean_text(corpus_data)

calc_expected <- function(cleaned_corpus){
  # Calculate length of parts and compute those part lengths as % of corpus
  # This calculation gives us the 'expected' percentages 
  part_sizes <- lapply(cleaned_corpus, length)
  part_sizes <- as.integer(part_sizes)
  
  # Calculate total length of corpus tokens
  # Divide part sizes by total length to get expected segments 
  corpus_length <- Reduce('+', part_sizes)
  expected_percentages <- list(part_sizes / corpus_length)
  
  print(expected_percentages)
  return(expected_percentages)
}

expected_percentages <- calc_expected(input_corpus)

# Make a document feature matrix - rows will be each document in corpus
document_feature_matrix <- function(cleaned_corpus){
  all_documents <- c()
  
  # The input is currently a list with character vectors
  # Loop through list and append to vector above 
  for(doc in cleaned_corpus){
    documents <- (paste(doc, collapse = ' '))
    all_documents <- c(all_documents, documents)
  }
  
  # These two lines split the character vector into a list of words 
  # Then, turns that list of words into a list with counts 
  words <- strsplit(all_documents, split = ' ')
  word_counts <- lapply(words, table)
  
  # Now we need to convert from a list of word_counts to matrix
  # The variable 'dictionary' is a character vector of all unique words 
  dictionary <- unique(unlist(words))
  dictionary <- dictionary[order(dictionary)]
  
  # Make the matrix dimensions
  freq_matrix <- matrix(NA, nrow = length(all_documents), ncol = length(dictionary))
  rownames(freq_matrix) <- paste('Doc', seq_along(all_documents))
  colnames(freq_matrix) <- dictionary
  
  # Loop through each of the documents
  for(doc in 1:length(all_documents)){
    relevant_word_count <- word_counts[[doc]]
    for(word in dictionary){
      if(word %in% names(relevant_word_count)){
        freq_matrix[doc, word] <- relevant_word_count[word]
      } else{
        freq_matrix[doc, word] <- 0
      }
    }
  }
  
  print(as.data.frame(freq_matrix))
  return(freq_matrix)
}

dtm <- document_feature_matrix(input_corpus)


calculate_observed <- function(dtm){
  obv_percents <- t(dtm)/colSums(dtm)
  obv_percents <- t(obv_percents)
  
  print(obv_percents)
  return(obv_percents)
}


observed_percentages <- calculate_observed(dtm)
expected_matrix <- as.matrix(expected_percentages[[1]])


calculate_DP <- function(observed_percentages, expected_matrix){
  abs_matrix <- apply(observed_percentages, 2, function(l) abs(l - expected_matrix))
  sums <- colSums(abs_matrix) 
  
  DP <-  sums / 2
  DP_table <- tibble(Types = names(DP), 
                     DP_score = DP)
  print(DP_table)
  
}

calculate_DP(observed_percentages, expected_matrix)


write.csv(DP_table, 
          file = '/Users/Alex/Documents/MA Linguistics /Computational Corpus/DP_output.csv')

