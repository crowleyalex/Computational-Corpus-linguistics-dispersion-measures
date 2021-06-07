# To compute tf-idf weights for words, we need to calculate two weights - tf and idf
# tf = frequency of target word in document / total words in document
# idf = log_e(total number of documents / number of documents with term in)

# Load in libraries 
library(tidyverse)
library('readr')

options(max.print = 100000)

# Get list of corpus file names, scan in all files, remove newlines 
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


# Make a document feature matrix - rows are the documents and columns are terms in the corpus
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

# Take the dtm values and use to compute term frequency 
term_freq <- function(doc_matrix){
  doc_length <- rowSums(doc_matrix)
  tf_matrix <- sweep(doc_matrix, 1, doc_length, '/' )
  
  print(tf_matrix)
  return(tf_matrix)
  
}

tf <- term_freq(dtm)


# Create an inverse document frequency matrix
inverse_document_frequency <- function(doc_matrix){
  total_docs <- nrow(doc_matrix)
  num_docs_t <- colSums(doc_matrix != 0)
  idf <- log(total_docs / num_docs_t)
  
  print(idf)
  return(idf)
}

idf <- inverse_document_frequency(dtm)


# Make a tf-idf function to apply weights to document feature matrix
tf_idf <- function(tf, idf){
  tf_idf_matrix <- sweep(tf, 2, idf, '*')
  print(as.data.frame(tf_idf_matrix))
  return(tf_idf_matrix)
}

tf_idf_values <- tf_idf(tf, idf)

write.csv(tf_idf_values, 
          file = '/Users/Alex/Documents/MA Linguistics /Computational Corpus/tf-idf-matrix.csv')

