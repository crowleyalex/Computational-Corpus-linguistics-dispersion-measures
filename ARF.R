# ARF - Average Reduced Frequency 

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

join_corpus <- function(corpus){
  # Join corpus so documents are irrelevant
  joined <- sapply(corpus, paste, collapse = ' ')
  joined <- paste(joined, collapse = '')
  joined <- strsplit(joined, ' ')
  
  print(joined)
  return(joined[[1]])
}

joined_corpus <- join_corpus(input_corpus)

get_segments <- function(joined_corpus){
  # For each term in corpus, calculate segments of that term relative to its frequency
  N <- length(joined_corpus)
  frequencies <- unlist(table(joined_corpus))

  frequency_table <- tibble(Types = names(frequencies), 
                            f = as.integer(frequencies), 
                            v = as.integer(N / frequencies))
  
  print(frequency_table)
  return(frequency_table)
}

segment_data <- get_segments(joined_corpus)
segment_data <- segment_data[order(factor(segment_data$Types, 
                                          levels = unique(joined_corpus))),]


# Example to calculate the segments for the word 'a' in the corpus 
calculate_RF <- function(segment_data){
  
  output_table <- tibble(Types = character(),
                         RF = numeric())
  
  for(i in 1:nrow(segment_data)){
    segments_of_term_a <- split(joined_corpus, ceiling(seq_along(joined_corpus)/segment_data$v[i]))

    RF_segs <- lapply(segments_of_term_a, function(o) match(segment_data[i, 'Types'], o))
    RF <- length(RF_segs[!is.na(RF_segs)])
    output_table[i, 'Types']=segment_data[i, 'Types']
    output_table[i, 'RF']=RF
  }
  # print(output_table)
  return(output_table)
}

RF_vals <- calculate_RF(segment_data)

calculate_positions <- function(){
  # Calculate individual positions of terms throughout the corpus
  word_indices <- 1:length(joined_corpus)
  names(word_indices) <- joined_corpus
  
  data_table <- tibble(Position = word_indices, 
                 Words = names(word_indices))
  
  recurrent_positions <- apply(data_table, 1, function(x) filter(data_table, Words %in% x))
  recurrent_positions <- unique(recurrent_positions)
  return(recurrent_positions)
}

token_positions <- calculate_positions()


calculate_distances <- function(){
  # Calculate the distances between terms throughout the corpus
  distances <- lapply(purrr::map(token_positions, 'Position'), function(j) abs(diff(j)))
  distances_frames <- lapply(distances, function(k) tibble(Distance = k))
  distance_1 <- lapply(purrr::map(token_positions, 'Position'), function(h) max(h) - min(h))
  
  complete_distances <- mapply(function(x,y) rbind(x,y), distance_1, distances_frames)
  complete_distances <- lapply(complete_distances, function(y) tibble(Distance = y, Words = names(y)))
  return(complete_distances)
}

complete_distances <- calculate_distances()


sum_of_smaller_dv <- mapply(function(v, distance_tbl) {
  # Calculate the sum of the smaller of d or v for f of t
  d <- distance_tbl$Distance
  sum(pmin(v, d))
}, segment_data$v, complete_distances)

# Finalise and export 
sum_of_smaller_dv <-  as.data.frame(sum_of_smaller_dv)
head(sum_of_smaller_dv)

final_data <- cbind(sum_of_smaller_dv, segment_data[,3] )
final_data <- cbind(final_data, segment_data[,1])
ARF <-  1 / final_data[,2] * final_data[,1]

final_data <- cbind(final_data, ARF)
final_data <- cbind(final_data, segment_data[,2])

percent_change <- ((final_data[,5] - final_data[,4]) / final_data[,5]) * 100
final_data <- cbind(final_data, percent_change)
head(final_data)

write.csv(final_data, 
          file = '/Users/Alex/Documents/MA Linguistics /Computational Corpus/ARF_change.csv')

