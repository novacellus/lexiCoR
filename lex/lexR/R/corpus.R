# Corpus Statistics

#' Number of tokens in corpus (default: words)
#' 
#' Wrapper function around rcqp::cqi_attribute_size
#' @param corpus Corpus name
#' @param what Positional attribute defaulting to token (default "word")
#' @export
corp_info <- function (corpus,what="word"){
  list(
    N = rcqp::cqi_attribute_size(paste(corpus,".",what,sep=""))
  )
}

#' Corpus Word Frequency List
#' 
#' Creates ranking of words
#' @param corpus Corpus name
corp_lemma_rank <- function(corpus, what="lemma") {
  attribute_to_be_searched <- corpus_string(corpus,attr=what)
  number_of_separate_lemmas <- cqi_lexicon_size(attribute = attribute_to_be_searched)
  ids <- seq(0,number_of_separate_lemmas-1,1)
  freqs <- rcqp::cqi_id2freq(attribute = attribute_to_be_searched, ids = ids)
  strings <- rcqp::cqi_id2str(attribute = attribute_to_be_searched, ids = ids)
  rank_table <- data.frame(ids = ids, freqs = freqs, lemmas = strings) %>%
    dplyr::arrange(., desc(freqs)) %>%
    dplyr::mutate(., ranks = rank(-freqs, ties.method = "min"))
  return(rank_table)
}