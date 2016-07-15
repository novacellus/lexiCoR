# Search and Lookup Functions

#' Find corpus id of the searched word
#' 
#' @param corpus Corpus name
#' @param attr Positional attribute (lemma, word, pos etc.). Default: lemma
#' @param what Searched string
#' @examples 
#' find_word('PATROLOGIA', 'lemma', 'tempus')
#' @return Numeric identifier of the searched string in the selected corpus
#' @export
find_word_id <- function(corpus,attr="lemma",what) {
  corpus_attr <- paste(corpus, ".", attr , sep="")
  node_id <- rcqp::cqi_str2id(corpus_attr, what)
  return(node_id)
}

#' Retrieve positions of the searched word
#' 
#' @param corpus Corpus name
#' @param attr Positional attribute (lemma, word, pos etc.). Default: lemma
#' @param what.id Numeric id to be searched in the corpus (retrieved with find_word_id)
#' @examples 
#' find_word('PATROLOGIA', 'lemma', 232)
#' @return Vector containing positions of the searched string in the selected corpus
#' @export
find_word_pos <-function(corpus, attr="word", what.id){
  corpus.attr <-paste(corpus, ".", attr, sep="")
  dump_node <-rcqp::cqi_id2cpos(attribute = corpus.attr, id = what.id)
  return(dump_node)
}


#' Retrieve corpus strings for provided ids (wrapper)
#' @param ids Vector of ids
#' @param corpus Corpus name
#' @param attr Strings to retrieve
find_ids2strings <- function(ids, corpus, attr = "lemma") {
 corpus.attr <- corpus_string(corpus, attr = attr)
 strings <- rcqp::cqi_id2str(attribute = corpus.attr, as.vector(ids))
 return(strings)
}

#' Word frequency in selected corpus
#' Calculates frequency of a word id in a corpus
#' @param ids Vector of ids for which frequency will be retrieved
word_frequency <- function(corpus, attr = "lemma", ids) {
  rcqp::cqi_id2freq(corpus_string(corpus, attr), ids)
}