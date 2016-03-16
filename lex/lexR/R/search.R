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
find_word_id <- function(corpus,attr="word",what) {
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
find_word_pos <-function(corpus, attr="lemma", what.id){
  corpus.attr <-paste(corpus, ".", attr, sep="")
  dump_node <-rcqp::cqi_id2cpos(attribute = corpus.attr, id = what.id)
  return(dump_node)
}
