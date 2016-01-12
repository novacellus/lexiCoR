library(rcqp)
corpora <- rcqp::cqi_list_corpora()
my_corpus <- readline(prompt = "Corpus name:\n")
mycorp_strings=list(
  name=unlist(my_corpus),
  lemma=paste(my_corpus,".lemma",sep = ""),
  word=paste(my_corpus,".word",sep = ""),
  pos=paste(my_corpus,".pos",sep = "")
)
mycorp_freq=list(
  lemma=rcqp::cqi_lexicon_size(as.character(mycorp_strings["lemma"])),
  word=rcqp::cqi_lexicon_size(as.character(mycorp_strings["word"])),
  pos=rcqp::cqi_lexicon_size(as.character(mycorp_strings["pos"]))
)
mycorp_stats = list(
  N = rcqp::cqi_lexicon_size(paste(mycorp_strings$name,".","word",sep=""))
)
node <- list(
  string=readline(prompt = "What string are you looking for?\n"),
  attr=readline(prompt = "What attr are you looking for?\n")
)
#"[word=\"tempus\"]"
#node["query"]=paste("[",node["attr"],"=","\"",node["string"],"\"","]",sep="")

#' Checks if word exists in the corpus
#' @return If word exists in the corpus, returns its id; if not, returns useful errors
find_word <- function(corpus=mycorp_strings$name,attr=node$attr,what=node$string) {
  corpus_attr <-paste(corpus, ".", attr , sep="")
  node.string <- rcqp::cqi_str2id(corpus_attr, what)
  return(node.string)
}
node["id"] <- find_word()

#' Find all occurrences of word by its id
#' @return Vector of word corpus positions
find_words_cpos <-function(corpus=mycorp_strings$name,attr=node$attr,what.id=node$id){
  corpus_attr <-paste(corpus, ".",attr, sep="")
  dump.pivot <-rcqp::cqi_id2cpos(attribute = corpus_attr,id = what.id)
  return(dump.pivot)
}
node["corpPos"] <- list(find_words_cpos())
node_stats <- function() {
  freq_raw<-length(node$corpPos)
  freq_rel<-freq_raw/mycorp_stats$N
  freq1000=freq_raw/1000
  freq1000000=freq_raw/1000000
  return(list(freq_raw=freq_raw,freq_rel=freq_rel,freq1000=freq1000,freq1000000=freq1000000))
}
node_stats()