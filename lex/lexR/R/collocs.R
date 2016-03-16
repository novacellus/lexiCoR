#' Ids of coocurrents
#' Creates a table of coocurring words' ids
#' @param node_cpos Vector containing word's corpus positions
#' @param window Collocational window
create_colloc_tbl <- function(node_cpos, window=5) {
  heigth <- length(node_cpos)
  width <- window * 2 + 1
  sequ <- seq(from = -window,to = window, by = 1)
  colloc_pos_tbl <- as.data.frame ( head(sapply(sequ, FUN = function(x) x + a ) ) ) # Calculates positions of coocurring words
  colloc_tbl <- rcqp::cqi_cpos2id("PATROLOGIA.lemma", cpos = unlist(colloc_tbl))
}