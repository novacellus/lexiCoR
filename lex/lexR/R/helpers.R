cut_corpus <- function(corp_pos, corp_size) 
{
  how_many_breaks <- 100
  thresh <- seq( 1,corp_size,corp_size / how_many_breaks ) %>%
    c(., corp_size)
  breaks <- cut(thresh,how_many_breaks, right = T)
  corp_pos_binned <- cut(x = corp_pos, breaks = how_many_breaks, labels = 1:how_many_breaks)
}

#' Joins corpus name with positional attribute for further searhc
corpus_string <- function (corpus, attr="lemma") {
  paste(corpus, ".", attr , sep="")
}

#' Find word in a ranking list
#' @param rank_table Ranking table
#' @param id Word or Lemma id
