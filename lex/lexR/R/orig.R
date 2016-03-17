#' Basic node word statistics
#'
#' Calculates node statistics basing on corpus position vector
#' @param corp_pos Vector of node word positions
#' @param corp_size Number of tokens in corpus
#' @return List containing node statistics (raw frequency, relative frequency, frequency per 1k and 1M tokens)
#' @export
node_stats <- function (corp_pos, corp_size) {
  freq_raw <- length(corp_pos)
  freq_rel <- freq_raw / corp_size
  freq1000 = freq_raw / 1000
  freq1000000 = freq_raw / 1000000
  return(
    list(
      freq_raw = freq_raw,
      freq_rel = freq_rel,
      freq1000 = freq1000,
      freq1000000 = freq1000000
    )
  )
}

#' Basic word form statistics
#' 
#' Calculates wordform distribution for selected node
#' @param string String CORPUS_NAME.attribute
#' @param corp_pos Vector of node word positions
#' @param corp_size Number of tokens in corpus
#' @return List with 2 dfs: 1) raw data in forms_cpos: pos | form | cposbin 2) summarized data in forms_cpos_sum: cposbin | count
#' @examples
#' forms_stats("PATROLOGIA.word", c(123,236,649), 102000000)
#' @export
forms_stats <- function (string, corp_pos, corp_size) {
  # Retrieve and clean list of forms
  forms <- as.factor(rcqp::cqi_cpos2id(string, corp_pos)) # Corpus pos -> Wordform id
  forms_str <- cqi_id2str(string, levels(forms)) # Wordform id -> Wordform string
  forms_str_low <- tolower(forms_str)
  levels(forms) <- forms_str_low
  
  # Create df: position | wordform | position bin
  forms_cpos <- data.frame(pos = corp_pos, form = forms)
  forms_cpos$cposbin <- cut_corpus(corp_pos = corp_pos, corp_size = corp_size)
  
   # forms_cpos$cposbin <- as.integer(as.character(cut(
   #   as.numeric(corp_pos),
   #   breaks = c(seq(
   #     0, to = as.numeric(corp_size + 100000), by = 1000000
   #   ), corp_size),
   #   labels = c(seq(
   #     1000000, to = corp_size + 100000, by = 1000000
   #   ), corp_size)
   # )))
  
  forms_cpos_sum <- select(forms_cpos, form, cposbin)  %>%
    group_by(., cposbin)  %>%
    summarise(., count = n())
  
  return(list(forms_cpos = forms_cpos,
              forms_cpos_sum = forms_cpos_sum))
}