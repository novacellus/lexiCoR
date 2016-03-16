#' Plots ranking
#' @param ranks Vector of ranks
#' @param freqs Vector of frequencies
#' @param ids Vector of ids (not obligatory)
#' @param emph_id Id of word/lemma to emphasize (not obligatory)
# plot_corpus_rank <- function(ranks, freqs, ids, emph_id, string) {
#   if (!is.null(ids) && !is.null(emph_id)) {
#     emph_index <- ranks[ids == emph_id] # Index of a data point we'd liek to emphasize
#     plot(x = ranks, y = freqs, 
#          pch = ifelse(ranks == ranks[emph_index], 19, 1),
#          cex = ifelse(ranks == ranks[emph_index], 2, 1),
#          col = ifelse(ranks == ranks[emph_index], "red", "black"))
#     arrows(x1=ranks[emph_index], y1=freqs[emph_index], x0 = 5000, y0 = 2000000, col = "red")
#     text(x = 5000, y = 2000000, labels = string[emph_index], offset = 20)
#   }
# }

#' Plots ranking with ggplot
#' @param ranks Vector of ranks
#' @param freqs Vector of frequencies
#' @param ids Vector of ids (not obligatory)
#' @param emph_id Id of word/lemma to emphasize (not obligatory)
plot_corpus_rank <- function(rank_table, emph_id, string) {
  if (!is.null(rank_table$ids) && !is.null(emph_id)) {
    # Index of a data point we'd like to emphasize
    emph_index <- rank_table$ranks[rank_table$ids == emph_id]
    
    plot <- ggplot(rank_table, aes(x = rank_table$ranks, y = rank_table$freqs)) + 
      geom_point(color = ifelse(rank_table$ranks == rank_table$ranks[emph_index], "red", "black"), 
                 size = ifelse(rank_table$ranks == rank_table$ranks[emph_index], 5, 1)) +
      annotate("text", x = rank_table$ranks[emph_index]+100,
               y = rank_table$freqs[emph_index] + 500000,
               label = string)
    
    return(plot)
  }
}

#' Plots log-ranking with ggplot
#' @param ranks Vector of ranks
#' @param freqs Vector of frequencies
#' @param ids Vector of ids (not obligatory)
#' @param emph_id Id of word/lemma to emphasize (not obligatory)
plot_corpus_rank_log <- function(rank_table, emph_id, string) {
  if (!is.null(rank_table$ids) && !is.null(emph_id)) {
    # Highlighted Data point properties
    emph_index <- rank_table$ranks[rank_table$ids == emph_id] #Index
    # ranking
    # frequency
    
    
    plot <- ggplot(rank_table, aes(x = log(rank_table$ranks), y = log(rank_table$freqs))) +
      geom_point(color = ifelse(rank_table$ranks == rank_table$ranks[emph_index], "red", "black"), 
                  size = ifelse(rank_table$ranks == rank_table$ranks[emph_index], 5, 1) ) + 
      annotate("text", x = log(rank_table$ranks[emph_index]+1,), y = log(rank_table$freqs[emph_index]) + 1, label = string)
    
    return(plot)
  }
}