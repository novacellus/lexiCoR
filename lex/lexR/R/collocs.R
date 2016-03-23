#' Ids and cpos of coocurrents
#' Creates a table of coocurring words' ids and cpos
#' @return List of colloc_pos and colloc ids
#' @param node_cpos Vector containing word's corpus positions
#' @param window Collocational window
create_colloc_tbl <- function(node_cpos, window=5) {
  heigth <- length(node_cpos)
  width <- window * 2 + 1
  sequ <- seq(from = -window,to = window, by = 1)
  # Calculate positions of coocurring words
  colloc_pos <- as.data.frame((sapply(sequ, FUN = function(x) as.character(x + node_cpos))), stringsAsFactors = T)
  colnames(colloc_pos) <- as.character(sequ)
  
  # Retrieve ids of coocurring words
  colloc_ids <- as.data.frame ( apply(colloc_pos, 2, function(x) as.factor(rcqp::cqi_cpos2id("PATROLOGIA.lemma", x) ) ) )
  
  return(list(colloc_pos = colloc_pos, colloc_ids = colloc_ids))
}

#' Create sorted lists of coocurrences in each window
#' @param colloc_ids_tbl Table of ids of coocurrences
#' @param ... if corpus and attr provided a string column will be added
create_colloc_list <- function(colloc_ids_tbl, ...) {
  dots <- list(...)
  
  colloc_list <- NULL  #Prepare container
  
  # For each column create a sorted frequency list
  sapply(names(colloc_ids_tbl), function(x)
      colloc_list[[x]] <<- table( colloc_ids_tbl[[x]] ) %>%
        as.data.frame(stringsAsFactors=T)  %>%
        magrittr::set_colnames(., c("ids", "freq"))  %>% 
        dplyr::arrange(.,desc(freq)) %>%
        {if (all(c("corpus", "attr") %in% names(dots) ) ) {
          dplyr::mutate (., str = find_ids2strings(ids = ids, corpus = dots$corpus, attr = dots$attr))
        } else .
          }
  )
  
  return (list(colloc_list))
}

#' Id-frequency table
#' @param collocs_ids_tbl Matrix with ids in windows
#' @param extend If TRUE, also E, f1, f2 are calculated
#' @param thresh Filter words with freq less than thresh
colloc_frequency_tbl <- function(collocs_ids_tbl, thresh = 10 , extend = T) {
  collocs_ids_tbl %>% 
    dplyr::select(., -`0`) %>% unlist(.) %>%  
    table %>% as.data.frame(.)  %>%
    magrittr::set_colnames(., c("id", "O")) %>% 
    mutate(., 
           f1 = rcqp::cqi_id2freq("PATROLOGIA.lemma",ids =  as.numeric (as.character(id) ) ), 
           f2 = rep( rcqp::cqi_id2freq("PATROLOGIA.lemma",ids =  as.numeric (as.character(collocs_ids_tbl$`0`[1]))), times = nrow(.) ) )  %>%  
    mutate (., N = rcqp::cqi_attribute_size("PATROLOGIA.lemma"))  %>%  
    mutate (., E = ( as.numeric(f1) * as.numeric(f2) ) / as.numeric(N) )
}


#' Build foundations for contingency table of collocates
#' @return Tabe with 011, 012, E11, E12 variables
# O = f - observed frequency of w1 + w2
# f1, f2 - frequencies of resp. w1, w2 in the whole corpus
# E = expected frequency of a word pair w1w2 = (kf1f2)/N where k - span size, e.g. 1o for 5L, 5R
# N - Number of tokens
calc_freq_signature <- function(t_collocs) {
  colloc_frequency_tbl(collocs_ids_tbl = t_collocs$colloc_ids) %>% 
    mutate(., 
           f1 = rcqp::cqi_id2freq("PATROLOGIA.lemma",ids =  id), 
           f2 = rcqp::cqi_id2freq("PATROLOGIA.lemma",ids =  t_collocs$colloc_ids$`0` ) )  %>%  
    mutate (., N = rcqp::cqi_attribute_size("PATROLOGIA.lemma"))  %>%  
    mutate (., E = ( as.numeric(f1) * as.numeric(f2) ) / as.numeric(N) )
}

#' Calculate coocurrence coefficients
coeffs <- function(O, E, ...) {
  dots <- list(...)
  
  MI <- log2(O / E)
  local_MI <- O * log2(O / E)
  z_score <- (O - E) / sqrt(E)
  t_score <- (O - E) / sqrt(O)
  simple_ll <- 2 * (O * log (O / E) - (O - E) )
  
    
  return(data.frame(MI = MI, local_MI = local_MI, z_score = z_score, t_score = t_score, simple_ll = simple_ll))
}

#' Calculate foundations for statistical association measures (Evert)
coeffs_stat_found <- coeffs <- function(O, E, f1, f2, N) {
  O11 <- O
  O12 <- f1 - O
  O21 <- f2-O
  O22 <- N - f1 - f2 + O
  
  return(data.frame(O11 = O11, O12 = O12, O21 = O21, O22 = O22))
}

coeffs_stat <- function(...) {
  dots <- list(...)
  
  Dice <- (2 * dots[["O11"]] ) / (dots$O11 + dots$O12) + (dots$O11 + dots$O21)
  return(data.frame(Dice = Dice))
}

# 
# 1. Typical session
# 
#t_colloc_tbl <- create_colloc_tbl(find_word_pos("PATROLOGIA", what.id = find_word_id("PATROLOGIA", what="pater")))
#t_colloc_freq <- colloc_frequency_tbl(t_colloc_tbl$colloc_ids)
#t_coeffs <- t_colloc_freq  %>% bind_cols(., coeffs(t_colloc_freq$O, t_colloc_freq$E)) %>% mutate(., lemma = (rcqp::cqi_id2str("PATROLOGIA.lemma", id)))
#t_colloc_stats <-  t_colloc_freq  %>% bind_cols(., with(., coeffs_stat_found(O, E, f1, f2, N)))  %>%  bind_cols(., with(., coeffs_stat(O11 = O11, O12 = O12, O21 = O21))) %>% mutate(., lemma = (rcqp::cqi_id2str("PATROLOGIA.lemma", id)))
#filter(t_colloc_stats, O > 10)  %>%  top_n(., 10, Dice)