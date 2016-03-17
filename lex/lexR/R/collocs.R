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
  
  colloc_list <- list()  #Prepare container
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