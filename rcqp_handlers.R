liber1 <- cqi_query("PATROLOGIA_BB","Liber", query="[lemma=\"liber1\"] :: match.text_vol = \"09[7-9]|1[0-3][0-9]\"")
liber1_sub <- cqi_dump_subcorpus("PATROLOGIA_BB:Liber")
window=5

collocs_lemma <- sapply(-window:window,function(x) cqi_cpos2str(attribute = "PATROLOGIA_BB.lemma", cpos =liber1[,1]+x) )
collocs_word <- sapply(-window:window,function(x) cqi_cpos2str(attribute = "PATROLOGIA_BB.word", cpos =liber1[,1]+x) )
collocs_pos <- sapply(-window:window,function(x) cqi_cpos2str(attribute = "PATROLOGIA_BB.pos", cpos =liber1[,1]+x) )
collocs_text_id <- sapply(-window:window,function(x) cqi_struc2str(attribute = "PATROLOGIA_BB.text_id", cqi_cpos2struc(attribute = "PATROLOGIA_BB.text_id", cpos =liber1[,1]+x) ))

collocs <- matrix(nrow=nrow(liber1),ncol = (2 * window +1)*4)

collocs[,seq(1,44,4)] <- collocs_lemma[,1:11]
collocs[,seq(2,44,4)] <- collocs_word[,1:11]
collocs[,seq(3,44,4)] <- collocs_pos[,1:11]
collocs[,seq(4,44,4)] <- collocs_text_id[,1:11]

col_names_L <- unlist(lapply(window:1,
                           function(x) sapply(c("lemma","word","pos","text_id"), 
                                              function(y) paste(x,"L_",y,sep = "")) ) )
col_names_node <- c("lemma","word","pos","text_id")
col_names_R <- unlist(lapply(1:window,
                             function(x) sapply(c("lemma","word","pos","text_id"), 
                                                function(y) paste(x,"R_",y,sep = "")) ) )
col_names <- c(col_names_L,col_names_node,col_names_R)
collocs <- as.data.frame(collocs)
colnames(collocs) <- col_names
