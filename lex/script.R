#Parameters
mode = 'dynamic'
library(rcqp)
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(corrplot)

#' Find a word in a given corpus
#' 
#' @param corpus Corpus name
#' @param attr Positional attribute (lemma, word, pos etc.). Default: lemma
#' @param what Searched string
#' @examples 
#' find_word('PATROLOGIA', 'lemma', 'tempus')
#' @return 
find_word <- function(corpus,attr,what) {
  corpus_attr <- paste(corpus, ".", attr , sep="")
  node.string <- rcqp::cqi_str2id(corpus_attr, what)
  return(node.string)
}

find_words_cpos <-function(corpus,attr,what.id){
  corpus_attr <-paste(corpus, ".",attr, sep="")
  dump.pivot <-rcqp::cqi_id2cpos(attribute = corpus_attr,id = what.id)
  return(dump.pivot)
}

corp_info <- function (corpus,what="word"){
  list(
    N = rcqp::cqi_attribute_size(paste(corpus,".",what,sep=""))
  )
}

node_stats <- function(corp_pos=node$corpPos,corp_size=corp$N) {
  freq_raw<-length(corp_pos)
  freq_rel<-freq_raw/corp_size
  freq1000=freq_raw/1000
  freq1000000=freq_raw/1000000
  return(list(freq_raw=freq_raw,freq_rel=freq_rel,freq1000=freq1000,freq1000000=freq1000000))
}

#Statystyka form
forms_stats <- function (string, corp_pos=node[["corpPos"]],corp_size){
  forms <- as.factor(rcqp::cqi_cpos2id(string,corp_pos))
  # Formy gramatyczne
  forms_str <- cqi_id2str(string, levels(forms))
  forms_str_low <- tolower(forms_str)
  levels(forms) <- forms_str_low
  forms_tab <- sort(table(forms),decreasing = T)
  #barplot(forms_tab,names.arg = levels(forms),horiz = F)
  
  forms_cpos <- data.frame(pos=corp_pos,form=forms)
  
  forms_cpos$cposbin <- as.integer(as.character(
    cut(as.numeric(corp_pos),
        breaks = c(seq(0,to=as.numeric(corp_size+100000),by = 1000000),corp_size),
        labels = c(seq(1000000,to=corp_size+100000,by = 1000000),corp_size)
        )
    ))
  
  forms_cpos_sum <- select(forms_cpos, form, cposbin)  %>% 
    group_by(.,cposbin)  %>%
    summarise(., count=n()) 
  
  return(list(
    forms_cpos=forms_cpos,
    forms_cpos_sum=forms_cpos_sum
  ))
}
plot_forms1 <- function(forms_cpos) {
  forms_plot <- ggplot(forms_cpos, aes(x=cposbin, colour=form))
  formsplot1 <- forms_plot + 
    geom_area(stat = "count",mapping = aes(fill=form),alpha=0.2) + 
    theme_light()
  return(formsplot1 )
}

plot_forms2 <- function(forms_cpos) {
  forms_plot <- ggplot(forms_cpos, aes(x=cposbin, colour=form))
  forms_plot2 <- forms_plot + geom_line(stat = "count") + theme_light()
  forms_plot2
}

plot_forms3 <-function(forms_cpos,forms_cpos_sum) {
  forms_plot <- ggplot(forms_cpos, aes(x=cposbin, colour=form))
  forms_plot2 <- forms_plot + geom_line(stat = "count") + theme_light()
  forms_plot3 <- forms_plot2 + theme_light() +
    xlab("Corpus position") + 
    geom_line(data = forms_cpos_sum,
              aes(x=as.integer(cposbin),y=count),
              colour="black", alpha=.5, stat="identity") 
  #+ scale_fill_manual(colour="black")
  forms_plot3  
}

# Scatterplot: frequency of form vs. frequency of lemma
plot_forms_scatter <- function(forms_cpos) {
  forms_cpos <- forms_cpos
  
  forms_cpos_groupped1 <- select(forms_cpos,c(form,cposbin)) %>% 
    group_by(., form,cposbin) %>% 
    summarise(., count=n())
  
  forms_cpos_groupped <- select(forms_cpos,c(form,cposbin)) %>% 
    group_by(., form,cposbin) %>% 
    summarise(., count=n()) %>% tidyr::spread(.,cposbin,count,fill = 0)
  
  cpos_forms <- as.data.frame(t(forms_cpos_groupped[,-1])) %>%
    `colnames<-` (unlist(forms_cpos_groupped[,1]))
  
  number_of_forms <- nrow(forms_cpos_groupped)
  
  #Scatterplot
  cpos_forms$total <- apply(cpos_forms,1,sum)
  cpos_forms$bin <- as.numeric(rownames(cpos_forms))
  #Scatterplot: forms ~ total frequency
  pairs_plot <- pairs(data = select(cpos_forms,-bin), total ~ . )
  cpos_forms_melted <- gather(cpos_forms,key = form,value = count,-bin)
  
  base_plot <- ggplot(data = filter(cpos_forms_melted,form != "total"), mapping=aes(x= rep(filter(cpos_forms_melted,form == "total")$count, number_of_forms),y=count , colour=form) ) +
    xlab("Lemma count") + ylab("Wordform count")
  
  # Scatterplot with linear regression line: all in one
  scatt1 <- base_plot + geom_point() +  geom_smooth()
  # Scatterplot with linear regression line: faceted
  scatt2 <- base_plot + geom_point() + geom_smooth() + 
    facet_wrap(facets = ~ form,ncol=3) + xlab("Lemma count") + ylab("Wordform count")
  
  #Boxplot
  boxpl <- base_plot + geom_boxplot()
  
  
  #apply(cpos_forms[,1:10],2,function(x) cor(x,cpos_forms$total))
  
  # ECDF plot
  ecdf <- ggplot(data = cpos_forms_melted, mapping=aes(x= count, colour=form) ) + stat_ecdf()
  # Heat map
  corrplot <-
    corrplot(cor(select(cpos_forms,-bin)),method = "shade",addCoef.col = "black")
  
  
  return(list(
    test=cpos_forms_melted,
    pairs = pairs_plot,
    base_plot = base_plot,
    scatt1=scatt1,
    scatt2 = scatt2,
    ecdf = ecdf,
    corrplot = corrplot,
    boxpl = boxpl
    
  ))
}

meta_plot <- function (corpus="PATROLOGIA",forms_cpos) {
  # Per text
  meta_all <- forms_cpos %>% 
    mutate(.,struc=cqi_cpos2struc("PATROLOGIA.text_titre",pos)) %>%
    group_by(.,struc) %>% summarise(.,count=n()) %>% 
    mutate(.,author=cqi_struc2str("PATROLOGIA.text_auteur",struc)) %>% 
    mutate(.,title=cqi_struc2str("PATROLOGIA.text_titre",struc)) %>% 
    mutate(.,id=cqi_struc2str("PATROLOGIA.text_id",struc)) %>% 
    mutate(.,type=cqi_struc2str("PATROLOGIA.text_type",struc)) %>%
    mutate(.,periode=cqi_struc2str("PATROLOGIA.text_periode",struc)) %>%
    mutate(.,N=sapply(struc,function(x) cqi_struc2cpos("PATROLOGIA.text_id",x )[2]-cqi_struc2cpos("PATROLOGIA.text_id", x )[1]+1) ) %>% 
    mutate(.,ratio=(count/N)*1000000) # Per million words
  #Per author
  meta_author <- meta_all %>% group_by(.,author) %>% summarise(.,ratio=(sum(count)/sum(N))*1000000 )
  meta_type <- meta_all %>% group_by(.,type) %>% summarise(.,ratio=(sum(count)/sum(N))*1000000 )
  meta_periode <- meta_all %>% group_by(.,periode) %>% summarise(.,ratio=(sum(count)/sum(N))*1000000 )
  
  # Probability of frequency
  meta_plot_auth_text <- ggplot(meta_all,mapping=aes(x=ratio)) + 
    geom_line(stat="density",aes(colour="texts")) + xlab("Frequency in million per words") +
    geom_line(stat="density",data=meta_author,mapping=aes(x=ratio,colour="authors")) +
    scale_colour_manual("Density in",breaks=c("texts","authors"),values=c("red","blue"))
  
  return(list(
    meta_plot = meta_plot_auth_text
  ))
}