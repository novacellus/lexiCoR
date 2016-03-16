#Parameters
mode = 'stat'
static <- list(corpus="PATROLOGIA", node="tempus")
library(rcqp)
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(corrplot)
#corpora <- rcqp::cqi_list_corpora()
my_corpus <- {if (mode=="stat") {
    static$corpus
  } else (
  readline(prompt = "Corpus name:")
  )
}
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
  N = rcqp::cqi_attribute_size(paste(mycorp_strings$name,".","word",sep=""))
)
node <- list(
  string = {
    if (mode=="stat") {
      static$node
    } else (
    readline(prompt = "What string are you looking for?\n")
    )
  }
  ,
  attr =
  {
    if (mode=="stat") {
      "lemma"
    } else (
      readline(prompt = "What attr are you looking for?\n")
    )
  }
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

#Statystyka form
forms <- as.factor(cqi_cpos2id(mycorp_strings$word,node$corpPos))
# Formy gramatyczne
forms_str <- cqi_id2str(mycorp_strings$word, levels(forms))
forms_str_low <- tolower(forms_str)
levels(forms) <- forms_str_low
forms_tab <- sort(table(forms),decreasing = T)
barplot(forms_tab,names.arg = levels(forms),horiz = F)

forms_cpos <- data.frame(pos=node$corpPos,form=forms)
forms_cpos$cposbin <- as.integer(as.character(cut(forms_cpos$pos,breaks = c(seq(0,to=mycorp_stats$N+100000,by = 1000000),103000000) , labels = c(seq(1000000,to=mycorp_stats$N+100000,by = 1000000),103000000) ) ) )
forms_cpos_sum <- select(forms_cpos, form, cposbin)  %>% group_by(.,cposbin)  %>% summarise(., count=n())


# Graphs

## Faceted
#forms_plot + stat_count() + facet_grid(form ~ .)
#forms_plot + geom_density(stat = "count")

forms_plot <- ggplot(forms_cpos, aes(x=cposbin, colour=form))
formsplot1 <- forms_plot + geom_area(stat = "count",mapping = aes(fill=form),alpha=0.2) + theme_light()
formsplot1

forms_plot2 <- forms_plot + geom_line(stat = "count") + theme_light()
forms_plot2
forms_plot3 <- forms_plot2 + theme_light() +
  xlab("Corpus position") + 
  geom_line(data = forms_cpos_sum,
            aes(x=as.integer(cposbin),y=count),
            colour="black", alpha=.5, stat="identity") + scale_fill_manual(colour="black")
forms_plot3

# Scatterplot: frequency of form vs. frequency of lemma
forms_cpos_groupped1 <- select(forms_cpos,c(form,cposbin)) %>% group_by(., form,cposbin) %>% summarise(., count=n())
forms_cpos_groupped <- select(forms_cpos,c(form,cposbin)) %>% group_by(., form,cposbin) %>% summarise(., count=n()) %>% tidyr::spread(.,cposbin,count,fill = 0)

cpos_forms <- as.data.frame(t(forms_cpos_groupped[,-1])) %>%  `colnames<-` (unlist(forms_cpos_groupped[,1]))
#Scatterplot
cpos_forms$total <- apply(cpos_forms,1,sum)
cpos_forms$bin <- as.numeric(rownames(cpos_forms))
#Scatterplot: forms ~ total frequency
pairs(data = select(cpos_forms,-bin), total ~ . )
cpos_forms_melted <- gather(cpos_forms,key = form,value = count,-bin)
# Scatterplot with linear regression line: all in one
a <- ggplot(data = filter(cpos_forms_melted,form != "total"), mapping=aes(x= rep(filter(cpos_forms_melted,form == "total")$count, 10),y=count , colour=form) ) +
  xlab("Lemma count") + ylab("Wordform count")
a + geom_point() +
  geom_smooth()
# Scatterplot with linear regression line: faceted
a +
  geom_point() +
  geom_smooth() +
  facet_wrap(facets = ~ form,ncol=3) + 
  xlab("Lemma count") + ylab("Wordform count")
apply(cpos_forms[,1:10],2,function(x) cor(x,cpos_forms$total))

# ECDF plot
ggplot(data = cpos_forms_melted, mapping=aes(x= count, colour=form) ) + stat_ecdf()

# dendrogram (wątpliwa użyteczność)
dend <- forms_cpos_groupped[,-1]
rownames(dend) <- unlist(forms_cpos_groupped[,1])
dend <- dend %>%  scale(.) %>% dist(.) %>% hclust()
plot(dend,main = "Word Form Groups according to frequencies")

# Heat map
corrplot(cor(select(cpos_forms,-bin)),method = "shade",addCoef.col = "black")

#Boxplot
a + geom_boxplot()

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
meta_plot_auth_text

# Density: by type
meta_plot_periode <- ggplot(meta_all,mapping=aes(x=ratio)) + 
  geom_line(stat="density",aes(colour=periode)) + xlab("Frequency in million per words")


# Density: by periode
meta_plot_type <- ggplot(meta_all,mapping=aes(x=ratio)) + 
  geom_line(stat="density",aes(colour=type)) + xlab("Frequency in million per words")

library(gridExtra)
grid.arrange(meta_plot_type,meta_plot_periode,meta_plot_auth_text)

# Lemma vs. corpus properties
types <- data.frame(id=0:cqi_lexicon_size("PATROLOGIA.lemma"),lemma=cqi_id2str("PATROLOGIA.lemma",0:cqi_lexicon_size("PATROLOGIA.lemma")),count=cqi_id2freq("PATROLOGIA.lemma",0:cqi_lexicon_size("PATROLOGIA.lemma"))) %>% 
  arrange(.,desc(count))
types_group <- types %>% 
  group_by(., count) %>% 
  summarise(., times=n() ) %>% 
  arrange(.,desc(times)) %>% 
  mutate(.,rank=rank(-times))
ggplot(types_group,aes(x=rank,y=times)) + geom_point()
  