#Gęstość frekwencji w poszczególnych częściach korpusu
#par(mfrow=c(1,2))
# Histogram poszczególnych frekwencji
par(mfrow=c(1,2))
hist(node$corpPos, col="gray",probability = T, plot = T)
lines(cumsum(histo$counts), col="yellow")
histo <- hist(node$corpPos, plot = F)
histo_cum <- c(0,cumsum(histo$counts))
# Krzywa gęstości
lines(density(node$corpPos,cut = F),col="blue", lwd=2)
lines(density(node$corpPos,cut=F, adjust = 3),col="blue", lwd=2, lty="dotted")
#Znormalizowana do histogramu suma kumulatywna
lines(x=histo$mids,
      y=rescale(x = histo_cum[-1],from=range(histo_cum[-1]),to=range(histo$density)),
      col="darkgreen",lwd=2)
#Skąd różnica między kumulatywną sumą a histogramem
# ECDF
plot(ecdf(node$corpPos),col="red",lwd=2,add=T)
# Róźnice między kolejnymi skokami
plot(x=2:22,y=lapply(seq(2:length(histo_cum)),FUN = function(x) histo_cum[x]-histo_cum[x-1] ) , type="l")
# Boxplot poszczególnych zliczeń
boxplot(histo$counts,outline = T)
