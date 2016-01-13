#Gęstość frekwencji w poszczególnych częściach korpusu
hist <- hist(node$corpPos, plot=F)
hist_cum <- cumsum(hist$counts)
plot(hist,main="")
hist(hist_cum,add=TRUE,col="grey")
dens <- density(node$corpPos,cut = F)
lines(dens,col="green")
#lines(dens)
#lines(x=dens$x)
#Kumulatywne
ec <- ecdf(node$corpPos)
plot(ec)

