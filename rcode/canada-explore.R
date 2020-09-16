
if (FALSE) { ## can manually skip

    setwd('..')

    if (FALSE)
        source('rcode/getdata.R')

}

options(width=70)

wc0 <- read.csv('data/confirmed_global.csv')
dim(wc0)
wc0[1:5, 1:6]

date <- as.Date(colnames(wc0)[5:ncol(wc0)], 'X%m.%d.%y')

cc <- wc0[which(wc0$Country.Region=='Canada'), ]
cc[,1]

range(cc.n <- colSums(cc[, 5:ncol(cc)]))

### w based on serial interval ~ Gamma(mean=5, sd=3)
### to compute R_t (reproduction number varying over time)
pw <- pgamma(0:14, shape=(5.8/3.9)^2, scale=3.9^2/5.8)
w <- diff(pw)/sum(diff(pw))

library(emisc) ### from https://github.com/eliaskrainski/emisc

length(c(1, 2, 4:8, 10:11))

par(mfrow=c(4,4), mar=c(3,3,1.5,0.5), mgp=c(2, 0.5, 0), las=1)
for (i in 1:14){
    cas <- cummax(unlist(cc[i, 5:ncol(cc)]))
    ii <- which(cas>0)
    epidplot(date[ii], cas[ii], which=list(2),  main=cc[i, 1])
    abline(h=log(rep(c(1,3), 6)*(10^rep(0:5, each=2)), 10), 
           lty=2, col=gray(0.5, 0.5))
}

length( c(1:2, 4:8, 9:11, 13:14))

png('figures/canada-provinces.png', width=500, height=700)
par(mfrow=c(4,3), mar=c(3,3,1.5,0.5), mgp=c(2, 0.5, 0), las=1)
for (i in c(1:2, 4:8, 9:11, 13:14)) {
##for (i in 1:8){
    cas <- cummax(unlist(cc[i, 5:ncol(cc)]))
    ii <- which(cas>0)
    epidplot(date[ii], cas[ii], which=list(1:2),  main=cc[i, 1])
    ##ep(date[ii], cas[ii], which=1,  main=cc[i, 1])
    abline(h=log(rep(c(1,3), 6)*(10^rep(0:5, each=2)), 10), 
           lty=2, col=gray(0.5, 0.5))
}
dev.off()
if (FALSE)
    system('eog figures/canada-provinces.png &')

par(mfrow=c(3,6), mar=c(3,3,1.5,0.5), mgp=c(2, 0.5, 0), las=1)
for (i in c(1:2, 4:8, 10:11)) {
##for (i in 1:8){
    cas <- cummax(unlist(cc[i, 5:ncol(cc)]))
    ii <- which(cas>0)
    epidplot(date[ii], cas[ii], which=2,  main=cc[i, 1])
    abline(h=log(rep(c(1,3), 6)*(10^rep(0:5, each=2)), 10), 
           lty=2, col=gray(0.5, 0.5))
    epidplot(date[ii], cas[ii], which=4, w=w)
    title(main=cc[i, 1])
}

source('rcode/canada-map.R')

ca.prov@data[, c(1, 3)]
ca.prov$pop19 <- c(
    5120184, 8552362, 39486, 158717, 1181987,
    41293, 1379121, 14745040, 780890, 44982,
    4428247, 520437, 978274)

ca.prov@data$PRENAME[!ca.prov@data$PRENAME %in% cc[,1]]
cc[!cc[,1] %in% ca.prov@data$PRENAME, 1]

grep('Princess', cc[,1], value=TRUE)
cc[grep('Princess', cc[,1]), 1:5]
rowSums(cc[grep('Princess', cc[,1]), -c(1:4)]) 

ij0 <- pmatch(ca.prov$PRENAME, cc[, 1])
ij0
ij <- ij0[complete.cases(ij0)]

table(as.character(ca.prov@data$PRENAME[complete.cases(ij0)])==
      as.character(cc[ij,1]))

news <- t(apply(as.matrix(cc[ij, 5:ncol(cc)]), 1,
                function(x) diff(c(0, cummax(x)))))
dim(news)

lnews <- apply(news, 2, function(x)
    ifelse(x==0, -1, log(x)))
dim(lnews)

exp(-9:0/3)
sum(exp(-9:0/3))
sum(w <- exp(-9:0/3)/sum(exp(-9:0/3)))
w <- rep(0.1,10)

l7news <- t(apply(news, 1, function(x) {
    r <- x
    r[1:10] <- mean(x[1:10])
    for (j in 11:length(x))
        r[j] <- sum(w*x[-9:0+j])
    return(ifelse(r<1, -1*(1-r), sqrt(r)))
}))
    

par(mfrow=c(1,1), mar=c(0,0,0,0), xaxs='i', yaxs='i')
stplot(l7news, ###as.matrix(cc[ij, 5:ncol(cc)]), 
###    log(as.matrix(cc[ij, 5:ncol(cc)])+0.5,10),
       ca.prov[complete.cases(ij0),], lwd=3, legend=FALSE, ex=0.5) 
plot(ca.prov, add=TRUE, lty=2)
text(coordinates(ca.prov)[complete.cases(ij0),1],
     coordinates(ca.prov)[complete.cases(ij0),2]-1,
     rowSums(news), col='red', srt=45)
1086 + 193 + 55 + 271

rowSums(news)
rowSums(news[aai <- c(3, 8, 11, 12), ])
dim(news)
rowSums(news[aai, -93:0+ncol(news)])
rowSums(news[aai, -90:0+ncol(news)])
sum(news[aai, -90:0+ncol(news)])

