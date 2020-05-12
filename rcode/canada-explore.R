
if (FALSE) { ## can manually skip

    options(width=70)
    setwd('..')

    if (FALSE)
        source('rcode/getdata.R')

}

wc0 <- read.csv('data/confirmed_global.csv')
dim(wc0)
wc0[1:5, 1:6]

date <- as.Date(colnames(wc0)[5:ncol(wc0)], 'X%m.%d.%y')

cc <- wc0[which(wc0$Country.Region=='Canada'), ]
cc[,1]

range(cc.n <- colSums(cc[, 5:ncol(cc)]))

### w based on serial interval ~ Gamma(mean=5, sd=3)
### to compute R_t (reproduction number varying over time)
pw <- pgamma(0:14, shape=(5/3)^2, scale=3^2/5)
w <- diff(pw)/sum(diff(pw))

library(emisc) ### from https://github.com/eliaskrainski/emisc

par(mfrow=c(2,2), mar=c(3,3,0.5,0.5), mgp=c(2, 0.5, 0))
epidplot(date, cc.n, w=w)

par(mfrow=c(3,4), mar=c(3,3,1.5,0.5), mgp=c(2, 0.5, 0), las=1)
for (i in c(1, 2, 4:11, 14:15)) {
    epidplot(date, unlist(cc[i, 5:ncol(cc)]), which=1,
             main=cc[i, 1])
    abline(h=log(rep(c(1,3), 5)*(10^rep(0:5, each=2)), 10), 
           lty=2, col=gray(0.5, 0.5))
}

par(mfrow=c(3,4), mar=c(3,3,1.5,0.5), mgp=c(2, 0.5, 0), las=1)
for (i in c(1, 2, 4:11, 14:15)) {
    cas <- unlist(cc[i, 5:ncol(cc)])
    ii <- which(cas>0)
    epidplot(date[ii], cas[ii], which=4,
             main=cc[i, 1], w=w)
}

source('rcode/canada-map.R')

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

par(mfrow=c(1,1), mar=c(0,0,0,0), xaxs='i', yaxs='i')
stplot(as.matrix(cc[ij, 5:ncol(cc)]), 
###    log(as.matrix(cc[ij, 5:ncol(cc)])+0.5,10),
       ca.prov[complete.cases(ij0),], lwd=3) 
plot(ca.prov, add=TRUE, lty=2)

