
if (FALSE) { ## can manually skip

    options(width=70)
    setwd('..')

    if (FALSE)
        source('rcode/getdata.R')

}

d.all <- read.csv('data/caso.csv.gv')
head(d.all,2)
tail(d.all,2)

d.all$date <- as.Date(as.character(d.all$date))
d.all <- d.all[order(d.all$date), ]

i.uf <- which(d.all$place_type=='state')

### put time series by states to wide shape
wc.uf <- reshape(d.all[i.uf, c('confirmed', 'date', 'state')],
                 direction='wide', timevar='date', idvar='state')
rownames(wc.uf) <- wc.uf[,1]
dim(wc.uf)
wc.uf[1:3, 1:3]

### some fix
wc.uf[, -1] <- t(apply(wc.uf[,-1], 1, function(x) {
    x[is.na(x)] <- 0
    return(cummax(x))
}))
wc.uf[1:3, 1:3]
wc.uf[1:3, -2:0+ncol(wc.uf)]

date <- as.Date(substring(colnames(wc.uf)[-1], 11))

apply(wc.uf[order(wc.uf[,1]), -1], 1, function(x)
    as.character(date)[which(x>0)[1]])

### w based on serial interval ~ Gamma(mean=5, sd=3)
### to compute R_t (reproduction number varying over time)
pw <- pgamma(0:14, shape=(5/3)^2, scale=3^2/5)
w <- diff(pw)/sum(diff(pw))

library(emisc) ### from https://github.com/eliaskrainski/emisc

par(mfrow=c(2,2), mar=c(3,3,1,0.5), mgp=c(2, 0.5, 0), las=1)
epidplot(date, colSums(wc.uf[,-1]), which=list(2, 3, 4, 6),
         main='Brasil', w=w, lxlab=rep('', 5), log10=FALSE,
         lylab=list('Casos novos por dia',
                    'Velocidade do crescimento diário (casos)',
                    'Crescimento relativo diário (%)',
                    'Número de reprodução')) 

par(mfrow=c(6,5), mar=c(2,3,1,0.5), mgp=c(2, 0.5, 0))
for (i in 1:nrow(wc.uf)) 
    epidplot(date, unlist(wc.uf[i, 2:ncol(wc.uf)]),
             which=1,  main=wc.uf[i, 1], cex=0.5) 
epidplot(date, colSums(wc.uf[,-1]), 1:3, main='Brasil')

## plot only the growth for each state and Brasil
par(mfrow=c(6,5), mar=c(2,3,1,0.5), mgp=c(2, 0.5, 0))
plot(date, diff(c(0, colSums(wc.uf[,-1]))),
     xlab='', ylab='', main='Brasil', pch=19, cex=0.5)
for (i in order(wc.uf[,1])) 
    plot(date, diff(c(0, unlist(wc.uf[i, 2:ncol(wc.uf)]))),
         xlab='', ylab='', main=wc.uf[i, 1], pch=19, cex=0.5) 

source('rcode/brazil-uf-map.R')
head(brufs@data,2)

(ij <- pmatch(brufs$UF, wc.uf[,1]))
table(brufs$UF==wc.uf[ij,1])

summary(n <- wc.uf[ij,ncol(wc.uf)])
summary(tx <- n/(1e-5*brufs$pop2019))

wc.uf[ij, -1:0+ncol(wc.uf)]/(1e-5*brufs$pop2019)

par(mfrow=c(1,3), mar=c(0,0,0,0), xaxs='i', yaxs='i')
stplot(as.matrix(wc.uf[ij, 2:ncol(wc.uf)]),
       brufs, lwd=3, ex=0.7, ey=0.7,
       leg.args=list(x='bottomleft', lty=1, ncol=2, cex=0.7,
                     legend=paste0(wc.uf[,1], ': ', n
                                   )[rev(order(n))]), 
       col=x2rgb(n, u=TRUE))
stplot(t(apply(wc.uf[ij, -1], 1, function(x) diff(c(0, x)))),
       brufs, lwd=3, ex=0.7, ey=0.7,
       leg.args=list(x='bottomleft', lty=1, ncol=2,
                     legend=paste0(wc.uf[,1], ': ', n
                                   )[rev(order(n))]), 
       col=x2rgb(n, u=TRUE))
stplot(as.matrix(wc.uf[ij, 2:ncol(wc.uf)]/
                 (1e-5*brufs$pop2019)),
       leg.args=list(x='bottomleft', lty=1, ncol=3, 
                     legend=paste0(wc.uf[,1], ': ',
                                   format(tx, digits=1)
                                   )[rev(order(tx))]), 
       brufs, lwd=3, ex=0.7, ey=0.7,
       col=x2rgb(tx, u=TRUE))


