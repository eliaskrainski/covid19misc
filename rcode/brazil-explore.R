
if (FALSE) { ## can manually skip

    options(width=70)
    setwd('..')

    if (FALSE)
        source('rcode/getdata.R')

}


library(readxl)

dfl <- 'data/DT1_PAINEL_COVIDBR_20200512.xlsx'
d.all <- as.data.frame(read_xlsx(dfl))

for (j in c(10:12))
    d.all[, j] <- as.numeric(d.all[, j])
d.all$Data <- as.Date(d.all$data)

dbr <- d.all[(d.all$regiao=='Brasil') &
             (d.all$obitosAcumulado>0), ]
dim(dbr)

### w based on serial interval with mean=5 and sd=3
pw <- pgamma(0:14, shape=(5/3)^2, scale=3^2/5)
w <- diff(pw)/sum(diff(pw))

library(emisc) ### from https://github.com/eliaskrainski/emisc
par(mfrow=c(2,2), mar=c(2,3,0.5,0.5), mgp=c(2,0.5,0), las=1)
epidplot(dbr$Data, dbr$obitosAcumulado, w=w,
         lwd=3, cex=0.5)

### from brazil.io
cbr <- read.csv('data/caso.csv.gv')
cbr$date <- as.Date(as.character(cbr$date))
cbr <- cbr[order(cbr$date), ]

i.uf <- which(cbr$place_type=='state')
sbr <- tapply(cbr$confirmed[i.uf], cbr$date[i.uf], sum)

par(mfrow=c(1,2), mar=c(2,3,0.5,0.5), mgp=c(2,0.5,0), las=1)
epidplot(dbr$Data, cummax(dbr$casosAcumulado), lwd=3, cex=0.5, which=1)
abline(v=min(dbr$Data)+7*(0:11), lty=2, col=gray(0.15, 0.5), lwd=2)
epidplot(as.Date(names(sbr)), cummax(sbr), lwd=3, cex=0.5, which=1)
abline(v=min(dbr$Data)+7*(0:11), lty=2, col=gray(0.15, 0.5), lwd=2)

### put time series by states to wide shape
wc.uf <- reshape(cbr[i.uf, c('confirmed', 'date', 'state')],
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

library(emisc) ### from https://github.com/eliaskrainski/emisc

par(mfrow=c(2,2), mar=c(3,3,1,0.5), mgp=c(2, 0.5, 0), las=1)
epidplot(date, colSums(wc.uf[,-1]),
         main='Brasil', w=w) 

par(mfrow=c(6,5), mar=c(2,3,1,0.5), mgp=c(2, 0.5, 0))
for (i in 1:nrow(wc.uf)) 
    epidplot(date, unlist(wc.uf[i, 2:ncol(wc.uf)]),
             which=1,  main=wc.uf[i, 1], cex=0.5) 
epidplot(date, colSums(wc.uf[,-1]), main='Brasil')

## plot only the growth for each state and Brasil
par(mfrow=c(6,5), mar=c(2,3,1,0.5), mgp=c(2, 0.5, 0))
for (i in 1:nrow(wc.uf)) 
    epidplot(date, unlist(wc.uf[i, 2:ncol(wc.uf)]), which=2,
             main=wc.uf[i, 1]) 
epidplot(date, colSums(wc.uf[,-1]), main='Brasil', which=2)

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


