
if (FALSE) { ## can manually skip

    options(width=70)
    setwd('..')

    if (FALSE)
        source('rcode/getdata.R')

}

d.all <- read.csv('data/caso.csv.gv')
head(d.all,2)

d.uf <- d.all[which(d.all$place_type=='state'), ]

w.uf <- lapply(d.uf[c('confirmed', 'deaths')],
               tapply, d.uf[c('state', 'date')], as.integer) 
str(w.uf)

n.uf <- lapply(w.uf, apply, 1, function(x) {
    x[is.na(x)] <- 0; diff(c(0, cummax(x)))
})

str(n.uf)
ss <- lapply(n.uf, apply, 2, function(y)
    mgcv:::gam(y~s(x), poisson(),
               list(x=1:length(y), y=y))$fitted)
str(ss)

plot(ss[[1]][,1], type='l')
points(n.uf[[1]][,1])

(dd <- dim(n.uf[[1]]))
uf.long <- data.frame(
    date=as.Date(rep(colnames(w.uf[[1]]), each=dd[2])),
    uf=rep(rownames(w.uf[[1]]), dd[1]),
    aCasos=as.vector(w.uf[[1]]),
    aObitos=as.vector(w.uf[[2]]),
    nCasos=as.vector(t(n.uf[[1]])),
    nObitos=as.vector(t(n.uf[[2]])),
    sCasos=as.vector(t(ss[[1]])),
    sObitos=as.vector(t(ss[[2]])))
    
summary(uf.long$nCasos)

library(ggplot2)

cores <- c('casos'='black', 'óbitos'='red')

png('figures/uf-date-new.png', 600, 600)
ggplot(uf.long) +
    geom_point(aes(x=date, y=nCasos, color='casos'), size=0.3) +
    geom_point(aes(x=date, y=nObitos, color='óbitos'), size=0.3) +
    geom_line(aes(x=date, y=sCasos, color='casos')) +
    geom_line(aes(x=date, y=sObitos, color='óbitos')) +
        scale_y_log10(breaks=c(1, 10, 100, 1000, 5000,30000),
                  limits=c(1, 30000)) +
    facet_wrap(~uf) + theme_bw() + 
    labs(x='', y='', color='Legenda') + 
    scale_color_manual(values = cores) +
    theme(legend.position=c(0.9,0.1))
dev.off()
if (FALSE)
    system('eog figures/uf-date-new.png &')

ufsel <- c('AC', 'AL', 'AM', 'AP', 'CE',
           'MA', 'PA', 'PB', 'PE')

ggplot(uf.long[uf.long$uf%in%ufsel, ]) +
    geom_point(aes(x=date, y=nCasos, color='casos'), size=0.7) +
    geom_point(aes(x=date, y=nObitos, color='óbitos'), size=0.7) +
    geom_line(aes(x=date, y=sCasos, color='casos')) +
    geom_line(aes(x=date, y=sObitos, color='óbitos')) +
        scale_y_log10(breaks=c(1, 10, 100, 1000, 5000,30000),
                  limits=c(1, 30000)) +
    facet_wrap(~uf) + theme_bw() + 
    labs(x='', y='', color='Legenda') + 
    scale_color_manual(values = cores) +
    theme(legend.position=c(0.071,0.923))


d.all$Date <- as.Date(d.all$date)
d.all <- d.all[order(d.all$Date), ]

ggplot(d.all[which(d.all$place_type=='state'), ]) +
    geom_point(aes(x=Date, y=confirmed, color='casos')) +
    geom_point(aes(x=Date, y=confirmed, color='óbitos')) +
    facet_wrap(~state) + scale_y_log10()


library(readxl)

dfl <- 'data/HIST_PAINEL_COVIDBR_20200515.xlsx'
d.all <- as.data.frame(read_xlsx(dfl))

for (j in c(10:12))
    d.all[, j] <- as.numeric(d.all[, j])
d.all$Data <- as.Date(d.all$data)

dbr <- d.all[(d.all$regiao=='Brasil') &
             (d.all$casosAcumulado>0), ]
##             (d.all$obitosAcumulado>0), ]
dim(dbr)

head(dbr,2)
summary(dbr)

dbr[which(dbr$emAcompanhamento>0), c(8,11,12,13,14)]

plot(dbr$Data, dbr$casosAcumulado)
lines(dbr$Data, dbr$obitosAcumulado, col=2)
points(dbr$Data, dbr$Recuperadosnovos, col=4)
lines(dbr$Data, ifelse(is.na(dbr$Recuperadosnovos), 0,
                       dbr$Recuperadosnovos) +
     dbr$obitosAcumulado, col=4)

### w based on serial interval ~ Gamma(mean=5, sd=3)
### to compute R_t (reproduction number varying over time)
pw <- pgamma(0:14, shape=(5/3)^2, scale=3^2/5)
w <- diff(pw)/sum(diff(pw))

library(emisc) ### from https://github.com/eliaskrainski/emisc

par(mfrow=c(2,4), mar=c(2,3,0.5,0.5), mgp=c(2,0.5,0), las=1)
epidplot(dbr$Data[dbr$casosAcumulado>0],
         dbr$casosAcumulado[dbr$casosAcumulado>0],
         w=w, lwd=3, cex=0.5, which=1:4)
points(dbr$Data, Rt.h, col=2, pch=4)
epidplot(dbr$Data[dbr$obitosAcumulado>0],
         dbr$obitosAcumulado[dbr$obitosAcumulado>0],
         lylab=c('óbitos', 
         w=w, lwd=3, cex=0.5, which=1:4)

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


