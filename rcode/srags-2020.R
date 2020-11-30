if (FALSE)
    setwd('..')

load("data/wdl.RData")

ibr <- which(wdl[[1]]$Country=='Brasil' &
             wdl[[1]]$Province=='')
ibr

n2br <- sapply(wdl, function(m) 
    diff(c(0, unlist(m[ibr, 8:ncol(wdl[[1]])]))))
tail(n2br)

source('rcode/load-srags-2020.R')

dim(srags20)
srags20[1,]

i5.ev.cl <- with(
    srags20, table(gIdade5, Evolução,
                   Classificação))

dim(i5.ev.cl)
i5.ev.cl[1,,]
apply(i5.ev.cl, 3, colSums)

i.c <- which(srags20$Classificação=='COVID19')
nc.day <- with(srags20[i.c, ], table(dSintPrinc))
sum(nc.day)
table(srags20$Classificação)

i.o.c <- which(srags20$Evolução=='Óbito' &
               srags20$Classificação == 'COVID19') 
n.day <- with(srags20[i.o.c, ], table(dEvol))
sum(n.day)

n.dpcr <- with(srags20[i.o.c, ], table(dPCR))
sum(n.dpcr)

msDate <- as.Date(rownames(n2br), 'X%Y%m%d')
Rse <- t(sapply(1:nrow(n2br), function(i) {
    d <- abs(1:nrow(n2br)-i)/6
    d <- (1-d)*(d<1)
    d/sum(d)
}))
Rse[1:7, 1:10]
table(rowSums(Rse))
tail(n2br)
n2s <- apply(n2br, 2, function(y) Rse %*% y)
summary(n2br)
summary(n2s)

xl <- list(x=as.Date(paste(2020, rep(1:12, each=3),
                           c(1,10,20), sep='-')))
xl$l <- format(xl$x, '%b,%d')
yl <- list(y=rep(10^(0:5),each=3)*c(1,2,4))
yl$l <- ifelse(yl$y<1e4, yl$y, paste0(yl$y/1e3, 'K'))

png('figures/casos-srags-datas.png', 1000, 600)
par(mfrow=c(1,1), mar=c(3,4,0.5,0.5), mgp=c(2.5,0.5,0))
plot(msDate, n2br[,1],
     type='l', lwd=2, log='y', axes=FALSE, 
     xlab='', ylab='C A S O S',
     xlim=range(as.Date(names(nc.day)))-c(0,1),
     ylim=pmax(0.7, range(nc.day, n2br[,1])+1))
lines(msDate, n2s[,1])
lines(as.Date(names(nc.day)),
     as.integer(nc.day), lwd=3, col=2)
abline(v=xl$x, h=yl$y, lty=2, col=gray(0.5, 0.5))
axis(1, xl$x, xl$l)
axis(2, yl$y, yl$l, las=1) 
ll0 <- c('Divulgação MS (confirmação)',
         'SRAGs por data de primeiros sintomas')
legend('topleft', bg=gray(0.99), lwd=3,
       c(ll0), 
       box.col='transparent', lty=c(1,2), col=c(1,2), cex=1.2)
dev.off()
if(FALSE)
    system('eog figures/casos-srags-datas.png &')

png('figures/obitos-data-ocorrencia.png', 600, 400)
par(mfrow=c(1,1), mar=c(3,4,0.5,0.5), mgp=c(2.5,0.5,0))
plot(msDate, n2br[,2],
     type='l', lwd=2, las=1, 
     xlab='', ylab='Ó B I T O S',
     xlim=range(as.Date(names(n.day)))-c(0,1),
     ylim=range(n.day, n.dpcr, n2br[,2]))
lines(msDate, n2s[,2])
lines(as.Date(names(n.day)),
     as.integer(n.day), lwd=3, col=2)
lines(as.Date(names(n.dpcr)),
      as.integer(n.dpcr), col=3, lty=2, lwd=3) 
abline(v=xl$x, h=pretty(par()$usr[3:4],10),
       lty=2, col=gray(0.5, 0.5))
ll0 <- c('Divulgação MS (confirmação)',
         'Óbitos por data de ocorrência',
         'Óbitos por data de PCR')
legend('topleft', bg=gray(0.99), lwd=3,
       c(ll0), 
       box.col='transparent', lty=c(1,1,2), col=c(1,2,3), cex=1.2)
dev.off()
if(FALSE)
    system('eog figures/obitos-data-ocorrencia.png &')

grep('CLAS', names(srags20), val=T)

sem.na.clf <- table(srags20$SEM_NOT, is.na(srags20$CLASSI_FIN))
colSums(sem.na.clf)

coresc <- c(gray(.5), 'orange', 'blue',
            'brown', 'wheat', 'red')

par(mfrow=c(2,2), mar=c(3,4,0.5,0.5), mgp=c(2,0.5,0))
barplot(table(srags20$Classificação, srags20$SEM_NOT)+0.1,
        col=coresc, las=1)
legend('topleft', levels(srags20$Classificação),
       fill=coresc, border=coresc, bty='n')

barplot(t(sem.na.clf), col=3:4)
plot(rowSums(sem.na.clf),
     ylim=range(rowSums(sem.na.clf)), pch=19,
     xlab='', ylab='', las=1)
for (j in 1:2) 
    points(sem.na.clf[,j], pch=8, col=j)

plot(sem.na.clf[, 2]/rowSums(sem.na.clf))

(m.i5 <- c(2 + 5*0:17, 95, 105))
i5.ev.cl[,,6]
colSums(i5.ev.cl[,,6])

par(mfrow=c(1,1), mar=c(4,4,0.5,0.5), mgp=c(2.5,0.5,0))
plot(m.i5, 100*i5.ev.cl[,3,6]/rowSums(i5.ev.cl[,2:3,6]),
     type='l', lwd=2, ylim=c(0,100), axes=FALSE,
     xlab='I D A D E ( anos )', ylab='Ó B I T O ( % )')
axis(1, 10*(0:11))
axis(2, 10*0:10, las=1)
abline(h=10*0:10, v=5*(0:22), col=gray(0.5,0.5), lty=2)
legend("topleft",
       c('Hospitalizados com SRAG por COVID19',
         'Brazil 2020 (atualizado até 29/Julho)'), 
       title='Percentual de óbitos de pacientes', bty='n')

t.evol <- as.numeric(difftime(srags$dEvol, srags$dSintPrinc,
                              units='days'))
summary(t.evol)

d.evol.pcr <- as.numeric(difftime(srags$dEvol, srags$dPCR, 
                                  units='days'))
summary(d.evol.pcr)

t.dt.nt <- table(srags$dNotifica)
t.dt.sp <- table(srags$dSintPrinc)
t.dt.dg <- table(srags$dDigita)
t.dt.pcr <- table(srags$dPCR)

tail(t.dt.dg,3)
tail(t.dt.sp,3)
tail(t.dt.nt,3)
tail(t.dt.pcr,3)

xlm <- as.Date(c('2020-03-15', tail(names(t.dt.dg),1)))

png('figures/srag-delay.png', 700, 500)
par(mfrow=c(2,4), mar=c(3,3,0,0), mgp=c(2,0.5,0))
d1 <- as.integer(difftime(
    srags$dSintPrinc, srags$dNotifica, units='days'))
summary(d1)
sum(d1>0)
d1[(d1<(-499)) | (d1>0)] <- NA
h1 <- hist(d1, -500:1-0.5, plot=FALSE)
plot(h1, xlim=c(-30, 0), col=gray(0.7), las=1,
     xlab='Atrado (dias)', ylab='', main='')
legend('topleft', title='Atraso na notificação',
       'Sintoma principal - notificação', bty='n')
d2 <- as.integer(difftime(
    srags$dNotifica, srags$dDigita, units='days'))
summary(d2)
sum(d2>0)
d2[(d2<(-499)) | (d2>0)] <- NA
h2 <- hist(d2, -500:1-0.5, plot=FALSE)
plot(h2, xlim=c(-30, 0), col=gray(0.7), las=1,
     xlab='Atrado (dias)', ylab='', main='')
legend('topleft', title='Atraso na digitação',
       'Notificação - digitação', bty='n')
d3 <- as.integer(difftime(
    srags$dPCR, srags$dDigita, units='days'))
summary(d3)
d3[(d3<(-199)) | (d3>200)] <- NA
h3 <- hist(d3, -200:201-0.5, plot=FALSE)
plot(h3, xlim=c(-30, 30), col=gray(0.7), las=1,
     xlab='data PCR - data digitação', ylab='', main='')
legend('topleft', title='Diferença', 
       'PCR - digitação', bty='n')
d4 <- as.integer(difftime(
    srags$dPCR, srags$dSintPrinc, units='days'))
summary(d4)
sum(d4<0, na.rm=TRUE)
d4[(d4<0) | (d4>500)] <- NA
h4 <- hist(d4, 0:501-0.5, plot=FALSE)
plot(h4, xlim=c(0, 30), col=gray(0.7), las=1,
     xlab='Tempo (dias)', ylab='', main='')
legend('topright', title='Tempo até a confirmação:',
       'Sintoma principal até PCR', bty='n')
par(mfrow=c(2,1), new=TRUE)
plot(as.Date(names(t.dt.dg)),
     as.integer(t.dt.dg), type='h', axes=FALSE,
     las=1, xlab='', ylab='', lwd=4, col=4, lty=3, xlim=xlm)
points(as.Date(names(t.dt.nt)),
       as.integer(t.dt.nt), col=3, lty=2, pch=19, type='o')
points(as.Date(names(t.dt.sp)),
       as.integer(t.dt.sp), pch=8, type='o')     
points(as.Date(names(t.dt.pcr)),
       as.integer(t.dt.pcr), pch=4, type='o', cex=2, col=2)     
legend('topleft', title='SRAGs por data de', bty='n',
       c("Sintoma Principal", "Notificação", "Digitação", "PCR"),
       col=c(1, 3, 4, 2), lty=c(1, 2, 3, 1), lwd=3)
axis(1, pretty(xlm,15), format(pretty(xlm,15), '%b,%d'))
axis(2, las=1)
abline(h=pretty(par()$usr[3:4]), lty=2, col=gray(0.7,0.3))
dev.off()
if (FALSE)
    system('eog figures/srag-delay.png &')

### labels da variavel CLASSI_FIN
lab.cf <- c('NA', 'Influenza', 'ORespir', 
            'OEtiol', 'NEspec', 'COVID19')
table(is.na(srags$CLASSI_FIN))
table(srags$CLASSI_FIN)
srags$Classificação  <- factor(
    ifelse(is.na(srags$CLASSI_FIN), 0,
           srags$CLASSI_FIN), c(0:5), lab.cf)
table(srags$Classificação,
      ifelse(is.na(srags$CLASSI_FIN), 0, srags$CLASSI_FIN))

lab.ev <- c('NA', 'Cura', 'Óbito', 'Indef')
table(is.na(srags$EVOLUCAO))
table(srags$EVOLUCAO)
srags$Evolução <- factor(
    ifelse(is.na(srags$EVOLUCAO), 0,
           srags$EVOLUCAO), c(0,1,2,9), lab.ev)
table(srags$Evolução,
      ifelse(is.na(srags$EVOLUCAO), 0, srags$EVOLUCAO))

tapply(t.evol, srags$Evolução, mean, na.rm=TRUE)
tapply(t.evol, srags$Evolução, mean, na.rm=TRUE)[1:3]

addmargins(table(d.evol.pcr<0, srags$Evolução))
tapply(d.evol.pcr, list(a=d.evol.pcr<0, e=srags$Evolução),
       mean, na.rm=TRUE) 
tapply(d.evol.pcr, list(a=d.evol.pcr<0, e=srags$Evolução),
       median, na.rm=TRUE) 

table(is.na(srags$CO_MUN_RES))
table(ipr <- substr(srags$CO_MUN_RES,1,2)=='41')

table(icwb <- substr(srags$CO_MUN_RES,1,6)=='410690')

addmargins(with(srags,
                table(Classificação, Evolução)))

addmargins(with(srags[ipr,],
                table(Classificação, Evolução)))

addmargins(with(srags[icwb,],
                table(Classificação, Evolução)))

### tempo de hospitalização
srags$durIntern <- as.numeric(difftime(
    as.Date(srags$DT_ENCERRA, '%d/%m/%Y'),
    as.Date(srags$DT_INTERNA, '%d/%m/%Y'), units='days'))
summary(srags$durIntern)

h.dI <- hist(srags$durIntern,
             c(-Inf, 0:90-0.5, Inf), plot=FALSE)

plot(h.dI, xlim=c(0,90))

head(h.dI$mid)
tail(h.dI$mid)

sum(h.dI$mid[2:(length(h.dI$mid)-1)]*
    h.dI$count[2:(length(h.dI$mid)-1)])/
    sum(h.dI$count[2:(length(h.dI$mid)-1)])

###
table(is.na(srags$SEM_PRI),
      is.na(srags$SEM_NOT))
table(srags$SEM_NOT-srags$SEM_PRI)

### 
uf.cl.ev  <- table(substr(srags$CO_MUN_RES,1,2),
                   srags$Classificação,
                   srags$Evolução)
dim(uf.cl.ev)

uf.cl.ev[1,,]

s.ne.uf <- apply(uf.cl.ev, 1, function(x) sum(x[5, ]))
s.co.uf <- apply(uf.cl.ev, 1, function(x) sum(x[6, ]))

o.ne.uf <- apply(uf.cl.ev, 1, function(x) x[5, 3])
o.co.uf <- apply(uf.cl.ev, 1, function(x) x[6, 3])
o.ne.uf
o.co.uf

summary(r.s.ne.co <- s.ne.uf/s.co.uf)
summary(r.o.ne.co <- o.ne.uf/o.co.uf)

qk <- c(0, 0.2, 0.4, 0.8, 1.2, 1.6, 2, 2.4)

load('maps/brufs.RData')

table(names(r.s.ne.co)==names(r.o.ne.co))
o.uf <- pmatch(brufs$CD_GEOCUF, names(r.o.ne.co))

table(brufs$CD_GEOCUF==names(r.o.ne.co[o.uf]))

library(emisc)

xlm <- bbox(brufs)[1,]
xlm[2] <- xlm[2]-0.1*diff(xlm)

png("figures/mapa-uf-obitos-ne-covid.png", 800, 400)
par(mfrow=c(1,2), mar=c(0,0,0,0), xaxs='i', yaxs='i')
plot(brufs, xlim=xlm, col=x2rgb(r.s.ne.co[o.uf], qk), border='white') 
text(coordinates(brufs)[, 1], coordinates(brufs)[, 2],
     format(100*r.s.ne.co[o.uf], digits=0), font=2, cex=1)
legend('topright', bty='n', adj=c(0, 0.05), 
       paste0('SRAGs com PCR\n"Não especificado"\n',
              'para cada 100 SRAGs\npor "SARS-COV-2"'))
plot(brufs, xlim=xlm, col=x2rgb(r.o.ne.co[o.uf], qk), border='white')
text(coordinates(brufs)[, 1], coordinates(brufs)[, 2],
     format(100*r.o.ne.co[o.uf], digits=0), font=2, cex=1)
legend('topright', bty='n', adj=c(0, 0.05), 
       paste0('óbitos com PCR\n"Não especificado"\n',
              'para cada 100 óbitos\npor "SARS-COV-2"'))
dev.off()
if (FALSE)
    system("eog figures/mapa-uf-obitos-ne-covid.png &")

table(is.na(srags$DT_NOTIFIC),
      is.na(srags$DT_SIN_PRI))

summary(as.Date(srags$DT_SIN_PRI, '%d/%m/%Y'))
summary(as.Date(srags$DT_NOTIFIC, '%d/%m/%Y'))

srags$AtrNot <- as.numeric(difftime(
    as.Date(srags$DT_NOTIFIC, '%d/%m/%Y'),
    as.Date(srags$DT_SIN_PRI, '%d/%m/%Y'),
    units='days'))
summary(srags$AtrNot)

srags$tempoPCR <- as.numeric(difftime(
    as.Date(srags$DT_PCR, '%d/%m/%Y'),
    as.Date(srags$DT_COLETA, '%d/%m/%Y'),
    units='days'))
summary(srags$tempoPCR)
summary(srags$tempoPCR[srags$Classificação=='COVID19'])

table(substr(srags$CO_MUN_RES,1,2))

summary(as.numeric(as.Date(srags$DT_PCR, '%d/%m/%Y')-
                   as.Date(srags$DT_NOT, '%d/%m/%Y')))

bkt <- -130:160-1e-5

par(mfrow=c(9,9), mar=c(0,0,0,0))
for (uf in sort(unique(substr(srags$CO_MUN_RES,1,2)))) {
    i <- which(substr(srags$CO_MUN_RES,1,2)==uf &
               srags$Classificação=='COVID19')
    h <- hist(srags$tempoPCR[i], bkt, plot=FALSE)
    plot(h, xlim=c(0, 30), col=rgb(0.1, 0.5, 0.1),
         xlab='', ylab='', main='', axes=FALSE)
    box(lty=3)
    ip <- h$mids>0
    segments(10*(0:3), rep(0,3), 10*(0:3),
             rep(mean(range(h$counts[ip])), 3))
    legend('topright',
           format(sum((h$counts*h$mids)[ip])/sum(h$counts[ip]), dig=2),
           title=srags$SG_UF_NOT[i[1]], bty='n', cex=1.5)
    h <- hist(as.numeric(as.Date(srags$DT_PCR[i], '%d/%m/%Y')-
                         as.Date(srags$DT_NOT[i], '%d/%m/%Y')),
              bkt, plot=FALSE)
    plot(h, xlim=c(0, 30), col=rgb(0.5,1,0.5), 
         xlab='', ylab='', main='', axes=FALSE)
    box(lty=3)
    ip <- h$mids>0
    segments(10*(0:3), rep(0,3), 10*(0:3),
             rep(mean(range(h$counts)), 3))
    legend('topright',
           format(sum((h$counts*h$mids)[ip])/sum(h$counts[ip]), dig=2),
           title=srags$SG_UF_NOT[i[1]], bty='n', cex=1.5)
    h <- hist(as.numeric(as.Date(srags$DT_PCR[i], '%d/%m/%Y')-
                         as.Date(srags$DT_SIN_PRI[i], '%d/%m/%Y')),
              bkt, plot=FALSE)
    plot(h, xlim=c(0, 30), col=rgb(1,0.5,0.1), 
         xlab='', ylab='', main='', axes=FALSE)
    box(lty=3)
    ip <- h$mids>0
    segments(10*(0:3), rep(0,3), 10*(0:3),
             rep(mean(range(h$counts)), 3))
    legend('topright',
           format(sum((h$counts*h$mids)[ip])/sum(h$counts[ip]), dig=2),
           title=srags$SG_UF_NOT[i[1]], bty='n', cex=1.5)
}

par(mfrow=c(7,4), mar=c(0,0,0,0))
for (uf in sort(unique(substr(srags$CO_MUN_RES,1,2)))) {
    i <- which(substr(srags$CO_MUN_RES,1,2)==uf &
               srags$Classificação=='COVID19')
    h <- hist(as.numeric(as.Date(srags$DT_PCR[i], '%d/%m/%Y')-
                         as.Date(srags$DT_SIN_PRI[i], '%d/%m/%Y')),
              bkt, plot=FALSE)
    plot(h, xlim=c(0, 30), col=rgb(1,0.5,0.1), 
         xlab='', ylab='', main='', axes=FALSE)
    box(lty=3)
    ip <- h$mids>0
    legend('topright',
           format(sum((h$counts*h$mids)[ip])/sum(h$counts[ip]), dig=2),
           title=srags$SG_UF_NOT[i[1]], bty='n', cex=1.5)
}


plot(as.Date(srags$DT_NOTIFIC, '%d/%m/%Y'),
     jitter(srags$AtrNot))

plot(with(srags,
          table(SEM_PRI, Evolução)), main='')

srags$Idade <- srags$NU_IDADE_N/
    (c(365.25, 12, 1)[as.integer(substr(srags$COD_IDADE, 1, 1))])

par(mfrow=c(1,1), mar=c(3,3,0,0), mgp=c(2,0.5,0))
plot(srags$Idade~factor(srags$SEM_NOT),
     xlab='Semana de notificação', ylab='Idade', las=1)
points(tapply(srags$Idade, srags$SEM_NOT, mean, na.rm=TRUE), pch=19)


par(mfrow=c(1,1), mar=c(3,3,0,0), mgp=c(2,0.5,0))
plot(srags$Idade~factor(srags$Evolução),
     xlab='Semana de notificação', ylab='Idade', las=1)
points(tapply(srags$Idade, srags$Evolução, mean, na.rm=TRUE), pch=19)

table(srags$Evolução)

par(mfrow=c(2,2), mar=c(3,3,0.5,0.5), mgp=c(1.5, 0.5, 0))
for (le in levels(srags$Evolução)) {
    hist(srags$Idade[srags$Evolução==le],
         xlab='', ylab='', main='')
    abline(v=mean(srags$Idade[srags$Evolução==le]), lwd=2, col=2)
    legend('topleft', le, bty='n')
}


table(srags$gIdade <- cut(
          srags$Idade, c(5*(0:18), 100, 199), right=FALSE))

levels(srags$gIdade)

gI.ev <- table(srags$gIdade, srags$Evolução)

gi.ev.covid <- with(
    srags[srags$Classificação=='COVID19', ], 
    table(gIdade, Evolução))
gi.ev.covid[,3]/rowSums(gi.ev.covid)


table(srags$Classificação)
table(icovid <- srags$Classificação=='COVID19')

par(mfrow=c(4,5), mar=c(2,2,0.1,0.1), mgp=c(2,0.5,0))
for (gi in levels(srags$gIdade)) {
    ii <- icovid & srags$gIdade==gi
    io <- srags$Evolução=='Óbito'
    hh <- hist(srags$durIntern[ii & !io],
               c(-Inf, 2*(0:50)-0.01, Inf), plot=FALSE)
    h2 <- hist(srags$durIntern[ii & io], 
               c(-Inf, 2*(0:50)-0.01, Inf), plot=FALSE)
    plot(hh, col=rgb(0.5,0.7,1,0.5), border='transparent',
         xlab='', ylab='', main='', axes=FALSE,
         xlim=c(0,90), ylim=range(hh$dens, h2$dens))
    plot(h2, add=TRUE, 
         col=rgb(1,0.7,0.5, 0.5), border='transparent')
    axis(1)
    jj <- 2:(length(hh$mid)-1)
    m <- sum(hh$mid[jj]*hh$count[jj])/
        sum(hh$count[jj])
    n.o <- sum(ii & io) 
    legend('topright', title=paste('Idade:', gi), bty='n', 
           c(paste(format(m, dig=2), 'dias'),
             paste(sum(ii), 'pacientes'),
             paste(sum(ii&io), 'óbitos (', 
                   format(100*n.o/sum(ii), digits=3), '%)')))
    print(median(srags$durIntern[ii], na.rm=TRUE))           
}
legend('right', c('óbito', 'não óbito'), bty='n',
       fill=rgb(c(1,0.5), c(0.7,0.7), c(0.5,1), 0.5),
       border=rgb(c(1,0.5), c(0.7,0.7), c(0.5,1), 0.5))


table(srags$Classificação,
      srags$Evolução)

t.ev.gI <- with(srags, 
                table(gIdade, Evolução, Classificação))
sum(t.ev.gI)

dim(t.ev.gI)
t.ev.gI[c(1:3, 18:20), , 6]

mI <- c(2.5+5*(0:17), 95, 110)

c.ev <- c('gray',
          rgb(c(0.3, 1, 0.3),
              c(0.3, 0.3, 1), c(1, 0.3, 0.3)))

par(mfrow=c(1,1), mar=c(3,3,0.5,0.5), mgp=c(2, 0.5, 0))
plot(mI, rep(0.5, length(mI)), type='n', 
     xlab='Idade', ylab='', ylim=c(0, 1), las=1)
for (k in 1:6)
    for (d in 1:4)
        lines(mI, t.ev.gI[,d,k]/rowSums(t.ev.gI[,,k]),
              col=c.ev[d], lwd=3, lty=k, pch=k, type='o')
legend('topleft', dimnames(t.ev.gI)[[2]], col=c.ev,
       bty='n', lwd=3, lty=1, ncol=4)
legend('topright', dimnames(t.ev.gI)[[3]], 
       bty='n', lwd=3, lty=1:6, ncol=6, pch=1:6)

c2 <- rgb(1:5/5, c(3:4, 5:3)/5, 5:1/5)
c2s <- rgb(0.5+(1:5/10), 0.5+c(3:4, 5:3)/10, 0.5+ 5:1/10, 0.5)

table(srags$Classificação)

b1i <- c(0:100-0.5, 106, 150)
m1i <- c(1:100-0.5, 103, 110)
b1i <- c(5*(0:18), 100, 140)
m1i <- c(5*(1:18)-2.5, 95, 110)

srags$g1i <- findInterval(srags$Idade, b1i)

ng1i <- with(srags, table(g1i, Evolução, Classificação))
dim(ng1i)

dimnames(ng1i)[-1]
head(ng1i[,,6])

table(srags$Classificação)

cores4 <- rgb(1:4/4, 1-2*abs(1:4/4-2/4), 4:1/4)
c4shad <- rgb(1:4/4, 1-2*abs(1:4/4-2/4), 4:1/4, 0.3)

par(mfrow=c(1,1), mar=c(2, 2, 0.5, 0.5))
plot(m1i, rep(NA,length(m1i)),
     las=1, type='o', pch=19, ylim=c(0,90),
     xlab='Idade', ylab='% óbitos dos internados com COVID')
kk <- c(5,6)
for (k in 1:length(kk)) {
    n <- rowSums(ng1i[,,kk[k]])
    y <- ng1i[,3,kk[k]]
    polygon(c(m1i, rev(m1i), m1i[1]),
            100*c(qbeta(0.025, y, n-y),
                  rev(qbeta(0.975, y, n-y)),
                  qbeta(0.025, y[1], n[1]-y[1])),
            col=c4shad[k], border=cores4[k])
    abline(h=10*(0:9), lty=2, col=gray(0.5,0.5))
    lines(m1i, 100*y/n, col=cores4[k])
}
legend('topleft', levels(srags$Classificação)[5:6],
       col=cores4[1:2], lwd=5, lty=1)

png('figures/obito-SRAG-idade.png', 600, 400)
par(mfrow=c(1,1), mar=c(3,3,0.5,0.5), mgp=c(2, 0.5, 0))
plot(mI, rep(0.5, length(mI)), type='n', 
     xlab='Idade',
     ylab='Prop. SRAG evoluiu para óbito',
     ylim=c(0, 1), las=1)
for (k in c(2,3,4,5,6)) {
    y <- t.ev.gI[,3,k]
    n <- rowSums(t.ev.gI[,,k])
    lines(mI, y/n, col=c2[k-1], lwd=3)
    if (k%in%c(5,6))
        polygon(c(mI, rev(mI), mI[1]),
            c(qbeta(0.025, 0.1+y, 1+n-y), 
              qbeta(0.975, rev(0.1+y), rev(0.1+n-y)),
              qbeta(0.025, 0.1+y[1], .1+n[1]-y[1])), 
            col=c2s[k-1])
}
legend('topleft', dimnames(t.ev.gI)[[3]][-1], 
       lty=1, lwd=3, col=c2[1:(k-1)])
abline(h=0:20/10, lty=2, col=gray(0.7, 0.5))
dev.off()
if (FALSE)
    system('eog figures/obito-SRAG-idade.png &')

table(srags$Evolução)

table(is.na(srags$CO_MUN_RES),
      is.na(srags$CO_MUN_NOT))

table(srags$CO_MUN_RES==srags$CO_MUN_NOT)
prop.table(table(srags$CO_MUN_RES==srags$CO_MUN_NOT))

table(srags$CO_REGIONA==srags$CO_RG_RESI)
prop.table(table(srags$CO_REGIONA==srags$CO_RG_RESI))

dt.int <- lapply(alld, function(x)
    as.Date(x$DT_INTERNA, '%d/%m/%Y'))
as.Date('1970-01-01') + sapply(dt.int, min, na.rm=TRUE)
as.Date('1970-01-01') + sapply(dt.int, max, na.rm=TRUE)
sapply(1:length(dt.int), function(i)
    length(which(dt.int[[i]]>as.Date(paste0(2012+i,'-12-31')))))
if (FALSE) {  
    for (i in 1:length(dt.int)) 
        dt.int[[i]][
            which(dt.int[[i]]>
                  as.Date(paste0(2012+i,'-12-31')))] <- NA
}

dt.enc <- lapply(alld, function(x)
    as.Date(x$DT_ENCERRA, '%d/%m/%Y'))
as.Date('1970-01-01') + sapply(dt.enc, min, na.rm=TRUE)
as.Date('1970-01-01') + sapply(dt.enc, max, na.rm=TRUE)
if (FALSE) {
    alld[[5]][order(dt.enc[[5]])[1:5],]
    dt.enc[[5]][which(dt.enc[[5]]=='2001-04-20')] <-
        as.Date('2017-04-20')
}
sapply(1:length(dt.enc), function(i)
    length(which(dt.enc[[i]]>as.Date(paste0(2012+i,'-12-31')))))
sapply(1:length(dt.enc), function(i)
    length(which(dt.enc[[i]]>as.Date(paste0(2012+i+1,'-12-31')))))
if (FALSE) {  
    for (i in 1:length(dt.enc)) 
        dt.enc[[i]][
            which(dt.enc[[i]]>
                  as.Date(paste0(2012+i+1,'-12-31')))] <- NA
}

dt.obi <- c(lapply(alld[1:6], function(x)
    as.Date(x$DT_OBITO, '%d/%m/%Y')), 
    lapply(alld[7:8], function(x)
        ifelse(x$EVOLUCAO==2,
               as.Date(x$DT_EVOLUCA, '%d/%m/%Y'), NA)))

as.Date('1970-01-01') + sapply(dt.obi, min, na.rm=TRUE)
as.Date('1970-01-01') + sapply(dt.obi, max, na.rm=TRUE)

for (j in 1:length(alld))
    alld[[j]]$Classificação <- factor(
        alld[[j]]$CLASSI_FIN, c(1:5, 9),
        lab.class.fin)

table(alld[[8]]$Classificação, alld[[8]]$Evolução)

summary(idade20c <- as.numeric(difftime(
            as.Date(alld[[8]]$DT_SIN_PRI, '%d/%m/%Y'),
            as.Date(alld[[8]]$DT_NASC, '%d/%m/%Y'),
            units='days'))/365.25)
summary(idade20c-idade20)

hist(idade20)

plot(idade20 ~ alld[[8]]$Evolução)

idade20class <- cut(idade20, 5*c(0:18, 200), right=FALSE)
tab.idade.evol <- table(idade20class, alld[[8]]$Evolução)

plot(tab.idade.evol, las=1, col=c('blue', 'red', gray(c(0.9,0.5))),
     xlab='Faixa etária',
     main='Desfecho de SRAGs por faixa etária - 2020')


dtF <- function(a, b)
    as.integer(difftime(a, b, units='days'))

t.int <- mapply(dtF, dt.enc, dt.int)

round(sapply(t.int, summary))

par(mfrow=c(3,3), mar=c(3,3,1,1), mgp=c(2, 0.5, 0))
for (j in 1:length(t.int)) {
    hist(t.int[[j]], breaks=c(-Inf, 0:51, Inf), 
         xlim=c(0, 50), col=gray(0.7),
         main=paste('Ano', names(t.int)[j],
                    ':', length(t.int[[j]]), 'Internações'), 
         xlab='', ylab='')
    m <- median(t.int[[j]], na.rm=TRUE)
    abline(v=m, col=2, lwd=4)
    text(m+2, par()$usr[4]-0.2*diff(par()$usr[3:4]),
         format(m, digits=4))
}


### fix the 'SEM_NOT' column to 'yyyyww'
for (j in 1:length(alld))
    if (all(nchar(sprintf("%02d", as.integer(alld[[j]]$SEM_NOT)))==2))
        alld[[j]]$SEM_NOT <- paste0(
            2012+j, sprintf("%02d", as.integer(alld[[j]]$SEM_NOT)))


parana <- TRUE
if (parana) {
    sel.d <- lapply(alld, function(x) {
        j <- which(names(x)=='CO_MUN_NOT')
        if (length(j)==0) {
            ii <- which(substr(x$ID_MUNICIP, 1, 2)=='41')
        } else {
            ii <- which(substr(x[,j], 1, 2)=='41')
        }
        return(x[ii, ])
    })
} else {
    sel.d <- alld 
}

if (FALSE) {
    curitiba <- FALSE#TRUE
    if (curitiba) {
        sel.d <- lapply(alld, function(x) {
            j <- which(names(x)=='CO_MUN_NOT')
        if (length(j)==0) {
            ii <- which(substr(x$ID_MUNICIP, 1, 6)=='410690')
        } else {
            ii <- which(x[,j]=='410690')
        }
            return(x[ii, ])
        })
    } else {
        sel.d <- alld 
    }
}

### Resultados de PCR, por ano - Brasil
sapply(alld, function(d) 
    table(factor(d$CLASSI_FIN, c(1:5, 9), lab.class.fin)))
### Resultados de PCR, por ano - Paraná
sapply(sel.d, function(d) 
    table(factor(d$CLASSI_FIN, c(1:5, 9), lab.class.fin)))

table(sel.d[[8]]$Classificação,
            sel.d[[8]]$Evolução)

### labels for the leves of the PCR_RESUL variable 
lpcrres <- c('Detectável', 'N detectável', 'Inconclusivo',
             'Não realizado', 'Ag. resultado', 'Ignorado')
sel.d[[8]]$resPCR <-
    factor(sel.d[[8]]$PCR_RESUL, c(1:5,9), lpcrres)

t2 <- table(sel.d[[8]]$resPCR,
      as.character(sel.d[[8]]$Classificação))
t2
colnames(t2) <- c('COVID', 'Influ', 'NEspec', 'OEtiol',  'OResp')
rownames(t2) <- c('Detectável', 'NDetect', 'Inconclus', 'NRealiz', 'AgResult', 'Ignorado')
t2

tco <- table(as.character(sel.d[[8]]$Classificação),
             sel.d[[8]]$Evolução)
tco
rownames(

pcr20 <- table(
    sel.d[[8]]$resPCR, substr(sel.d[[8]]$SEM_NOT, 5, 6))
pcr20

wc.freq <- table(
    substr(unlist(lapply(sel.d, function(d) d$SEM_NOT)), 5, 6),
    unlist(lapply(sel.d, function(d)
        factor(d$CLASSI_FIN, c(1:5, 9), lab.class.fin))), 
    substr(unlist(lapply(sel.d, function(d) d$SEM_NOT)), 1, 4))

dim(wc.freq)
dimnames(wc.freq)

### soma no ano
apply(wc.freq, 3, apply, 2, sum)

### stats for previous years 
prev.y.stats <- lapply(1:6, function(j)
    apply(wc.freq[,j,1:7], 1, summary, na.rm=TRUE))
str(prev.y.stats)

### define labels for the log10 scale
log(yl0 <- c(1, 3), 10)
yl <- list(l=c(yl0, 10*yl0, 100*yl0, 1000*yl0, 1e4*yl0))
yl$y <- log(yl$l, 10)
yl$ll <- gsub('000000', '1M', as.character(yl$l))
yl$ll <- gsub('00000', '100K', yl$ll)
yl$ll <- gsub('0000', '0K', yl$ll)
yl$ll <- gsub('000', 'K', yl$ll)
yl

### define colors 
cores0 <- c('red4', 'blue', 'green4', 'black', 'orange', 'gray')
cores <- c(cores0[1:5], rep(NA,3), cores0[6])
length(cores)

par(mfcol=c(2,1), mar=c(3,3,0,0), mgp=c(2,0.5,0))
for (log10 in FALSE) { ##c(FALSE, TRUE)) {
    par(mar=c(1.5,3,0,0))
    if (log10) {
        plot(log(1+wc.freq[,1,1], 10), axes=FALSE,
             ylim=range(log(1+wc.freq, 10)),
             ylab='Número de SRAGs por semana', type='n')
    } else {
        plot(wc.freq[,1,1], axes=FALSE,
             ylim=range(wc.freq),
             ylab='Número de SRAGs por semana', type='n')
    }
    yy <- c(prev.y.stats[[1]][1, ],
            rev(prev.y.stats[[1]][6, ]),
            prev.y.stats[[1]][1,1])
    if (log10)
        yy <- log(ifelse(yy==0, 0.5, yy), 10)
    polygon(c(1:53, 53:1, 1), yy, 
            col=rgb(1, 0.5, 0.5, 0.5))
    yy <- c(prev.y.stats[[2]][1, ],
            rev(prev.y.stats[[2]][6, ]),
            prev.y.stats[[2]][1,1])
    if (log10)
        yy <- log(ifelse(yy==0, 0.5, yy), 10)
    polygon(c(1:53, 53:1, 1), yy, 
            col=rgb(0.5, 0.5, 1, 0.5))
    yy <- c(prev.y.stats[[3]][1, ],
            rev(prev.y.stats[[3]][6, ]),
            prev.y.stats[[3]][1,1])
    if (log10)
        yy <- log(ifelse(yy==0, 0.5, yy), 10)
    polygon(c(1:53, 53:1, 1), yy, 
            col=rgb(0.5, 1, 0.5, 0.5))
    yy <- c(prev.y.stats[[4]][1, ],
            rev(prev.y.stats[[4]][6, ]),
            prev.y.stats[[4]][1,1])
    if (log10)
        yy <- log(ifelse(yy==0, 0.5, yy), 10)
    polygon(c(1:53, 53:1, 1), yy, 
            col=gray(0.7, 0.5))
    for (j in 1:5) {
        yy <- wc.freq[1:23, j, 8]
        if (log10)
            yy <- log(ifelse(yy<0.5, 0.5, yy), 10) 
        lines(yy, col=cores[j], lwd=3)
    }
    axis(1)
    if (log10) {
        axis(2, yl$y, yl$ll, las=1) 
        abline(h=yl$y, lty=2, col=gray(0.5, 0.5))
    } else {
        axis(2)
        abline(h=pretty(par()$usr[3:4]), col=gray(0.5, 0.5))
    }
    legend('topright', title='Resultado de teste', lwd=3,
           lab.class.fin[1:5], col=cores0[1:5], lty=1, bty='n')
    legend('topleft', c('anos anteriores', '2020'),
           lty=c(NA,1), fill=c(gray(0.7,0.5), 0),
           border=c(gray(0.7,0.5), 0), bty='n')
    par(mar=c(3,3,0,0))
    if (log10) {
        plot(log(ifelse(pcr20[1, ]==0, 0.5, pcr20[1, ]), 10), 
             xlab='Semana epidemiológica', axes=FALSE, xlim=c(1,53), 
             ylab='Número de SRAGs por semana', type='n',
         ylim=log(range(pcr20+0.5, na.rm=TRUE), 10))
        axis(1); axis(2, yl$y, yl$ll, las=1)
        abline(h=yl$y, lty=2, col=gray(0.5, 0.5))
    } else {
        plot(pcr20[1, ], type='n', axes=FALSE, 
             xlab='Semana epidemiológica', 
         ylab='Número de SRAGs por semana', 
         xlim=c(1,53), ylim=range(pcr20, na.rm=TRUE)) 
        axis(1); axis(2) 
        abline(h=pretty(par()$usr[3:4]), col=gray(0.5, 0.5))
    }
    for (j in 1:nrow(pcr20)) {
        yy <- pcr20[j, ]
        if (log10)
            yy <- log(ifelse(yy==0, 0.5, yy), 10)
        lines(yy, col=cores0[j], lwd=3)
    }
    legend('right', rownames(pcr20), col=cores0, lty=1, lwd=3,
           title='Result. PCR', bty='n')
}


