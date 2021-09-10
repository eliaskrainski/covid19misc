
if (FALSE)
    setwd('..')

options(timeout=60*10)

u0 <- 'http://dadosabertos.c3sl.ufpr.br/curitiba/CasosCovid19/'

k <- 0
repeat {
    ufl <- paste0(u0,  Sys.Date() + k,
                  '_Casos_Covid_19_-_Base_de_Dados.csv')
    if (class(try(download.file(ufl, 'data/casosCuritibaSM.csv'),
                  TRUE))=='try-error') {
        k <- k-1
    } else {
        break
    }
}

library(data.table)

dcwb <- as.data.frame(fread(
    'data/casosCuritibaSM.csv', encoding='Latin-1', dec=','))

dcwb$date <- as.Date(dcwb[,2], '%d/%m/%Y')
summary(dcwb$date)

if (!any(ls()=='alldates'))
    alldates <- gsub('-', '', seq(as.Date('20200121', '%Y%m%d'),
                    Sys.Date(), 1))

dcwb$fdate <- factor(gsub('-', '', 
                          dcwb$date,
                          fixed=TRUE), alldates)
tail(dcwb)

if(FALSE) {

    summary(dcwb$IDADE)

    table(cut(dcwb$IDADE, c(0, 30, 60, Inf), right=F))
    table(Idade20 <- cut(dcwb$IDADE, c(0, 20, 40, 60, 80, Inf), right=F))
    table(dcwb$g3idade <- cut(dcwb$IDADE, c(0, 30, 50, 70, Inf), right=F))

    sexo <- toupper(dcwb$SEXO)
    desfecho <- ifelse(dcwb$ENC=='', 'Investigação',
                ifelse(dcwb$ENC=='RECUPERADO', 'Recuperado', 'Óbito'))

    load('data/w2pop.RData')

    icwbp <- which(dimnames(w2pop)[[1]]=='4106902 Curitiba - PR')
    icwbp

    pcwb <- w2pop[icwbp,, ]
    pcwb

    pop5 <- aggregate(pcwb, by=list(Idade20=rep(1:5, c(4,4,4,4,3))), sum)[,-1]
    rownames(pop5) <- levels(Idade20)

    table(Idade20, desfecho)
    rowSums(pop5)
    round(100*table(Idade20, desfecho)/rowSums(pop5), 2)
    round(100*prop.table(table(Idade20, desfecho), 1), 2)

    table(Idade20, sexo)

    round(100*table(Idade20, sexo)/pop5, 2)

    round(100*prop.table(table(Idade20, sexo), 1), 2)

    round(100*prop.table(table(Idade20, desfecho), 1), 2)

    round(100*prop.table(table(Idade20, desfecho, sexo), 1), 2)

    ti3n <- table(substr(dcwb$fdate, 1, 6),
                  dcwb$g3idade, dcwb$ENC)
    dim(ti3n)
    dimnames(ti3n)
    (ng <- dim(ti3n)[2])

    ti3n[,,2]

    tx <- as.Date(paste0(dimnames(ti3n)[[1]], '15'), '%Y%m%d')
    it <- 4:(length(tx)-2)

    n.age <- apply(ti3n, 2, rowSums)

    png('figures/curitiba_age.png', 2000, 1000, res=150)
    par(mfrow=c(2,2), mar=c(1.5,3.5,0.0,0.5), mgp=c(2.5,0.5,0), las=1)
    plot(tx, n.age[,1], type='l', xlab='',
         ylab=c('', '#', '#')[k],
         ylim=range(n.age), xlim=range(tx[it]))
    for (i in 2:ng) lines(tx, n.age[,i], col=i)
    legend('topleft', dimnames(ti3n)[[2]], col=1:ng, lty=1,
           bty='n', title='Casos conf.')
    k <- 2
    plot(tx, ti3n[,1,k], type='l', xlab='',
         ylab=c('', '#', '#')[k],
         ylim=range(ti3n[it,,k]), xlim=range(tx[it]))
    for (i in 2:ng) lines(tx, ti3n[,i,k], col=i)
    legend('topleft', dimnames(ti3n)[[2]], col=1:ng, lty=1,
           bty='n', title=dimnames(ti3n)[[3]][k])
    plot(tx, ti3n[,1,k]/n.age[,k], type='l', xlab='', ylab='óbitos/casos conf.',
         ylim=range(ti3n[it,,k]/n.age[it,]), xlim=range(tx[it]))
    for (i in 2:ng) lines(tx, ti3n[,i,k]/n.age[,i], col=i)
    legend('top', dimnames(ti3n)[[2]], col=1:ng, lty=1,
           bty='n', title=dimnames(ti3n)[[3]][k])
    plot(tx[it],
         tapply(dcwb$IDADE, substr(dcwb$fdate, 1, 6),
                mean, na.rm=TRUE)[it],
         xlab='', ylab='Idade',
         type='l', lwd=2, ylim=c(35, 75))
    lines(tx[it],
          tapply(dcwb$IDADE[dcwb$ENC=='ÓBITO CONF'],
                 substr(dcwb$fdate, 1, 6)[dcwb$ENC=='ÓBITO CONF'],mean)[it], col=2)
    legend('center', c('Casos conf.', 'Óbitos conf.'),
           col=c(1,2), lwd=2, title='Idade média')
    dev.off()

    if(FALSE)
        system('eog figures/curitiba_age.png &')

}

t3 <- table(dcwb$fdate, dcwb$ENCE)
str(t3)

t3a <- apply(t3, 2, cumsum)
str(t3a)

nc3 <- rowSums(t3a)
tail(nc3)

(ntail <- which(diff(rev(nc3), na.rm=TRUE)<0)[1])

t3a[-ntail:0+nrow(t3a), ]
tail(diff(c(0, t3a[,2])), ntail)

bsm <- read.csv('data/boletinsSMCuritiba.csv')
bsm$Date <- as.Date(as.character(bsm$date), '%Y%m%d')
head(bsm)
tail(bsm)

bsm$casos <- bsm$ativos + bsm$obitos + bsm$recuperados
bsm$fdate <- factor(bsm$date, alldates)
tail(diff(bsm$casos))

wbsm <- lapply(bsm[c('casos', 'obitos')], tapply, 
               bsm[c('fdate')], as.integer)
str(wbsm)

jj0 <- which(bsm$Date>tail(dcwb$date, ntail)[1])
jj0

jj <- pmatch(bsm$date[jj0], rownames(t3))
jj

if(FALSE) {

    png('figures/casos-ativos-CuritibaSM.png', 750, 500)
    par(mfrow=c(1,1), mar=c(4, 3.5, .5, 3.5), mgp=c(2.5, 0.5, 0), las=2)
    with(bsm, plot(Date, ativos, axes=FALSE,
         type='h', lwd=6, ylim=c(0, max(ativos, na.rm=TRUE)),
         xlab='', ylab='Casos ativos'))
    axis(2, pretty(c(0, bsm$ativos), 10), las=1)
    axis(1, pretty(bsm$Date, 10),
         format(pretty(bsm$Date, 10), '%d%b%y'), las=2)
    o.t <- diff(bsm$obitos)
    y2 <- o.t/max(o.t, na.rm=TRUE)
    points(bsm$Date[-1], y2*max(bsm$ativos), type='o', 
           pch=8, col=2, cex=2, lwd=2)
    axis(4, max(bsm$ativos)*pretty(c(0, max(o.t)), 10)/max(o.t),
         pretty(c(0, max(o.t)), 10), las=1, line=0)
    mtext('Óbitos', 4, 2, las=3)
    legend('topleft', c('Casos ativos', 'Óbitos'),
           pch=8, pt.cex=c(0,2), lwd=2, col=1:2, bty='n')
    dev.off()

    if(FALSE)
        system('eog figures/casos-ativos-CuritibaSM.png &')

}

t3a[jj, ] <- as.matrix(bsm[jj0, 2:4])


if (!any(is.na(t3a[-2:0 + nrow(t3a), ]))) {
    if (any((t3a[nrow(t3a)-1, 2:3] - 
             t3a[nrow(t3a)-2, 2:3])<0))
        t3a[nrow(t3a)-1, ] <- t3a[nrow(t3a)-2, ]
    if (any((t3a[nrow(t3a), 2:3] - 
             t3a[nrow(t3a)-1, 2:3])<0))
        t3a[nrow(t3a), ] <- t3a[nrow(t3a)-1, ]
    if(sum(t3a[nrow(t3a), 1:3])==sum(t3a[nrow(t3a)-1, 1:3]))
        t3a[nrow(t3a), 1:3] <- NA
}

tail(t3a, 14)
tail(rowSums(t3a), 14)

wcwb <- list(casos=rowSums(t3a), 
             obitos=t3a[,2])
str(wcwb)

##plot(diff(c(0,wcwb[[1]])))

print(sapply(wcwb, function(x) tail(x, 15)))

print(sapply(wcwb, function(x) tail(diff(c(0,x)), 15)))


if(FALSE){

    tno <- as.integer(difftime(
        as.Date(dcwb$'DATA.ÓBITO', '%d/%m/%Y'),
        dcwb$date, units='days'))

    qtno <- quantile(tno, na.rm=TRUE)
    qtno
    mmtno <- c(mean(tno, na.rm=TRUE),
               mean(tno[tno>0], na.rm=TRUE))

    h <- hist(
        tno, qtno[1]:(qtno[5]+1)-0.5, 
        col=gray(.5), border=gray(.5))
    segments(qtno[2:4], 25, qtno[2:4], 35)
    text(qtno[2:4], rep(30, 3), format(qtno[2:4]))
    rug(mmtno, lwd=2, col=2)
    mtext(format(mmtno, digits=4),
          1, 1,at=mmtno, las=2)

    plot(h, xlim=c(-10, 70),
         col=gray(.5), border=gray(.5))
    segments(qtno[2:4], 25, qtno[2:4], 35)
    text(qtno[2:4], rep(30, 3), format(qtno[2:4]))
    rug(mmtno, lwd=2, col=2)
    mtext(format(mmtno, digits=4),
          1, 1,at=mmtno, las=2)

}


if (FALSE) {

    date <- as.Date(names(wcwb[[1]]), '%Y%m%d')
    mm <- substr(date,1,7)
    
    ww <- as.integer(difftime(date, min(date), un='days'))%/%7

    nww <- list(tapply(diff(c(0, wcwb[[1]])), ww, sum),
                tapply(diff(c(0, wcwb[[2]])), ww, sum))
    
    plot(nww[[1]], log='y', ylim=c(0.7, max(nww[[1]])))
    points(nww[[2]], col=2, pch=8)

    plot(nww[[2]]/nww[[1]])

    plot(nww[[2]][4:(length(nww[[1]])-1)]/
         nww[[1]][2:(length(nww[[2]])-3)])
    points(nww[[2]][3:(length(nww[[1]])-1)]/
         nww[[1]][2:(length(nww[[2]])-2)], col=2)
    
}
