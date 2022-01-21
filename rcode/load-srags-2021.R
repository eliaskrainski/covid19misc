if (FALSE)
    setwd('..')

options(width=70)

(fls <- system('ls data/INFLUD21*.csv', TRUE))
if (length(fls)>0) {
    file <- fls[tail(order(as.Date(substr(
        fls, 15, 24), '%d-%m-%Y')), 1)]
    (rfile <- gsub('.csv', '.RData', file))
} else {
    rfls <- system('ls data/INFLUD*.RData', TRUE)
    rfile <- rfls[tail(order(as.Date(substr(
        rfls, 13, 22), '%d-%m-%Y')), 1)]
}
rfile

if (file.exists(rfile)) {

    load(rfile)

} else {

    library(data.table)

    system.time(srags21 <- as.data.frame(fread(file)))

    srags21$dSintPrinc <- as.Date(srags21$DT_SIN_PRI, '%d/%m/%Y')
    srags21$dNotifica <- as.Date(srags21$DT_NOTIFIC, '%d/%m/%Y')
    srags21$dInterna <- as.Date(srags21$DT_INTERNA, '%d/%m/%Y')
    srags21$dDigita <- as.Date(srags21$DT_DIGITA, '%d/%m/%Y')
    srags21$dPCR <- as.Date(srags21$DT_PCR, '%d/%m/%Y')
    srags21$dEvol <- as.Date(srags21$DT_EVOLUCA, '%d/%m/%Y')
    srags21$dEncerra <- as.Date(srags21$DT_ENCERRA, '%d/%m/%Y')
    
### labels da variavel CLASSI_FIN
    lab.cf <- c('NA', 'Influenza', 'ORespir', 
                'OEtiol', 'NEspec', 'COVID19')

    table(is.na(srags21$CLASSI_FIN))
    table(srags21$CLASSI_FIN)
    
    srags21$Classificação  <- factor(
        ifelse(is.na(srags21$CLASSI_FIN), 0,
               srags21$CLASSI_FIN), c(0:5), lab.cf)
    table(srags21$Classificação,
          ifelse(is.na(srags21$CLASSI_FIN), 0, srags21$CLASSI_FIN))

    lab.ev <- c('NA', 'Cura', 'Óbito', 'Indef')
    table(is.na(srags21$EVOLUCAO))
    table(srags21$EVOLUCAO)
    srags21$Evolução <- factor(
        ifelse(is.na(srags21$EVOLUCAO), 0,
               srags21$EVOLUCAO), c(0,1,2,9), lab.ev)
    table(srags21$Evolução,
          ifelse(is.na(srags21$EVOLUCAO), 0, srags21$EVOLUCAO))

    srags21$anoS <- substr(srags21$dSintPrinc, 1, 4)
    table(srags21$anoS, srags21$Classificação)
    table(srags21$Evolu, srags21$anoS)

    addmargins(with(srags21[(srags21$Classificação=='COVID19'),],
        table(Evolução, anoS)))

    table(srags21$evolucao <- factor(ifelse(
              srags21$Evolução=='NA', 'Indef', as.character(srags21$Evolução)),
              c('Cura', 'Óbito', 'Indef')), 
          srags21$Evolução)
    t0 <- with(srags21[(srags21$Classificação=='COVID19'),],
        table(evolucao, UTI))
    addmargins(t0)
    
    t1 <- cbind(t0[, 1, drop=FALSE], rowSums(t0[, 2:3]))
    addmargins(t1)

    srags21$diasInt <- as.integer(difftime(
        srags21$dEvol, srags21$dInterna, units='days'))
    tapply(srags21$diasInt, srags21$UTI, summary)

    tapply(srags21$diasInt, srags21$UTI, sum, na.rm=TRUE)
    sum(tapply(srags21$diasInt, srags21$UTI, sum,
               na.rm=TRUE) * c(1800, 800, 1000))*1e-6
    
    table(srags21$UTI)
    addmargins(with(srags21[srags21$Classificação=='COVID19',],
                    table(Evolução, UTI)))
    
    
    srags21$Idade <- srags21$NU_IDADE_N/
        (c(365.25, 12, 1)[as.integer(substr(srags21$COD_IDADE, 1, 1))])

    
    table(srags21$gIdade5 <- cut(
              srags21$Idade, c(5*(0:18), 100, 199), right=FALSE))
    table(srags21$gIdade10 <- cut(
              srags21$Idade, c(10*(0:8), 100, 199), right=FALSE))
    table(srags21$g4idade <- cut(
              srags21$Idade, c(0, 30, 50, 70, Inf)))
    table(cut(srags21$Idade, c(0, 50, 60, 70, 80, Inf), right=FALSE))

    system.time(save('srags21',
                     file=rfile,
                     compress='xz'))

}

if(FALSE) {

    library(lubridate)
    table(epiweek(srags21$dEvol), srags21$Evol)

    with(srags21[which(substr(srags21$CO_MUN_RES,1,2)=='41'),],
         table(epiweek(dEvol), Evolução))

}

table(srags21$Classificação)
table(srags21$Evolução)

ii.cov <- which((srags21$Classificação=='COVID19'))
ii.ob <- which((srags21$Classificação=='COVID19') &
               (srags21$Evolução=='Óbito'))

wob.srags <- table(srags21$CO_MUN_RES[ii.ob],
                   substr(srags21$dSintPrinc[ii.ob], 1, 10))
dim(wob.srags)

wob.srags[1:3, 1:5]
wob.srags[1:3, -4:0+ncol(wob.srags)]

if(FALSE)
    plot(as.Date(colnames(wob.srags)), colSums(wob.srags), type='h')

if(FALSE) {

    q.age <- c(0,30,50,60,70,80,90,Inf)
    m.age <- c(15, 40, 55, 65, 75, 85, 95)
    
    nw.age <- table(substr(srags21$dSintPrinc[ii.cov], 1, 10),
                    cut(srags21$Idade[ii.cov], q.age, right=FALSE))
    colSums(nw.age)

    datex <- as.Date(rownames(nw.age))
    nt <- length(datex)
    xlim <- range(datex) + c(0, -30)

    ow.age <- table(factor(substr(srags21$dSintPrinc[ii.ob], 1, 10),
                           as.character(datex)), 
                    cut(srags21$Idade[ii.ob], q.age, right=FALSE))
    colSums(ow.age)

    c(sum(nw.age), sum(ow.age))
    
    p.acc.age <- t(rbind(0, apply(nw.age, 1, cumsum)))/rowSums(nw.age)
    p.aco.age <- (0.001 + t(rbind(0, apply(ow.age, 1, cumsum))))/(0.001 + rowSums(ow.age))

    par(mfrow=c(1,2), mar=c(3.5, 3.5, 0.5, 0.5), mgp=c(2, 0.5, 0), las=1)
    plot(datex, nw.age[,1]/rowSums(nw.age),
         type='n', ylim=0:1, xlim=xlim, 
         xlab='', ylab='Proporção')
    for(k in 1:ncol(nw.age)) {
        c0 <- (k-1)/(ncol(nw.age)-1)
        polygon(datex[c(1:nt, nt:1, 1)],
                c(p.acc.age[, k], p.acc.age[nt:1, k+1], p.acc.age[1,k]),
                col=rgb(c0, 1-2*abs(c0-0.5), 1-c0))
    }
    plot(datex, ow.age[,1]/rowSums(ow.age),
         type='n', ylim=0:1, xlim=xlim, 
         xlab='', ylab='Proporção')
    for(k in 1:ncol(ow.age)) {
        c0 <- (k-1)/(ncol(ow.age)-1)
        polygon(datex[c(1:nt, nt:1, 1)],
                c(p.aco.age[, k], p.aco.age[nt:1, k+1], p.aco.age[1,k]),
                col=rgb(c0, 1-2*abs(c0-0.5), 1-c0))
    }

    table(cut(srags21$Idade[ii.cov], q.age, right=FALSE))
    table(cut(srags21$Idade[ii.ob], q.age, right=FALSE))

    p.age <- as.numeric(
        table(cut(srags21$Idade[ii.ob], q.age, right=FALSE))/
        table(cut(srags21$Idade[ii.cov], q.age, right=FALSE)))
    p.age

    tapply(srags21$Idade[ii.cov],
           cut(srags21$Idade[ii.cov], q.age, right=FALSE), mean)
    tapply(srags21$Idade[ii.ob],
           cut(srags21$Idade[ii.ob], q.age, right=FALSE), mean)

    par(mfrow=c(1,1), mar=c(3.5, 3.5, 0.5, 0.5), mgp=c(2, 0.5, 0), las=1)
    plot(c(22, 41, 55, 65, 74, 84, 93),
         p.age, type='o', ylim=0:1,
         xlab='Idade', ylab='óbitos/SRAGs')
    abline(v=10*(0:10), h=0:10/10, lty=2, col=gray(0.7))

    plot(datex, rowSums(nw.age), type='l', xlim=xlim)
    lines(datex, rowSums(ow.age), col=2)

    ng <- ncol(nw.age)

    par(mfrow=c(1,1), mar=c(2,3.5,0,0.5), mgp=c(2,0.5,0), las=1)
    plot(datex, rowSums(ow.age)/rowSums(nw.age),
         type='l', xlim=xlim, ylim=c(0,0.65), lwd=4,
         xlab='', ylab='Óbito/SRAG', bty='n')
    for(k in 1:ng) {
        y <- ow.age[,k]
        n <- nw.age[,k]
        lines(datex, (1 + y)/(1 + n), col=k, lty=3, lwd=2)
    }
    legend('bottom', colnames(ow.age), col=1:ng, lty=3, lwd=2, bty='n')

}
