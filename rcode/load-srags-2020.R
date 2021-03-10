if (FALSE)
    setwd('..')

options(width=70)

diff(c('0630'=356189, '0707'=393980,
       '0714'=436524, '0721'=476918,
       '0729'=522263, '0803'=540746,
       '0810'=575936,
       '1102'=882148, '1110'=886836,
       '1122'=945461, '3011'=973111,
       '1214'=1029684,
       '0104'=1103284, '0111'=1136682))

(fls <- system('ls data/INFLUD-*202*.csv', TRUE))
if (length(fls)>0) {
    file <- fls[tail(order(as.Date(substr(
        fls, 13, 22), '%d-%m-%Y')), 1)]
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

    library(readr)
    system.time(srags20 <- as.data.frame(read_csv2(file)) )
    dim(srags20)

    srags20$dSintPrinc <- as.Date(srags20$DT_SIN_PRI, '%d/%m/%Y')
    srags20$dNotifica <- as.Date(srags20$DT_NOTIFIC, '%d/%m/%Y')
    srags20$dInterna <- as.Date(srags20$DT_INTERNA, '%d/%m/%Y')
    srags20$dDigita <- as.Date(srags20$DT_DIGITA, '%d/%m/%Y')
    srags20$dPCR <- as.Date(srags20$DT_PCR, '%d/%m/%Y')
    srags20$dEvol <- as.Date(srags20$DT_EVOLUCA, '%d/%m/%Y')
    srags20$dEncerra <- as.Date(srags20$DT_ENCERRA, '%d/%m/%Y')

    
    
### labels da variavel CLASSI_FIN
    lab.cf <- c('NA', 'Influenza', 'ORespir', 
                'OEtiol', 'NEspec', 'COVID19')

    table(is.na(srags20$CLASSI_FIN))
    table(srags20$CLASSI_FIN)
    
    srags20$Classificação  <- factor(
        ifelse(is.na(srags20$CLASSI_FIN), 0,
               srags20$CLASSI_FIN), c(0:5), lab.cf)
    table(srags20$Classificação,
          ifelse(is.na(srags20$CLASSI_FIN), 0, srags20$CLASSI_FIN))

    lab.ev <- c('NA', 'Cura', 'Óbito', 'Indef')
    table(is.na(srags20$EVOLUCAO))
    table(srags20$EVOLUCAO)
    srags20$Evolução <- factor(
        ifelse(is.na(srags20$EVOLUCAO), 0,
               srags20$EVOLUCAO), c(0,1,2,9), lab.ev)
    table(srags20$Evolução,
          ifelse(is.na(srags20$EVOLUCAO), 0, srags20$EVOLUCAO))

    srags20$anoS <- substr(srags20$dSintPrinc, 1, 4)
    table(srags20$anoS, srags20$Classificação)
    table(srags20$Evolu, srags20$anoS)

    addmargins(with(srags20[(srags20$Classificação=='COVID19'),],
        table(Evolução, anoS)))

    table(srags20$evolucao <- factor(ifelse(
              srags20$Evolução=='NA', 'Indef', as.character(srags20$Evolução)),
              c('Cura', 'Óbito', 'Indef')), 
          srags20$Evolução)
    t0 <- with(srags20[(srags20$Classificação=='COVID19'),],
        table(evolucao, UTI))
    addmargins(t0)
    
    t1 <- cbind(t0[, 1, drop=FALSE], rowSums(t0[, 2:3]))
    addmargins(t1)

    srags20$diasInt <- as.integer(difftime(
        srags20$dEvol, srags20$dInterna, units='days'))
    tapply(srags20$diasInt, srags20$UTI, summary)

    tapply(srags20$diasInt, srags20$UTI, sum, na.rm=TRUE)
    sum(tapply(srags20$diasInt, srags20$UTI, sum,
               na.rm=TRUE) * c(1800, 800, 1000))*1e-6
    
    table(srags20$UTI)
    addmargins(with(srags20[srags20$Classificação=='COVID19',],
                    table(Evolução, UTI)))
    
    
    srags20$Idade <- srags20$NU_IDADE_N/
        (c(365.25, 12, 1)[as.integer(substr(srags20$COD_IDADE, 1, 1))])

    
    table(srags20$gIdade5 <- cut(
              srags20$Idade, c(5*(0:18), 100, 199), right=FALSE))
    table(srags20$gIdade10 <- cut(
              srags20$Idade, c(10*(0:8), 100, 199), right=FALSE))
    table(srags20$gIdade3 <- cut(
              srags20$Idade, c(0, 10, 30, 40, 50, 60, 70, 80, 90, 199), right=FALSE))
    i3m <- c(5, 20, 35, 35, 55, 65, 75, 85, 95)

    mm.ev <- with(srags20[srags20$Classificação=='COVID19',],
                  table(substr(dEvol, 1, 7), Evolução))
    mm.ev

    x.date <- as.Date(paste0(dimnames(id.ev)[[1]], '-15'))

    p.id.ev <- sapply(c(0.025, 0.5, 0.975), function(p)
        qbeta(p, 1 + id.ev[,3], 1 + rowSums(id.ev[,-3])))
    
    plot(x.date, id.ev[,3]/rowSums(id.ev),
         ylim=0:1, type='n')
    segments(x.date, p.id.ev[,1], x.date, p.id.ev[,3])
    points(x.date, p.id.ev[,2], pch=19, col=2)
    
    id10ev <- with(srags20[srags20$Classificação=='COVID19',],
                  table(substr(dEvol, 1, 7), gIdade10, Evolução))
    dim(id10ev)
    dimnames(id10ev)

    apply(id10ev, 3, colSums)
    apply(id10ev, 3, rowSums)

    p.ob.id10 <- apply(id10ev, 1, function(m) m[,3]/rowSums(m))
    dim(p.ob.id10)
    dimnames(p.ob.id10)

    p1ob.id10 <- apply(id10ev, 1, function(m)
        pbeta(0.0210, 1+m[,3], 1+rowSums(m)-m[,3]))
    p2ob.id10 <- apply(id10ev, 1, function(m)
        pbeta(0.9710, 1+m[,3], 1+rowSums(m)-m[,3]))

    plot(x.date, p.ob.id10[1,], pch=8,
         ylim=range(p1ob.id10, p2ob.id10, na.rm=TRUE))
    segments(x.date, p1ob.id10[1,], x.date, p2ob.id10[1,])

    system.time(save('srags20',
                     file=rfile,
                     compress='xz'))

}
