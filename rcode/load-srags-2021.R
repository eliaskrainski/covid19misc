if (FALSE)
    setwd('..')

options(width=70)

(fls <- system('ls data/INFLUD21*2021.csv', TRUE))
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

    library(readr)
    ct <- ''
    system.time(srags21 <- as.data.frame(read_csv2(
                    file)))
    dim(srags21)

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
