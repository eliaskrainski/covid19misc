if (FALSE)
    setwd('..')

options(width=70)

diff(c('0630'=356189, '0707'=393980,
       '0714'=436524, '0721'=476918,
       '0729'=522263, '0803'=540746,
       '0810'=575936))

fls <- system('ls data/INFLUD*2020.csv', TRUE)
file <- fls[tail(order(as.Date(substr(
    fls, 13, 22), '%d-%m-%Y')), 1)]
(rfile <- gsub('.csv', '.RData', file))

if (file.exists(rfile)) {

    load(rfile)

} else {
    
    system.time(srags20 <- read.csv2(file)) 
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

    
    srags20$Idade <- srags20$NU_IDADE_N/
        (c(365.25, 12, 1)[as.integer(substr(srags20$COD_IDADE, 1, 1))])

    
    table(srags20$gIdade5 <- cut(
              srags20$Idade, c(5*(0:18), 100, 199), right=FALSE))
    table(srags20$gIdade10 <- cut(
              srags20$Idade, c(10*(0:8), 100, 199), right=FALSE))

    save('srags20',
         file=rfile,
         compress='xz')

}
