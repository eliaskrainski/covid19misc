if (FALSE)
    setwd('..')

options(width=70)

if (file.exists('data/sragsl-0919.RData')) {
    
    load('data/sragsl-0919.RData')

} else {
    
### file names
    fls <- paste0('data/',
                  c(paste0('influd', sprintf('%02d', 9:18),
                           '-limpo-final.csv'),
                    'influd19-limpo-27.04.2020-final.csv'))
    names(fls) <- 2009:2019
    
### read the datasets
    system.time(sragsl <- lapply(fls, read.csv2)) ## ~30secs
    
    system.time(save(
        'sragsl',
        file='data/sragsl-0919.RData',
        compress='xz'))

}

fl20 <- paste0('data/', 'INFLUD-21-07-2020.csv')

system.time(sragsl[[length(sragsl)+1]] <- read.csv2(fl20))
names(sragsl)[length(sragsl)] <- '2020'

t(sapply(sragsl, dim))

for (j in 1:length(sragsl))
    sragsl[[j]]$Evolução <- factor(
        ifelse(is.na(sragsl[[j]]$EVOLUCAO), 0,
               sragsl[[j]]$EVOLUCAO),
        c(1, 2, 0, 9), 
        c('Cura', 'Óbito', 'NA', 'Indef'))

### labels da variavel CLASSI_FIN
l.class.fin <- c('NA', 'Influ', 'OResp', 'OEtiol',
                 'NEspec', 'Covid')
lab.class.fin <- c('Não avaliado',
                   'Influenza', 'Outras respiratórias',
                   'Outras etiologias', 'Não especificada',
                   'SARS-COV-2') 

for (j in 1:length(sragsl)) {
    y <- sragsl[[j]]$CLASSI_FIN
    y[is.na(y)] <- 0
    if (j==length(sragsl)) {        
        sragsl[[j]]$Classificação <- factor(
            y, c(0:5, 9),
            l.class.fin[c(1:6, 5)])
    } else {
        sragsl[[j]]$Classificação <- factor(
            y, c(0:6, 9),
            l.class.fin[c(1:5, 5, 6, 5)])
    }
}

### Define variável idade
for (j in 1:length(sragsl)) {    
    if (any(names(sragsl[[j]])=='COD_IDADE')) {
        sragsl[[j]]$Idade  <- sragsl[[j]]$NU_IDADE_N/
            (c(365.25, 12, 1)[as.integer(substr(
                  sragsl[[j]]$COD_IDADE, 1, 1))])
    } else {
        sragsl[[j]]$Idade  <- as.integer(substring(
            sragsl[[j]]$NU_IDADE_N,2))/
            (c(1, 365.25, 12, 1)[as.integer(substr(
                  sragsl[[j]]$NU_IDADE_N, 1, 1))])
    }
}


(bid <- c(0, 1, 5*(1:16), 90, Inf))
(mid <- c(0.5, 3, 5*(2:16)-2.5, 85, 95))

### define variável grupo de idade
for (j in 1:length(sragsl)) {
    sragsl[[j]]$gIdade <- cut(
        sragsl[[j]]$Idade, bid, right=FALSE)
}
