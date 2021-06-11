
if(FALSE)
    setwd('..')

t0 <- Sys.time()

load('data/dMunDateDose.RData')
ls()

dim(dMunDateDose)
head(dMunDateDose)

sum(dMunDateDose$N)

tapply(dMunDateDose$N, dMunDateDose$vacina_descricao_dose, sum)

source('rcode/ocommon.R')
head(uf,2)

cUF <- substr(dMunDateDose$paciente_endereco_coibgemunicipio,1,2)
dMunDateDose$UF <- uf$UF[pmatch(cUF, rownames(uf), duplicates.ok=TRUE)]
                              
tapply(dMunDateDose$N, dMunDateDose$UF, sum)

Date <- seq(as.Date('20200121', '%Y%m%d'), Sys.Date(), 1)
alldates <- gsub('-', '', as.character(Date))

dMunDateDose$fdate <- factor(
    gsub('-', '', substr(dMunDateDose$vacina_dataaplicacao, 1, 10)), 
    alldates)

head(dMunDateDose)
tail(dMunDateDose)

dlab <- sort(unique(dMunDateDose$vacina_descricao_dose))
names(dlab) <- gsub(' ', '', gsub('ª', '', dlab))
names(dlab)[1:2] <- paste0(
    substring(names(dlab)[1:2], 2),
    substr(names(dlab)[1:2], 1, 1))
dlab

dMunDateDose[,1] <- as.factor(dMunDateDose[,1])

system.time(wvac.mu <- lapply(dlab[1:2], function(d) {
    ii <- which(dMunDateDose[,3] %in% d)
    tapply(dMunDateDose$N[ii],
           dMunDateDose[ii, c(1,6)], sum)
}))

str(wvac.mu)

sapply(wvac.mu, sum, na.rm=TRUE)

library(data.table)
cmurs <- as.data.frame(
    fread('data/HIST_PAINEL_COVIDBR.csv',
          select=c('codmun', 'codRegiaoSaude',
                   'nomeRegiaoSaude')))
head(cmurs,2)
cmurs <- cmurs[complete.cases(cmurs),]

irr <- unique(union(grep('REGI', cmurs[,3]), 
                    grep('RS', cmurs[,3])))
i.norr <- setdiff(1:nrow(cmurs), irr)

reg.tmp <- cmurs[,3]
reg.tmp[i.norr] <- paste('RS', reg.tmp[i.norr])

cmurs.l <- tapply(cmurs[,1], paste0(cmurs[,2], '_', reg.tmp),
                  function(x) sort(unique(x)))
iwmu.rg <- lapply(cmurs.l, pmatch, rownames(wvac.mu[[1]]))
summary(unlist(iwmu.rg))

wvac.rs <- lapply(wvac.mu, function(wv) 
    t(sapply(iwmu.rg, function(i)
        colSums(wv[i[!is.na(i)],,drop=FALSE], na.rm=TRUE))))
str(wvac.rs)

sapply(wvac.rs, sum, na.rm=TRUE)

mu.uf <- substr(rownames(wvac.mu[[1]]), 1, 2)
imu.uf <- tapply(rownames(wvac.mu[[1]]), mu.uf, function(cod) {
    i0 <- pmatch(rownames(wvac.mu[[1]]), cod)
    i0[!is.na(i0)]
})

wvac.uf <- lapply(wvac.mu, function(m)
    t(sapply(imu.uf[1:27], function(i)
        colSums(m[i, , drop=FALSE], na.rm=TRUE))))
str(wvac.uf)

sapply(wvac.uf, sum, na.rm=TRUE)

i.R <- c(list(1:27),
         lapply(1:5, function(i)
             which(substr(rownames(wvac.uf[[1]]),1,1)==i)))
names(i.R) <- c('', 'Norte', 'Nordeste',
                'Sudeste', 'Sul', 'Centro-Oeste')
i.R

wvac.R <- lapply(wvac.uf, function(m) 
    t(sapply(i.R, function(i) colSums(m[i,,drop=FALSE], na.rm=TRUE))))
str(wvac.R)

sapply(wvac.R, sum, na.rm=TRUE)

source('rcode/ocommon.R')

wvac <- mapply('rbind', wvac.mu, wvac.rs, wvac.uf, wvac.R, SIMPLIFY=FALSE)
str(wvac)

load('data/wdl.RData')

i2mc <- pmatch(rownames(wvac.mu[[1]]), wdl[[1]]$code)
i2m.u <- pmatch(substr(rownames(wvac.mu[[1]]),1,2), rownames(uf),
                duplicates.ok=TRUE)
summary(i2m.u)

wloc1a <- ifelse(is.na(i2mc), rownames(wvac.mu[[1]]), paste0(wdl[[1]]$City[i2mc], ', '))
wloc1b <- ifelse(is.na(i2m.u), '', paste0(uf$UF[i2m.u], ' - BR'))

wloc2 <- paste0(substring(rownames(wvac.rs[[1]]), 7), ', ', 
                uf$UF[pmatch(substr(rownames(wvac.rs[[1]]), 1, 2), rownames(uf),
                             duplicates.ok=TRUE)],
                ' - BR')

wloc3 <- paste0(uf$State[pmatch(substr(rownames(wvac.uf[[1]]), 1, 2), rownames(uf))], 
                ' - BR')
wloc3

wloc4 <- c('Brasil', paste0(names(i.R)[-1], ' - BR'))
wloc4

attr(wvac, 'local') <- c(
    paste0(wloc1a, wloc1b), wloc2, wloc3, wloc4)

save(wvac,
     file='data/wvac.RData')

Sys.time()-t0
