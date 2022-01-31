
if(FALSE)
    setwd('..')

t0 <- Sys.time()

library(data.table)
system.time(dMunDateDose <- as.data.frame(fread('data/tMunDateN.csv')))

dim(dMunDateDose)

head(dMunDateDose)

sum(dMunDateDose$N)

n.doses <- tapply(dMunDateDose$N, dMunDateDose$Dose, sum)
n.doses <- sort(n.doses, decreasing=TRUE)
n.doses

dl0 <- names(n.doses)##levels(dMunDateDose$Dose)
        dl <- rep('3', length(dl0))
        dl[intersect(grep('1', dl0), grep('ose', dl0))] <- '1'
        dl[intersect(grep('1', dl0), grep('OSE', dl0))] <- '1'
        dl[intersect(grep('2', dl0), grep('ose', dl0))] <- '2'
        dl[intersect(grep('2', dl0), grep('OSE', dl0))] <- '2'
        dl[grep('icial', dl0)] <- '1'
        dl[grep('ICIAL', dl0)] <- '1'
        dl[grep('nica', dl0)] <- '2'
        dl[grep('NICA', dl0)] <- '2'
        dl[dl0%in%c('Dose', 'Dose ', 'DOSE', 'DOSE ')] <- '2'
dl[union(grep('Re', dl0), grep('RE', dl0))] <- '3'
dl <- paste0('D', dl)

system.time(Dose <- factor(factor(dMunDateDose$Dose, dl0, dl), c('D1','D2','D3')))
ldose <- levels(Dose)#[1:2]
names(ldose) <- ldose
ldose

source('rcode/ocommon.R')
head(uf,2)

cUF <- substr(dMunDateDose$cod6,1,2)
dMunDateDose$UF <- uf$UF[pmatch(cUF, rownames(uf), duplicates.ok=TRUE)]

Date <- seq(as.Date('20200121', '%Y%m%d'), Sys.Date(), 1)
alldates <- gsub('-', '', as.character(Date))

system.time(dMunDateDose$fdate <- factor(
                gsub('-', '', substr(dMunDateDose$Date, 1, 10)), 
                alldates))

head(dMunDateDose)
tail(dMunDateDose)

dMunDateDose[,1] <- as.factor(dMunDateDose[,1])

str(dMunDateDose)

system.time(wvac.mu <- lapply(ldose, function(d) {
    ii <- which(Dose %in% d)
    print(length(ii))
    tapply(dMunDateDose$N[ii],
           dMunDateDose[ii, c('cod6', 'fdate')], sum)
}))

str(wvac.mu)

sapply(wvac.mu, sum, na.rm=TRUE)
sapply(wvac.mu, function(m)
    colSums(m[, ncol(m)-4:0], na.rm=TRUE))

hpsfls <- system('ls data/HIST_PAINEL*.csv', TRUE)
hpsfls

cmurs <- as.data.frame(
    fread(tail(hpsfls,1), 
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
