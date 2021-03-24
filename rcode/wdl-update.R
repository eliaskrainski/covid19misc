if (FALSE) { ## can manually skip

    setwd('..')

}

if (!any(ls() %in% c('ussabb', 'uf')))
    source('rcode/ocommon.R')

library(parallel)
(ncores <- as.integer(detectCores()/2))

### load global data and create the 'wdl' object
if (!any(ls()=='dupdate'))
    dupdate <- TRUE

system.time(source('rcode/wc0update.R'))

ls()
sapply(wdl, dim)

### US states data
us.d <- read.csv('data/daily.csv')

us.d$date <- factor(us.d$date, alldates)

w.us <- lapply(us.d[c('positive', 'death')], tapply, 
               us.d[c('state', 'date')], as.integer)

for (k in 1:2) {
    wdl[[k]] <- rbind(
        wdl[[k]],
        data.frame(code='', City='',
                   Province.State=rownames(w.us[[k]]),
                   Country.Region='US',
                   Lat=as.numeric(NA),
                   Long=as.numeric(NA),
                   w.us[[k]]))
}

### US counties population from
## https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv
uscpop <- read.csv('data/us-counties-statistics.csv')
head(uscpop[,1:20],1)

table(uscpop$SUMLEV)
tapply(uscpop$POPESTIMATE2019,
       uscpop$SUMLEV, sum)
summary(uscpop$POPESTIMATE2019[uscpop$SUMLEV==50])
uscpop[tail(order(uscpop$POPESTIMATE2019)), 1:7]

i2ist <- pmatch(uscpop$STNAME,
                ussabb$State.District,
                duplicates.ok=TRUE)
uscpop$STcode <- ussabb$Postal.Code[i2ist]

## uscpop[c("STcode", "POPESTIMATE2019")][uscpop$SUMLEV==40, ]

uss.pop <- tapply(uscpop$POPESTIMATE2019[uscpop$SUMLEV==40],
                  uscpop$STcode[uscpop$SUMLEV==40], sum)
uss.pop

w2i.uss <- pmatch(rownames(w.us[[1]]), names(uss.pop))
summary(w2i.uss)
sum(wpop.uss <- uss.pop[w2i.uss], na.rm=TRUE)
summary(wpop.uss)

## US county data
system.time(uscl <- read.csv('data/us-counties.csv'))
head(uscl,3)

system.time(uscl$fdate <- factor(gsub(
                '-', '', uscl$date), alldates))

uscl$ST <- as.character(uscl$state)
for (j in 1:nrow(ussabb)) {
    iij <- which(uscl$ST==ussabb$State[j])
    cat(j, as.character(ussabb$State)[j], length(iij), '\n')
    uscl$ST[iij] <- ussabb$Postal[j]
}

table(uscl$ST)

length(unique(paste(uscl$fips)))
stopifnot(all(table(uscl$fips, uscl$fdate)<2))

length(unique(paste(uscl$county, uscl$state)))
length(unique(paste(uscl$county, uscl$ST)))
stopifnot(all(table(paste(uscl$county, uscl$ST), uscl$fdate)<2))

unique(paste(uscl$county, uscl$ST)[is.na(uscl$fips)])

if (FALSE) {
    grep('ounty', uscl$county,val=T)
    unique(grep('unicip', uscl$county,val=T))
    length(unique(grep('city', uscl$county,val=T)))
    (unique(grep('City', uscl$county,val=T)))
}

uscl$local <- paste(uscl$county, uscl$ST, 'US', sep='_')

system.time(wuscl <- lapply(
                uscl[, c('cases', 'deaths')], tapply,
                uscl[, c('local', 'fdate')], sum))
str(wuscl)

wuscl.loc <- strsplit(rownames(wuscl[[1]]), '_')
stopifnot(length(unique(sapply(wuscl.loc, length)))==1)

dim(wuscl.loc <- sapply(wuscl.loc, as.character))
head(t(wuscl.loc))

uscpop[uscpop$SUMLEV==50, ][
    tail(order(uscpop$POPESTIMATE2019[uscpop$SUMLEV==50])), 1:7]

for (k in 1:2) {
    wdl[[k]] <- rbind(
        wdl[[k]],
        data.frame(code='', City=wuscl.loc[1,],
                   Province.State=wuscl.loc[2,], 
                   Country.Region='US',
                   Lat=as.numeric(NA),
                   Long=as.numeric(NA),
                   wuscl[[k]]))
}

w2i.usc <- pmatch(
    paste(wuscl.loc[1,], wuscl.loc[2,], sep='_'),
    c(paste(gsub(' County', '',
                 gsub(' city', '', uscpop$CTYNAME)),
            uscpop$STcode, sep='_'),
      'New York City_NY'))
summary(w2i.usc)
dim(uscpop[which(is.na(w2i.usc)), 6:7])

lapply(nycc, function(x)
    uscpop$CTYNAME[paste(x, 'County')==uscpop$CTYNAME])

nyc.pop <- uscpop$POPESTIMATE2019[
(uscpop$STNAME=='New York') &
(uscpop$CTYNAME %in% paste(nycc, 'County'))]
nyc.pop

sum(nyc.pop)

summary(wpop.usc <- c(uscpop$POPESTIMATE2019,
                      sum(nyc.pop))[w2i.usc])

grep('New York', paste(wuscl.loc[1,], wuscl.loc[2,], sep='_'), val=T)
wuscl[[1]][grep('New York', paste(wuscl.loc[1,], wuscl.loc[2,], sep='_')),
           c(-3:0+ncol(wuscl[[1]]))]


### BR regions
r.pt <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
uf$Rcod <- as.integer(substr(rownames(uf), 1, 1))
uf$Região <- r.pt[uf$Rcod]

for (j in which(sapply(uf, is.factor)))
    uf[,j] <- as.character(uf[,j])
    
head(uf)

### br mun pop
brmpop <- read.csv2('data/populacao2019municipio.csv', skip=3, nrow=5571)
dim(brmpop)
brmpop[nrow(brmpop)-2:0,]
brmpop[1:2,]

grep('ivac', brmpop[,2], val=T)
grep('de (RN)', brmpop[,2], val=T, fixed=TRUE)
grep('RN', grep('Campo', brmpop[,2], val=T, fixed=TRUE), val=TRUE)
grep('ona', brmpop[,2], val=T)
grep('rer', brmpop[,2], val=T)
grep('Cardo', brmpop[,2], val=T)
grep('Borges', brmpop[,2], val=T)
grep('Pingo', brmpop[,2], val=T)
grep('Santa Tere', brmpop[,2], val=T)
grep('Letras', brmpop[,2], val=T)
grep('Taboc', brmpop[,2], val=T)

##head(dbr[intersect(grep('Campo Grande', dbr$city),
##                 which(dbr$state=='RN')), ])
brmpop[brmpop[,1]=='2401305', ]

brmpop[,2] <- as.character(brmpop[,2])

brmpop[,2] <- gsub(
    'Augusto Severo (RN)', 'Campo Grande (RN)',
    brmpop[,2], fixed=TRUE)
brmpop[,2] <- gsub(
    'Dona Eusébia (MG)', 'Dona Euzébia (MG)',
    brmpop[,2], fixed=TRUE)
brmpop[,2] <- gsub(
    'Ererê (CE)', 'Ereré (CE)', 
    brmpop[,2], fixed=TRUE)
brmpop[,2] <- gsub(
    'São Thomé das Letras (MG)', 'São Tomé das Letras (MG)', 
    brmpop[,2], fixed=TRUE)
brmpop[,2] <- gsub(
    'Fortaleza do Tabocão', 'Tabocão', 
    brmpop[,2], fixed=TRUE)

if (wcota) {

    brmpop[,2] <- gsub(
        'Atílio Vivacqua', 'Atilio Vivacqua', brmpop[,2])
    brmpop[,2] <- gsub(
        'Gracho Cardoso (SE)', 'Graccho Cardoso (SE)', 
        brmpop[,2], fixed=TRUE)
    brmpop[,2] <- gsub(
        'Santa Teresinha', 'Santa Terezinha', brmpop[,2])

### data from Wesley Cota 
    system.time(dbr <- read.csv(
                    'data/cases-brazil-cities-time.csv.gz'))
    
    dim(dbr)
    summary(as.Date(unique(dbr$date)))

    dbr[tail(order(dbr$totalCases),2),]
    
    table(nchar(dbr$ibgeID))
    
    unique(dbr$city[nchar(dbr$ibgeID)<5])
    
    head(dbr,2)
    table(dbr$state)
    
    for (j in which(sapply(dbr, is.factor)))
        dbr[, j] <- as.character(dbr[, j])
    
    i.mu.l <- which(dbr$city!='TOTAL')
    i.rg.l <- which(dbr$name_RegiaoDeSaude!='')
    

    if (FALSE) {

        system.time(dbr$fcode <- gsub(
                        'CASO SEM LOCALIZAÇÃO DEFINIDA', 'Indefinido',
                        as.character(dbr$city)))
        
        system.time(dbr$fcode <- gsub(
                        "Olho-d'Água do Borges", "Olho d'Água do Borges", dbr$fcode))
        
        system.time(dbr$fcode <- gsub(
                        "Pingo-d'Água", "Pingo d'Água", dbr$fcode))
        
        system.time(dbr$fcode <- gsub(
                        'Santa Teresinha', 'Santa Terezinha', dbr$fcode))
        
        grep('Indefinido', unique(dbr$fcode), val=T)
        
    }
    
    dbr$fdate <- factor(gsub('-', '', dbr$date, 
                             fixed=TRUE), alldates)
    head(dbr, 3)
    
    dbr$Regiao <- uf$Regi[pmatch(dbr$state, uf$UF,
                                 duplicates.ok=TRUE)]
    table(is.na(dbr$Regiao))
    dbr$Regiao[is.na(dbr$Regiao)] <- ''
    table(dbr$Regiao)
    
    system.time(
        wbr.mu <- lapply(
            dbr[i.mu.l, c('totalCases', 'deaths')], tapply,
            dbr[i.mu.l, c('city', 'fdate')], as.integer))
    str(wbr.mu)

    system.time(rnmu <- gsub('CASO SEM LOCALIZAÇÃO DEFINIDA',
                             'Indefinido', rownames(wbr.mu[[1]])))
    system.time(rnmu <- gsub("Olho-d'Água do Borges", "Olho d'Água do Borges", rnmu))
    system.time(rnmu <- gsub("Pingo-d'Água", "Pingo d'Água", rnmu))        
    system.time(rnmu <- gsub('Santa Teresinha', 'Santa Terezinha', rnmu))
    rownames(wbr.mu[[1]]) <- rownames(wbr.mu[[2]]) <- rnmu        
    
    system.time(st.mun <- dbr$state[pmatch(rownames(wbr.mu[[1]]), dbr$city)])

    system.time(mun.mun <- sapply(rownames(wbr.mu[[1]]), function(x)
        substr(x, 1, nchar(x)-3)))
    
    table(is.na(st.mun))
    table(st.mun)
    mun.mun[is.na(st.mun)]
    st.mun[is.na(st.mun)] <- substring(
        names(mun.mun)[is.na(st.mun)], 12)
    table(is.na(st.mun))
    
    system.time(
        wbr.rg <- lapply(
            dbr[i.rg.l, c('totalCases', 'deaths')], tapply,
            dbr[i.rg.l, c('name_RegiaoDeSaude', 'fdate')], sum))
    str(wbr.rg)
    
    st.rg <- dbr$state[pmatch(rownames(
                     wbr.rg[[1]]), dbr$name_RegiaoDeSaude)]
    table(st.rg)
    
    system.time(
        wbr.uf <- lapply(
            dbr[i.mu.l, c('totalCases', 'deaths')], tapply,
            dbr[i.mu.l, c('state', 'fdate')], sum))
    str(wbr.uf)

    system.time(
        wbr.R <- lapply(
            dbr[c('totalCases', 'deaths')], tapply,
            dbr[c('Regiao', 'fdate')], sum))
    str(wbr.R)
   
    unddbr <- dbr[!duplicated(dbr$city),]
    und.mnm <- sapply(strsplit(
        unddbr$city, '/'), head, 1)
    munam.br <- 'city'
    stnam.br <- 'state'
    rgnam.br <- 'name_RegiaoDeSaude'
    
}

if (usems) {

    if (FALSE) {
        
        system.time(dbr <- read.csv2(
                        'data/HIST_PAINEL_COVIDBR.csv'))

        for (j in which(sapply(dbr, is.factor)))
            dbr[, j] <- as.character(dbr[, j])

    } else {
        
        system.time(
            dbr <- as.data.frame(
                readr::read_csv2(
                           'data/HIST_PAINEL_COVIDBR.csv',
                           col_types='cccccccciiiiiiiic')))

    }
    
    dim(dbr)
    head(dbr,2)
    summary(as.Date(unique(dbr$data)))
    
    i.mu.l <- which(dbr$municipio!='')
    i.rg.l <- which(dbr$nomeRegiaoSaude!='')
    
    dbr$fdate <- factor(gsub('-', '', dbr$data, 
                             fixed=TRUE), alldates)
    head(dbr, 3)

    dbr$Regiao <- dbr$regiao
    dbr$Regiao[dbr$regiao=='Brasil'] <- ''

    system.time(dbr$mun.uf <- paste(
                    dbr$municipio, 
                    dbr$estado, sep='/'))
    
    unddbr <- dbr[i.mu.l, ][!duplicated(dbr$mun.uf[i.mu.l]),]
    und.mnm <- sapply(strsplit(
        unddbr$mun.uf, '/'), head, 1)
    munam.br <- 'municipio'
    stnam.br <- 'estado'
    rgnam.br <- 'nomeRegiaoSaude'
    
    if (FALSE) {
        
        table(dbr$Regiao)
        
        table(table(dbr$mun.uf,
                    dbr$fdate))
        table(table(dbr$mun.uf[i.mu.l], dbr$fdate[i.mu.l]))

    }

    system.time(wbr.mu <- mclapply(
                    dbr[i.mu.l, c('casosAcumulado', 'obitosAcumulado')], tapply,
                    dbr[i.mu.l, c('mun.uf', 'fdate')], as.integer))
    str(wbr.mu)
    
    mun.mun <- sapply(rownames(wbr.mu[[1]]), function(x)
        substr(x, 1, nchar(x)-3))
    
    st.mun <- sapply(rownames(wbr.mu[[1]]), function(x)
        substring(x, nchar(x)-1))
    table(is.na(st.mun))
    table(st.mun)
    
    system.time(wbr.rg <- lapply(
                    dbr[i.rg.l, c('casosAcumulado', 'obitosAcumulado')], tapply,
                    dbr[i.rg.l, c('nomeRegiaoSaude', 'fdate')], sum))
    str(wbr.rg)
    
    st.rg <- dbr$estado[pmatch(rownames(
                     wbr.rg[[1]]), dbr$nomeRegiaoSaude)]
    table(st.rg)
    
    system.time(wbr.uf <- lapply(
                    dbr[i.mu.l, c('casosAcumulado', 'obitosAcumulado')], tapply,
                    dbr[i.mu.l, c('estado', 'fdate')], sum))
    str(wbr.uf)

    table(dbr$Regiao)
    idd <- which((dbr$Regiao=='') & duplicated(dbr$fdate))
    if (length(idd)>0) {
        system.time(wbr.R <- lapply(
                        dbr[-idd, c('casosAcumulado', 'obitosAcumulado')], tapply,
                        dbr[-idd, c('Regiao', 'fdate')], sum))
    } else {
        system.time(wbr.R <- lapply(
                        dbr[, c('casosAcumulado', 'obitosAcumulado')], tapply,
                        dbr[, c('Regiao', 'fdate')], sum))
    }
    str(wbr.R)
    
}

if (usefnd) {
    
    library(covid19br)

    system.time(dbrms <- as.data.frame(
                    covid19br:::downloadBR('en', 'cities')))
    
    dim(dbrms)
    head(dbrms,2)
    summary(dbrms$date)

    for (j in which(sapply(dbrms, is.factor)))
        dbrms[, j] <- as.character(dbrms[, j])

    system.time(dbrms$fcode <- ifelse(
                    dbrms$city=='', 'Indefinido', dbrms$city))

    table(paste0(dbrms$fcode, ' (', dbrms$state, ')') %in% brmpop[,2])
    table(dbrms$city[!paste0(dbrms$fcode, ' (', dbrms$state, ')') %in% brmpop[,2]])

    dbrms$fdate <- factor(gsub('-', '', dbrms$data, 
                               fixed=TRUE), alldates)
    table(dbrms$region)

}

dim(unddbr)

summary(w2i.brm <- pmatch(
            paste0(mun.mun, ' (', st.mun, ')'),
            as.character(brmpop[,2])))
paste(mun.mun, st.mun)[is.na(w2i.brm)]
summary(wpop.brm <- brmpop$X2019[w2i.brm])

unddbr[1,]
brmpop[1,]

head(unddbr[,munam.br])
head(unddbr[,stnam.br])
i2und.mn <- pmatch(
    brmpop[-1,2],
    paste0(unddbr[,munam.br], ' (', 
           unddbr[,stnam.br], ')'))
summary(i2und.mn)

str(brmpop$X2019)
str(unddbr[i2und.mn[complete.cases(i2und.mn)], rgnam.br])
wpop.rg <- tapply(
    brmpop$X2019[-1],
    unddbr[i2und.mn, rgnam.br],
           sum, na.rm=TRUE)
str(wpop.rg)

stopifnot(all(rownames(wbr.rg[[1]])==names(wpop.rg)))

wpop.uf <- tapply(
    brmpop$X2019[-1],
    unddbr[i2und.mn, stnam.br],
    sum, na.rm=TRUE)
str(wpop.uf)

stopifnot(all(rownames(wbr.uf[[1]])==names(wpop.uf)))

table(unddbr$Regiao)
wpop.R <- c(
    brmpop$X2019[1],
    tapply(brmpop$X2019[-1],
           unddbr$Regiao[i2und.mn],
           sum, na.rm=TRUE))
str(wpop.R)

stopifnot(all(rownames(wbr.R[[1]])==names(wpop.R)))

system.time(source('rcode/dados-curitiba.R'))

    for (k in 1:2) {
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(code='', City='',
                       Province.State=rownames(wbr.R[[k]]), 
                       Country.Region='Brasil', Lat=NA, Long=NA,
                       wbr.R[[k]]))
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(code='', City='', 
                       Province.State=rownames(wbr.uf[[k]]),
                       Country.Region='Brasil', Lat=NA, Long=NA,
                       wbr.uf[[k]]))
        reg.tmp <- rownames(wbr.rg[[k]])
        irr <- setdiff(1:length(reg.tmp), grep('Reg', reg.tmp))
        reg.tmp[irr] <- paste('Reg.', reg.tmp[irr])
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(code='',
                       City=reg.tmp, 
                       Province.State=st.rg, 
                       Country.Region='BR', Lat=NA, Long=NA,
                       wbr.rg[[k]]))
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(code='', 
                       City=mun.mun, 
                       Province.State=st.mun, 
                       Country.Region='BR', Lat=NA, Long=NA,
                       wbr.mu[[k]]))
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(
                code='4104902', 
                City='Curitiba(SM)',
                Province.State='PR', 
                Country.Region='BR', Lat=NA, Long=NA,
                matrix(wcwb[[k]], 1,
                       dimnames=list(NULL, alldates))))
    }

if (FALSE) {

    iii <- sort(sample(tail(1:nrow(wdl[[1]]), 5570), 10))
    wdl[[1]][iii, 2:3]
    wpop.brm[iii]
    grep('Ibipeba', brmpop[,2], val=T)
    
}

sapply(wdl, dim)

for (k in 1:2) {
    rownames(wdl[[k]]) <- 1:nrow(wdl[[k]])
    wdl[[k]] <- as.data.frame(wdl[[k]])
    wdl[[k]][, 7] <- as.integer(wdl[[k]][, 7])
    ii <- which(is.na(wdl[[k]][, 7]))
    if (length(ii)>0) 
        wdl[[k]][ii, 7] <- 0L
    nt <- ncol(wdl[[k]])
    for (j in 8:nt) {
        y <- as.integer(wdl[[k]][,j])
        ii <- which(is.na(y))
        if (length(ii)>0)
            y[ii] <- wdl[[k]][ii, j-1]
        wdl[[k]][, j] <- y
    }
}

##if (FALSE) {

k <- 1
dlast <- wdl[[k]][, (nt-5):nt]-
    wdl[[k]][, (nt-6):(nt-1)]
mlast <- dlast[, 5:6]<(0.01*rowMeans(dlast[, 1:4]))
for (i in which(rowSums(mlast)>0)) {
    wdl[[k]][i, which(mlast[i, ]) + nt-2] <- NA
    wdl[[k+1]][i, which(mlast[i, ]) + nt-2] <- NA
}

##}

table(duplicated(paste(wdl[[1]]$City,
                       wdl[[1]]$Prov,
                       wdl[[1]]$Country)))
paste(wdl[[1]]$City,
      wdl[[1]]$Prov,
      wdl[[1]]$Country)[
    duplicated(paste(wdl[[1]]$City,
                       wdl[[1]]$Prov,
                       wdl[[1]]$Country))]

(wpop.cwb <- brmpop$X2019[brmpop[,2]=='Curitiba (PR)'])

length(c(wpop.c, wpop.uss, wpop.usc, wpop.brm,
         wpop.rg, wpop.uf, wpop.R, wpop.cwb))
attr(wdl, 'population') <- c(
    wpop.c, wpop.uss, wpop.usc, wpop.R, 
    wpop.uf, wpop.rg, wpop.brm, wpop.cwb)

attr(wdl, 'Sys.time') <- Sys.time()

system.time(save('wdl', file='data/wdl.RData'))

