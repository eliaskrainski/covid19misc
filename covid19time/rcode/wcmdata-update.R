
if (FALSE) { ## can manually skip

    setwd('..')

}

library(parallel)
(ncores <- detectCores())

### load global data and create the 'wdl' object
dupdate <- FALSE
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

ussabb <- read.csv('data/us-states-abbreviation.csv')
head(ussabb,2)
for (j in 1:ncol(ussabb))
    ussabb[,j] <- as.character(ussabb[,j])

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

nycc <- c('New York', 'Kings', 'Bronx', 'Richmond', 'Queens')
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
wuscl[[1]][grep('New York', paste(wuscl.loc[1,], wuscl.loc[2,], sep='_')), c(-3:0+ncol(wuscl[[1]]))]

### BR states
uf <- data.frame(
    STATE=c("SERGIPE", "MARANHÃO", "ESPÍRITO SANTO", "AMAZONAS",
            "RORAIMA", "GOIÁS", "AMAPÁ", "RIO GRANDE DO SUL",
            "PARAÍBA", "PIAUÍ", "SÃO PAULO", "SANTA CATARINA",
            "PERNAMBUCO", "RIO DE JANEIRO", "MATO GROSSO DO SUL",
            "MATO GROSSO", "BAHIA", "MINAS GERAIS", "ALAGOAS",
            "CEARÁ", "RIO GRANDE DO NORTE", "PARANÁ", "RONDÔNIA",
            "DISTRITO FEDERAL", "ACRE", "PARÁ", "TOCANTINS"),
    State=c('Sergipe', 'Maranhão', 'Espírito Santo', 'Amazonas',
            'Roraima', 'Goiás', 'Amapá', 'Rio Grande do Sul',
            'Paraíba', 'Piauí', 'São Paulo', 'Santa Catarina',
            'Pernambuco', 'Rio de Janeiro', 'Mato Grosso do Sul',
            'Mato Grosso', 'Bahia', 'Minas Gerais', 'Alagoas',
            'Ceará', 'Rio Grande do Norte', 'Paraná', 'Rondônia',
            'Distrito Federal', 'Acre', 'Pará', 'Tocantins'), 
    UF = c("SE", "MA", "ES", "AM", "RR", "GO", "AP", "RS", "PB",
           "PI", "SP", "SC", "PE", "RJ", "MS", "MT", "BA", "MG",
           "AL", "CE", "RN", "PR", "RO", "DF", "AC", "PA", "TO"),
    row.names=c("28", "21", "32", "13", "14", "52", "16", "43", "25",
                "22", "35", "42", "26", "33", "50", "51", "29", "31",
                "27", "23", "24", "41", "11", "53", "12", "15", "17"))

### BR regions
r.pt <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
uf$Rcod <- as.integer(substr(rownames(uf), 1, 1))
uf$Região <- r.pt[uf$Rcod]

for (j in which(sapply(uf, is.factor)))
    uf[,j] <- as.character(uf[,j])

head(uf)

### br mun pop
brmpop <- read.csv2('data/populacao2019municipio.csv', skip=3)
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
            dbr[i.mu.l, c('fcode', 'fdate')], as.integer))
    str(wbr.mu)

    mun.mun <- sapply(rownames(wbr.mu[[1]]), function(x)
        substr(x, 1, nchar(x)-3))
    
    st.mun <- dbr$state[pmatch(rownames(wbr.mu[[1]]), dbr$fcode)]
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

    if (FALSE) {
        
        table(dbr$Regiao)
        
        table(table(dbr$mun.uf <- paste(
                        dbr$municipio, 
                        dbr$estado, sep='/'),
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

    system.time(wbr.R <- lapply(
                    dbr[c('casosAcumulado', 'obitosAcumulado')], tapply,
                dbr[c('Regiao', 'fdate')], sum))
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

summary(w2i.brm <- pmatch(
            paste0(mun.mun, ' (', st.mun, ')'),
            as.character(brmpop[,2])))
paste(mun.mun, st.mun)[is.na(w2i.brm)]
summary(wpop.brm <- brmpop$X2019[w2i.brm])

unddbr <- dbr[!duplicated(dbr$fcode),]
dim(unddbr)

unddbr[1,]
brmpop[1,]

und.mnm <- sapply(strsplit(
    unddbr$fcode, '/'), head, 1)
i2und.mn <- pmatch(
    brmpop[,2],
    paste0(und.mnm, ' (',
           unddbr$state, ')'))
summary(i2und.mn)

head(paste0(und.mnm, ' (', unddbr$state, ')'))
tail(paste0(und.mnm, ' (', unddbr$state, ')'))

wpop.rg <- tapply(
    brmpop$X2019,
    unddbr$name_RegiaoDeSaude[i2und.mn],
    sum, na.rm=TRUE)
str(wpop.rg)

stopifnot(all(rownames(wbr.rg[[1]])==names(wpop.rg)))

wpop.uf <- tapply(
    brmpop$X2019,
    unddbr$state[i2und.mn],
    sum, na.rm=TRUE)
str(wpop.uf)

stopifnot(all(rownames(wbr.uf[[1]])==names(wpop.uf)))

table(unddbr$Regiao)
wpop.R <- c(
    brmpop$X2019[1],
    tapply(brmpop$X2019,
           unddbr$Regiao[i2und.mn],
           sum, na.rm=TRUE))
str(wpop.R)

stopifnot(all(rownames(wbr.R[[1]])==names(wpop.R)))

source('rcode/dados-curitiba.R')

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
                       Country.Region='BR', Lat=NA, Long=NA,
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

save('wdl', file='data/wdl.RData')

if (!any(ls()=='gmob'))
    gmob <- TRUE

if (gmob) {
### mobility data from Google

    library(readr)
    ##system.time(gmbl <- read.csv(
    ##"data/Global_Mobility_Report.csv"))
    
    system.time(gmbl <- as.data.frame(read_csv(
                    "data/Global_Mobility_Report.csv",
                    col_types='cccccccciiiiii')))
    
    dim(gmbl)
    head(gmbl,2)
    summary(as.Date(unique(gmbl$date)))
    
    colnames(gmbl) <- gsub(
        '_percent_change_from_baseline', '', colnames(gmbl))

    for(j in which(sapply(gmbl, is.factor)))
        gmbl[, j] <- as.character(gmbl[, j])

    ##unique(grep('municip', gmbl$sub_region_2, val=T))
    ##unique(grep('city', gmbl$sub_region_2, val=T))
    ##length(unique(grep('City', gmbl$sub_region_2, val=T)))

    ##table(gmbl$country_region[grep('City', gmbl$sub_region_2)])
    ##unique(gmbl$sub_region_2[intersect(
      ##              grep('City', gmbl$sub_region_2),
        ##            which(gmbl$country_region=='United States'))])
    ##unique(grep('County', gmbl$sub_region_2, val=T))
    
    system.time(gmbl$fdate <- factor(gsub(
                    '-', '', gmbl$date), alldates))
    summary(as.integer(table(gmbl$fdate)))
    
    ##table(gmbl$country_region_code)
    
    ##table(as.character(
      ##  gmbl$sub_region_1[grep('State', gmbl$sub_region_1)]))

    ##system.time(gmbl$sub_region_1 <- gsub('State of ', '', gmbl$sub_region_1))
    system.time(iaux <- which(substr(gmbl$sub_region_1,1,9)=='State of '))
    system.time(gmbl$sub_region_1[iaux] <- substring(gmbl$sub_region_1[iaux], 10))

    system.time(gmbl$sub_region_2 <- gsub(' County', '', gmbl$sub_region_2))
    
    for (j in 1:nrow(ussabb)) {
        iij <- which(gmbl$sub_region_1==ussabb$State[j])
        cat(j, as.character(ussabb$State)[j], length(iij), '\n')
        gmbl$sub_region_1[iij] <- ussabb$Postal[j]
    }

    for (j in 1:nrow(uf)) {
        iij <- which(gmbl$sub_region_1==uf$State[j])
        cat(j, as.character(uf$State)[j], length(iij), '\n')
        gmbl$sub_region_1[iij] <- uf$UF[j]
    }

    if (any(ls()=='wdl'))
        system.time(load('data/wdl.RData'))
    
    length(aa <- paste(
               wdl[[1]]$City, 
               wdl[[1]]$Province,
               wdl[[1]]$Country, sep='_'))
    tail(sort(table(wdl[[1]]$Country)))
    table(duplicated(aa))
    table(aa[duplicated(aa)])

    if (FALSE) {

        wdl[[1]][grep('Caxias', aa), c(1:4, -2:0+ncol(wdl[[1]]))]

        length(bb <- unique(paste(
                   gmbl$sub_region_2,
                   gmbl$sub_region_1,
                   gmbl$country_region_code, sep='_')))
        tail(sort(table(gmbl$country_region))) 

        table(aa %in% bb)
        table(bb %in% aa)

    }

    gmbl$local <- paste(
        gmbl$sub_region_2,
        gmbl$sub_region_1,
        gmbl$country_region_code, sep='_')

    sort(table(gmbl$local[icode <- substr(
                              gmbl$local, 1, 2)=='__']))
    table(icode)
    
    gmbl$local[icode] <- paste0('__', gmbl$country_region[icode])
    gmbl$local[gmbl$local=='__United States'] <- '__US'

    if (FALSE) {

        table(aa %in% gmbl$local)
        table(unique(aa) %in% gmbl$local)
        table(unique(gmbl$local) %in% aa)
        
        length(unique(gmbl$local))
        length(unique(paste(gmbl$local, gmbl$metro)))
        
        table(w0 <- table(gmbl$local, gmbl$fdate))
        table(apply(w0>2, 1, any))
        table(apply(w0>2, 2, any))
        w0[apply(w0>2, 1, any), c(1:2, -9:0+ncol(w0))]
        
        head(gmbl,2)
        
        gmbl[which(gmbl$local=='__EG'), ]
        
    }

##stopifnot(all(table(gmbl$local, gmbl$fdate)<2))

    library(parallel)

    table(im.inc <- gmbl$local %in% aa)

    system.time(
        wgmbl <- mclapply(
            gmbl[im.inc, 9:14], tapply,
            gmbl[im.inc, c('local', 'fdate')], 
            mean))

    str(wgmbl[1])

    grep('Curitiba', rownames(wgmbl[[1]]))
    grep('Curitiba', rownames(wgmbl[[1]]), val=T)

    for (k in 1:length(wgmbl)) {
        ii <- grep('Curitiba_PR', rownames(wgmbl[[k]]))
        new <- wgmbl[[k]][ii,, drop=FALSE]
        rownames(new) <- gsub(
            'Curitiba', 'Curitiba(SM)', rownames(new))
        wgmbl[[k]] <- rbind(wgmbl[[k]], new)
    }

    for (k in 1:length(wgmbl)) {
        ii <- which(rownames(wgmbl[[k]])=='__Brazil')
        new <- wgmbl[[k]][ii,, drop=FALSE]
        rownames(new) <- gsub(
            '__Brazil', '__Brasil', rownames(new))
        wgmbl[[k]] <- rbind(wgmbl[[k]], new)
    }

    system.time(save(
        'wgmbl',
        file='data/wgmbl.RData',
        compress='xz'))

}

## Apple mobility data
if (!any(ls()=='amob'))
    amob <- TRUE

if (amob) {

    (afl <- system('ls data/applemobilitytrends*csv', TRUE))
    afl <- afl[order(as.Date(
        substr(afl, 26, 35)), decreasing=TRUE)]
    afl

    system.time(wambl0 <- read.csv(afl[1]))
    dim(wambl0)

    wambl0[1:5, 1:10]
    table(wambl0[,1])

    for (j in 1:6)
        wambl0[, j] <- as.character(wambl0[,j])

    grep('Curitiba', wambl0$region, val=TRUE)
    wambl0[grep('Curitiba', wambl0$region), 1:7]

    tail(sort(table(wambl0$region[grep('County', wambl0$region)])))
    tail(sort(table(wambl0$region[grep('City', wambl0$region)])))
    tail(sort(table(wambl0$region[grep('Municipio', wambl0$region)])))
    wambl0$region <- gsub(' County', '', wambl0$region)
    wambl0$region <- gsub(' City', '', wambl0$region)
    wambl0$region <- gsub(' Municipio', '', wambl0$region)

    table(wambl0$region=='Brazil')
    table(wambl0$region=='United States')
    table(wambl0$sub.region=='Brazil')
    table(wambl0$sub.region=='United States')

    table(wambl0$region=='US')
    wambl0$region <- gsub(
        'United States', 'US', wambl0$region)
    table(wambl0$country=='US')
    wambl0$country <- gsub(
        'United States', 'US', wambl0$country)

    table(wambl0$region=='BR')
    wambl0$region <- gsub('Brazil', 'BR', wambl0$region)
    table(wambl0$country=='BR')
    wambl0$country <- gsub('Brazil', 'BR', wambl0$country)
    
    grep('state', wambl0$region, val=T)
    grep('state', wambl0$sub.region, val=T)
    grep('state', wambl0$country, val=T)

    wambl0$region <- gsub(
        ' (state)', '', wambl0$region, fixed=TRUE)
    wambl0$sub.region <- gsub(
        ' (state)', '', wambl0$sub.region, fixed=TRUE)

    head(ussabb,2)
    for (j in 1:nrow(ussabb)) {
        ii <- which(wambl0$region==ussabb$State[j])
        cat(j, ussabb$State[j], length(ii), '')
        wambl0$region[ii] <- ussabb$Postal[j]
        ii <- which(wambl0$sub.region==ussabb$State[j])
        cat(length(ii), '')
        wambl0$sub.region[ii] <- ussabb$Postal[j]
        ii <- which(wambl0$country==ussabb$State[j])
        cat(length(ii), '\n')
        wambl0$country[ii] <- ussabb$Postal[j]
    }

    head(uf,2)
    for (j in 1:nrow(uf)) {
        ii <- which(wambl0$region==uf$State[j])
        cat(j, uf$State[j], length(ii), '')
        wambl0$region[ii] <- uf$UF[j]
        ii <- which(wambl0$sub.region==uf$State[j])
        cat(length(ii), '')
        wambl0$sub.region[ii] <- uf$UF[j]
        ii <- which(wambl0$country==uf$State[j])
        cat(length(ii), '\n')
        wambl0$country[ii] <- uf$UF[j]
    }

    grep('Bras', wambl0$region)
    grep('Bras', wambl0$alternative_name, val=T)
    grep('Bras', wambl0$sub.region)
    grep('Bras', wambl0$country)

    table(wambl0$transportation)

    wambl <- split(wambl0,
                   wambl0$transportation_type)
    sapply(wambl, dim)

    for(k in 1:length(wambl)) {
        wambl[[k]]$geo_type <- as.character(wambl[[k]]$geo_type)
        wambl[[k]]$region <- as.character(wambl[[k]]$region)
        wambl[[k]]$sub.region <- as.character(wambl[[k]]$sub.region)
        wambl[[k]]$country <- as.character(wambl[[k]]$country)
    }

    wambl0[grep('Curitiba', wambl0$region), 1:7]
    wambl0[grep('Para', wambl0$region), 1:6]
    wambl0[grep('Brazil', wambl0$region), 1:6]

    wambl0[grep('Paulo', wambl0$region), 1:6]

    wambl0[which(wambl0$region=='BR'), 1:7]

    table(wambl0$geo_type)
    
    for (k in 1:length(wambl)) {
        tlocal <- paste(
            ifelse(wambl[[k]]$geo_type %in% c('city', 'county'),
                   wambl[[k]]$region, ''), 
            ifelse(wambl[[k]]$geo_type %in% c('sub-region'), 
                   wambl[[k]]$region, wambl[[k]]$sub.region), 
            ifelse(wambl[[k]]$geo_type %in% c('country/region'), 
                   wambl[[k]]$region, wambl[[k]]$country), sep='_')

        tmp <- as.matrix(wambl[[k]][, 7:ncol(wambl[[k]])])
        
        ii <- grep('Curitiba_PR', tlocal)
        if (length(ii)>0) {
            cat(ii, tlocal[ii], '\n')
            wacwb <- as.matrix(
                wambl[[k]][ii, 7:ncol(wambl[[k]]), drop=FALSE])
            ncwb <- gsub('Curitiba', 'Curitiba(SM)', tlocal[ii])
            rownames(wacwb) <- ncwb
            tmp <- rbind(tmp, wacwb)
            tlocal <- c(tlocal, ncwb)
        }
        
        ii <- which(tlocal %in% paste0(c('NY', nycc[-1]), '_NY_US'))
        if (length(ii)>0) {
            print(ii)
            wanyc <- matrix(
                colMeans(wambl[[k]][ii, 7:ncol(wambl[[k]]),
                                    drop=FALSE]), 1)
            nnyc <- 'New York City_NY_US'
            rownames(wanyc) <- nnyc
            tmp <- rbind(tmp, wanyc)
            tlocal <- c(tlocal, nnyc)
        }
        
        wambl[[k]] <- tmp 
        attr(wambl[[k]], 'local') <- tlocal 
        attr(wambl[[k]], 'Date') <-
            as.Date(colnames(wambl[[k]]), 'X%Y.%m.%d')
    }

    sapply(wambl, nrow)
    sapply(wambl, function(x) length(attr(x, 'local')))

    grep('Curitiba', attr(wambl[[1]], 'local'),value=TRUE)
    
    system.time(save(
        'wambl',
        file='data/wambl.RData',
        compress='xz'))

}
