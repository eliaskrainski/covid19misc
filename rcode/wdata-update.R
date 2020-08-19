
if (FALSE) { ## can manually skip

    setwd('..')

}

options(width=70)

if (!any(ls()=='brio'))
    brio <- TRUE

if (!any(ls()=='wcota')) 
    wcota <- !brio

if (!any(ls()=='usems'))
    usems <- TRUE

source('rcode/getdata.R')

wwfun <- function(fl) {
    m <- read.csv(fl)
    co <- as.character(m$Country)
    prov.bl <- m$Province!=''
    toSum <- setdiff(
        co[prov.bl],
        do.call('intersect', split(co, c('x', 'y')[prov.bl+1])))
    if (length(toSum)>0) {
        aux <- data.frame(code='', City='', Province.State='',
                          Country.Region=toSum, Lat=NA, Long=NA) 
        cs <- sapply(toSum, function(x)
            colSums(m[which(m$Country==x), 5:ncol(m)]))
        return(rbind(data.frame(aux, t(cs)), 
                     data.frame(code='', City='', m)))
    } else {
        return(m)
    }                                                 
}

wdl <- lapply(c(confirmed='data/confirmed_global.csv',
                deaths='data/deaths_global.csv'), wwfun)

Date <- seq(as.Date(colnames(wdl[[1]])[7], 
                    'X%m.%d.%y'), Sys.Date(), 1)
head(Date)
tail(Date)

for (k in 1:2) {
    dtmp <- as.Date(colnames(wdl[[k]])[7:ncol(wdl[[k]])],
                    'X%m.%d.%y')
    if (tail(dtmp, 1)!=Sys.Date()) 
        wdl[[k]] <- data.frame(wdl[[k]][, 1:ncol(wdl[[k]])],
                               new=NA)
}

##Date <- unique(c(as.Date(colnames(wdl[[1]])[7:ncol(wdl[[1]])],
  ##                     'X%m.%d.%y'), Sys.Date()))
alldates <- gsub('-', '', as.character(Date))

colnames(wdl[[1]])[7:ncol(wdl[[1]])] <-
    colnames(wdl[[2]])[7:ncol(wdl[[2]])] <-
    paste0('X', gsub('-', '', as.character(Date)))


### US data
us.d <- read.csv('data/daily.csv')

us.d$date <- factor(us.d$date, alldates)

w.us <- lapply(us.d[c('positive', 'death')], tapply, 
               us.d[c('state', 'date')], as.integer)

##for (k in 1:2) {
  ##  dtmp <- as.Date(colnames(w.us[[k]]), '%Y%m%d')
##    if (tail(dtmp, 1)==Sys.Date())
  ##      w.us[[k]] <- w.us[[k]][, 1:(ncol(w.us[[k]])-1)]
##}
##object.size(w.us)

for (k in 1:2) {
##    w.us[[k]][is.na(w.us[[k]])] <- 0L
    wdl[[k]] <- rbind(
        wdl[[k]],
        data.frame(code='', City='',
                   Province.State=rownames(w.us[[k]]),
                   Country.Region='US',
                   Lat=as.numeric(NA),
                   Long=as.numeric(NA),
                   w.us[[k]]))
}

if (brio) {
### Brasil data
    system.time(dbr <- read.csv('data/caso.csv.gv'))
}  else {
### data from Wesley Cota 
    system.time(dbr <- read.csv('data/cases-brazil-cities-time.csv'))
}

dbr$fdate <- factor(gsub('-', '', dbr$date, 
                         fixed=TRUE), alldates)


if (brio) {

    i.uf <- which(dbr$place_type=='state')
    i.mu <- which(dbr$place_type=='city')

### Identifica as colunas (y) e (x)
    jj.y  <- match(c('confirmed', 'deaths'),
                   colnames(dbr))
    jj.x <- match(c('city_ibge_code', 'fdate'),
                  colnames(dbr))
    
    jj.x.uf <- match(c('state', 'fdate'),
                     colnames(dbr))

    wbr.mu <- lapply(dbr[i.mu, jj.y], tapply,
                     dbr[i.mu, jj.x], as.integer)
    object.size(wbr.mu)
    
    wbr.uf <- lapply(dbr[i.uf, jj.y], tapply,
                     dbr[i.uf, jj.x.uf], as.integer)
    object.size(wbr.uf)
        
    uf <- c(SE = "SERGIPE", MA = "MARANHÃO", ES = "ESPÍRITO SANTO",
            AM = "AMAZONAS", RR = "RORAIMA", GO = "GOIÁS",
            AP = "AMAPÁ", RS = "RIO GRANDE DO SUL", PB = "PARAÍBA",
            PI = "PIAUÍ", SP = "SÃO PAULO", SC = "SANTA CATARINA",
            PE = "PERNAMBUCO", RJ = "RIO DE JANEIRO",
            MS = "MATO GROSSO DO SUL", MT = "MATO GROSSO",
            BA = "BAHIA", MG = "MINAS GERAIS", AL = "ALAGOAS",
            CE = "CEARÁ", RN = "RIO GRANDE DO NORTE",
            PR = "PARANÁ", RO = "RONDÔNIA",
            DF = "DISTRITO FEDERAL", AC = "ACRE",
            PA = "PARÁ", TO = "TOCANTINS")

    imunam <- pmatch(rownames(wbr.mu[[1]]), dbr$city_ibge_code)
    munam <- as.character(dbr$city)[imunam]
    
    iufuf <- pmatch(rownames(wbr.mu[[1]]), dbr$city_ibge_code)
    ufuf <- dbr$state[iufuf]
    
    stnam.uf <- uf[pmatch(rownames(wbr.uf[[1]]), 
                          names(uf), duplicates.ok=TRUE)]
    stnam.mu <- uf[pmatch(ufuf, names(uf), duplicates.ok=TRUE)]
    
    for (k in 1:2) {
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(code='1', City='total', Province.State='total', 
                       Country.Region='Brasil (total)',
                       Lat=NA, Long=NA,
                       matrix(colSums(wbr.uf[[k]]), 1,
                              dimnames=list(
                                  '', colnames(wbr.uf[[k]])))))
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(code='', City='',
                       Province.State=stnam.uf, 
                       Country.Region='Brazil', Lat=NA, Long=NA,
                       wbr.uf[[k]]))
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(code=rownames(wbr.mu[[k]]),
                       City=munam,
                       Province.State=stnam.mu, 
                       Country.Region='Brazil', Lat=NA, Long=NA,
                       wbr.mu[[k]]))
    }

} else {

    uf <- data.frame(
        STATE=c("SERGIPE", "MARANHÃO", "ESPÍRITO SANTO", "AMAZONAS",
                "RORAIMA", "GOIÁS", "AMAPÁ", "RIO GRANDE DO SUL",
                "PARAÍBA", "PIAUÍ", "SÃO PAULO", "SANTA CATARINA",
                "PERNAMBUCO", "RIO DE JANEIRO", "MATO GROSSO DO SUL",
                "MATO GROSSO", "BAHIA", "MINAS GERAIS", "ALAGOAS",
                "CEARÁ", "RIO GRANDE DO NORTE", "PARANÁ", "RONDÔNIA",
                "DISTRITO FEDERAL", "ACRE", "PARÁ", "TOCANTINS"), 
        UF = c("SE", "MA", "ES", "AM", "RR", "GO", "AP", "RS", "PB",
               "PI", "SP", "SC", "PE", "RJ", "MS", "MT", "BA", "MG",
               "AL", "CE", "RN", "PR", "RO", "DF", "AC", "PA", "TO"),
        row.names=c("28", "21", "32", "13", "14", "52", "16", "43", "25",
                    "22", "35", "42", "26", "33", "50", "51", "29", "31",
                    "27", "23", "24", "41", "11", "53", "12", "15", "17"))
    
    dbr$fcode <- factor(dbr$ibgeID,
                        sort(as.integer(unique(
                            c(rownames(uf), dbr$ibgeID)))))
    
    wbr <- lapply(dbr[c('totalCases', 'deaths')], tapply,
                  dbr[c('fcode', 'fdate')], as.integer)
    
    iwm <- which(nchar(rownames(wbr[[1]]))==7)
    iws <- which(nchar(rownames(wbr[[1]]))==2)
    
    wbr.uf <- lapply(wbr, function(x) {
        aggregate(x[unique(c(iws, iwm)),],
                  list(code=substr(rownames(x)[unique(c(iws, iwm))], 1, 2)),
                  sum, na.rm=TRUE)
    })
    
    summary(imunam <- pmatch(rownames(wbr[[1]])[iwm], dbr$ibgeID) )
    munam <- as.character(dbr$city)[imunam]
    
    stnam.uf <- uf$STATE[pmatch(wbr.uf[[1]]$code, rownames(uf))]
    i.uf.mu <- pmatch(as.character(dbr$state)[imunam],
                      as.character(uf$UF), duplicates.ok=TRUE)
    table(as.character(dbr$state)[imunam]==as.character(uf$UF)[i.uf.mu])
    stnam.mu <- uf$STATE[i.uf.mu]
    
    for (k in 1:2) {
##        wbr.uf[[k]][wbr.uf[[k]]==0] <- NA 
##        wdl[[k]] <- rbind(
  ##          wdl[[k]],
    ##        data.frame(code='1', City='total', Province.State='total', ##
   ##                    Country.Region='Brasil (total)',
     ##                  Lat=NA, Long=NA,
       ##                matrix(colSums(wbr.uf[[k]][,-1]), 1,
         ##                     dimnames=list(
           ##                       '', colnames(wbr.uf[[k]])[-1]))))
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(code=wbr.uf[[k]]$code, City='',
                       Province.State=stnam.uf, 
                       Country.Region='Brazil', Lat=NA, Long=NA,
                       as.matrix(wbr.uf[[k]][,-1])))
##        wbr[[k]][wbr[[k]]==0] <- NA 
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(code=rownames(wbr[[k]])[iwm],
                       City=munam,
                       Province.State=stnam.mu, 
                       Country.Region='Brazil', Lat=NA, Long=NA,
                       wbr[[k]][iwm,]))
    }
}

sapply(wdl,dim)
tail(wdl[[1]][, 1:10],2)

if (usems & file.exists('data/HIST_PAINEL_COVIDBR.csv')) {

    system.time(dbrms <- read.csv('data/HIST_PAINEL_COVIDBR.csv'))
    dbrms$fdate <- factor(gsub('-', '', as.Date(dbrms$data, '%m-%d-%y'),
                               fixed=TRUE), alldates)
    head(dbrms,2)
    tail(dbrms,2)

    iimm <- which(!is.na(dbrms$codmun) & (dbrms$municipio!=''))
    str(iimm)

    system.time(dbr.m <- aggregate(
        dbrms[iimm, c('casosAcumulado', 'obitosAcumulado')],
        dbrms[iimm, c('fdate', 'codmun')], sum))
    head(dbr.m, 2)
    
    system.time(wbr.m <- lapply(
                    dbr.m[, c('casosAcumulado', 'obitosAcumulado')],
                    tapply,
                    dbr.m[, c('codmun', 'fdate')], as.integer)) 
    str(wbr.m)
    wbr.m[[1]][1:5, ncol(wbr.m[[1]])-3:0]

    if (brio) {

        i2i.mm <- pmatch(rownames(wbr.m[[1]]),
                         substr(rownames(wbr.mu[[1]]),1,6)) 
        c(nrow(wbr.mu[[1]]), nrow(wbr.m[[1]]), length(i2i.mm))

        i2na <- is.na(i2i.mm)
        table(rownames(wbr.m[[1]])[!i2na]==
              substr(rownames(wbr.mu[[1]])[i2i.mm[!i2na]], 1, 6))
        
        jj2 <- 1:(which(colSums(wbr.m[[1]])>0)[1]-1)
        tail(jj2)
        colSums(wbr.m[[1]][, max(jj2)+0:3], na.rm=TRUE)
        colSums(wbr.mu[[1]][, max(jj2)+0:3], na.rm=TRUE)

        for (k in 1:2) {
            wbr.m[[k]][!i2na, jj2] <- 
                wbr.mu[[k]][i2i.mm[!i2na], jj2]
        }

    }

    iiuf <- which(is.na(dbrms$codmun) &
                  (dbrms$estado!=''))
    system.time(dbr.uf <- aggregate(
        dbrms[iiuf, c('casosAcumulado', 'obitosAcumulado')],
        dbrms[iiuf, c('fdate', 'estado')], sum))
    head(dbr.uf, 2)
    dbr.uf$estado <- as.character(dbr.uf$estado)

    system.time(
        wbr.uf <- lapply(
            dbr.uf[, c('casosAcumulado', 'obitosAcumulado')], tapply,
            dbr.uf[, c('estado', 'fdate')], as.integer))
    sapply(wbr.uf, dim)

    head(dbrms,2)
    table(substr(dbrms$coduf,1,1))
    table(substr(dbrms$codmun,1,1))
    dbrms$Região <- c("Norte", "Nordeste", "Sudeste", "Sul",
                      "Centro-Oeste", "", "")[
        as.integer(substr(dbrms$coduf,1,1))]

    dbr.r <- aggregate(
        dbrms[is.na(dbrms$codmun),
              c('casosAcumulado', 'obitosAcumulado')],
        dbrms[is.na(dbrms$codmun),
              c('Região', 'fdate')], sum, na.rm=TRUE)

    wbr.r <- lapply(
        dbr.r[c('casosAcumulado', 'obitosAcumulado')], tapply,
        dbr.r[c('Região', 'fdate')], as.integer) 

    sapply(wbr.r, dim)
    wbr.r[[1]][, 1:5]
    wbr.r[[1]][, -3:0+ncol(wbr.r[[1]])]

    sapply(wbr.m, dim)
    sapply(wbr.uf, dim)
    sapply(wbr.r, dim)
    
    sapply(wbr.m, function(x) colSums(x[, -3:0+ncol(x)], na.rm=TRUE))
    sapply(wbr.uf, function(x) colSums(x[, -3:0+ncol(x)], na.rm=TRUE))
    sapply(wbr.r, function(x) colSums(x[, -3:0+ncol(x)], na.rm=TRUE))  

    id.m <- pmatch(rownames(wbr.m[[1]]), dbrms$codmun)
    id.uf <- pmatch(rownames(wbr.uf[[1]]), dbrms$estado)
    id.r <- pmatch(rownames(wbr.r[[1]]), dbrms$Região)

    summary(id.m)
    summary(id.uf)
    summary(id.r)

    tail(wdl[[1]][, 1:9],3)
    tail(dbrms,2)

    for (k in 1:2) {
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(code=rownames(wbr.m[[k]]),
                       City=as.character(dbrms$municipio)[id.m],
                       Province.State=as.character(dbrms$estado)[id.m], 
                       Country.Region='Brasil', Lat=NA, Long=NA,
                       wbr.m[[k]]))
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(code='', 
                       City='', 
                       Province.State=as.character(dbrms$estado)[id.uf], 
                       Country.Region='Brasil', Lat=NA, Long=NA,
                       wbr.uf[[k]]))
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(code='', 
                       City='', 
                       Province.State=rownames(wbr.r[[k]]),
                       Country.Region='Brasil', Lat=NA, Long=NA,
                       wbr.r[[k]]))
    }
    
    
}


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


attr(wdl, 'Sys.time') <- Sys.time()

save('wdl', file='data/wdl.RData')
