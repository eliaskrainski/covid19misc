
if (FALSE) { ## can manually skip

    options(width=70)
    setwd('..')

}

naFix <- function(x) {
    x[is.na(x)] <- 0L
    return(x) 
}

if (!any(ls()=='brio'))
    brio <- TRUE 

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

for (k in 1:2) {
    dtmp <- as.Date(colnames(wdl[[k]])[7:ncol(wdl[[k]])],
                    'X%m.%d.%y')
    if (tail(dtmp, 1)==Sys.Date())
        wdl[[k]] <- wdl[[k]][, 1:(ncol(wdl[[k]])-1)]
}

Date <- as.Date(colnames(wdl[[1]])[7:ncol(wdl[[1]])],
                'X%m.%d.%y')
alldates <- gsub('-', '', as.character(Date))

colnames(wdl[[1]])[7:ncol(wdl[[1]])] <-
    colnames(wdl[[2]])[7:ncol(wdl[[2]])] <-
    paste0('X', gsub('-', '', as.character(Date)))


### US data
us.d <- read.csv('data/states_daily_4pm_et.csv')

us.d$date <- factor(us.d$date, alldates)

w.us <- lapply(us.d[c('positive', 'death')], tapply, 
               us.d[c('state', 'date')], as.integer)

for (k in 1:2) {
    dtmp <- as.Date(colnames(w.us[[k]]), '%Y%m%d')
    if (tail(dtmp, 1)==Sys.Date())
        w.us[[k]] <- w.us[[k]][, 1:(ncol(w.us[[k]])-1)]
}
object.size(w.us)

for (k in 1:2) {
    w.us[[k]][is.na(w.us[[k]])] <- 0L
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
    dbr <- read.csv('data/caso.csv.gv')
}  else {
### data from Wesley Cota 
    dbr <- read.csv('data/cases-brazil-cities-time.csv')
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
    
    for (k in 1:2) {
        wbr.uf[[k]][is.na(wbr.uf[[k]])] <- 0L
        dtmp <- as.Date(colnames(wbr.mu[[k]]), '%Y%m%d')
        if (tail(dtmp, 1)==Sys.Date())
            wbr.mu[[k]] <- wbr.mu[[k]][, 1:(ncol(wbr.mu[[k]])-1)]
        wbr.mu[[k]][is.na(wbr.mu[[k]])] <- 0L
        dtmp <- as.Date(colnames(wbr.uf[[k]]), '%Y%m%d')
        if (tail(dtmp, 1)==Sys.Date())
            wbr.uf[[k]] <- wbr.uf[[k]][, 1:(ncol(wbr.uf[[k]])-1)]
    }
    
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
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(code='1', City='total', Province.State='total', 
                       Country.Region='Brasil (total)',
                       Lat=NA, Long=NA,
                       matrix(colSums(wbr.uf[[k]][,-1]), 1,
                              dimnames=list(
                                  '', colnames(wbr.uf[[k]])[-1]))))
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(code=wbr.uf[[k]]$code, City='',
                       Province.State=stnam.uf, 
                       Country.Region='Brazil', Lat=NA, Long=NA,
                       as.matrix(wbr.uf[[k]][,-1])))
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(code=rownames(wbr[[k]])[iwm],
                       City=munam,
                       Province.State=stnam.mu, 
                       Country.Region='Brazil', Lat=NA, Long=NA,
                       wbr[[k]][iwm,]))
    }
}


for (k in 1:2) {
    rownames(wdl[[k]]) <- 1:nrow(wdl[[k]])
    wdl[[k]] <- as.data.frame(wdl[[k]])
    for (j in 7:ncol(wdl[[k]])) {
        y <- as.integer(wdl[[k]][,j])
        y[y==0] <- NA
        wdl[[k]][, j] <- y
    }    
}

save('wdl', file='data/wdl.RData')
