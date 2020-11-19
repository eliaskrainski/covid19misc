
if (FALSE) { ## can manually skip

    setwd('..')

}

options(width=70)

if (!any(ls()=='brio'))
    brio <- TRUE

if (!any(ls()=='wcota')) 
    wcota <- !brio

if (!any(ls()=='usems'))
    usems <- FALSE

usefnd <- !usems

Date <- seq(as.Date('20200122', '%Y%m%d'), Sys.Date(), 1)
length(Date)
head(Date)
tail(Date)

alldates <- gsub('-', '', as.character(Date))
head(alldates)
tail(alldates)

source('rcode/getdata.R')

source('rcode/dados-curitiba.R')

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
sapply(wdl, dim)
wdl[[1]][1:3, 1:7]

for (k in 1:2) {
    dtmp <- as.Date(colnames(wdl[[k]])[7:ncol(wdl[[k]])],
                    'X%m.%d.%y')
    nnt <- as.integer(difftime(
        Sys.Date(), tail(dtmp,1), units='days'))
    if (nnt>0)
        for (j in 1:nnt)
            wdl[[k]] <- data.frame(wdl[[k]][, 1:ncol(wdl[[k]])],
                                   new=NA)
}

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

    sapply(wdl, dim)
    
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

r.pt <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")

if (usefnd) {

    system.time(dbrms <-  as.data.frame(
                    readRDS('data/cities.rds')))

    head(dbrms,2)
    summary(dbrms$date)

    dbrms$fdate <- factor(gsub('-', '', as.character(dbrms$date),
                               fixed=TRUE), alldates)

    ynams <- c('accumCases', 'accumDeaths')
    xnams <- c('fdate', 'city_code')
    mnam <- 'city'
    rsnam <- 'healthRegion'
    rscod <- 'healthRegion_code'
    snam <- 'state'
    scod <- 'state_code'

    r.en <- c('North', 'Northeast', 'Southeast', 'South', 'Midwest')
    dbrms$regiao <- factor(dbrms$region, r.en, r.pt)
    rnam <- 'regiao'
    
    iiuf <- which((substring(dbrms[, xnams[2]], 3)=='0000') &
                  (dbrms[,snam]!=''))
    str(iiuf)

}

if (usems & file.exists('data/HIST_PAINEL_COVIDBR.csv')) {
    
    system.time(dbrms <- read.csv2('data/HIST_PAINEL_COVIDBR.csv'))

    summary(dbrms$date <- as.Date(dbrms$data, '%Y-%m-%d'))
    
    dbrms$fdate <- factor(gsub(
        '-', '', dbrms$date, fixed=TRUE), alldates)
    
    ynams <- c('casosAcumulado', 'obitosAcumulado')
    xnams <- c('fdate', 'codmun')
    mnam <- 'municipio'
    rsnam <- 'nomeRegiaoSaude'
    rscod <- 'codRegiaoSaude'
    snam <- 'estado'
    scod <- 'coduf'
    rnam <- 'regiao'
    
    iiuf <- which(is.na(dbrms[xnams[2]])[,1] &
                  (dbrms[snam][,1]!=''))
    str(iiuf)
    
}

if (FALSE) { ### overall look

    library(ggplot2)

    ggplot(dbrms[dbrms$municipio=='',]) +
        geom_point(aes(x=data, y=casosNovos,
                      color=estado, group=estado)) +
        scale_y_log10() + facet_wrap(~regiao)


    dbrms[dbrms$mun=='' & dbrms$data=='2020-11-08',]

    with(dbrms[dbrms$data=='2020-11-08', ],
         tapply(casosAcumulado, list(m=municipio=='',
                                     u=estado), mean, na.rm=TRUE))
    
}


if (usefnd | usems) {

    head(dbrms,2)
    tail(dbrms,2)

    iimm <- which(!is.na(dbrms[xnams[2]]) &
                  (dbrms[,mnam]!=''))
    str(iimm)

    iiuf <- which(nchar(dbrms[, xnams[2]])==6)
    str(iiuf)

    if (TRUE) {
        
        system.time(wbr.m <- lapply(
                        dbrms[iimm, ynams], 
                        tapply,
                        dbrms[iimm, rev(xnams)], as.integer))

        system.time(wbr.u <- lapply(
                        dbrms[iiuf, ynams], 
                        tapply,
                        dbrms[iiuf, c(scod, xnams[1])],
                        sum, na.rm=TRUE))
    
    
    } else {

        system.time(dbr.m <- aggregate(
                        dbrms[iimm, ynams],
                        dbrms[iimm, xnams], sum))
        head(dbr.m, 2)
        
        system.time(wbr.m <- lapply(
                        dbr.m[, ynams], 
                        tapply,
                        dbr.m[, rev(xnams)], as.integer)) 

    }
    
    str(wbr.m)
    wbr.m[[1]][1:5, ncol(wbr.m[[1]])-3:0]
    sapply(wbr.m, function(x) colSums(x[, -2:0+ncol(x)]))

    (dd1 <- as.integer(Sys.Date()-max(dbrms$date)))
    if (as.integer(substr(Sys.time(), 12, 13))>=19) {
        usesesa <- dd1>0         
    } else {
        usesesa <- dd1>1
    }
    usesesa
    
    if (usesesa) {
        
        source('rcode/sesa-prepare.R')
        
        head(rownames(waco.pr[[1]]))
        i2i.mpr <- pmatch(substr(rownames(waco.pr[[1]]), 1, 6),
                          rownames(wbr.m[[1]]))
        summary(i2i.mpr)

        if (FALSE)
            plot(colSums(wbr.m[[1]][i2i.mpr,], na.rm=TRUE),
                 colSums(waco.pr[[1]], na.rm=TRUE))

        tail(colSums(wbr.m[[1]][i2i.mpr,], na.rm=TRUE))
        tail(colSums(waco.pr[[1]], na.rm=TRUE))

        for (k in 1:2) {
            wbr.m[[k]][i2i.mpr, ] <- waco.pr[[k]]
        }

        colSums(wbr.m[[1]][i2i.mpr, -3:0+ncol(wbr.m[[1]])])
        wbr.m[[1]][rownames(wbr.m[[1]])=='410690',
                   -3:0+ncol(wbr.m[[1]])]

    }    

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

        i2i.u <- pmatch(rownames(wbr.u[[1]]),
                        dbrms[pmatch(rownames(wbr.uf[[1]]),
                                     dbrms[, snam]), scod])
        
        for (k in 1:2) {
            wbr.u[[k]][, jj2] <- 
                wbr.uf[[k]][i2i.u, jj2]
        }
                               

    }

##    irs <- pmatch(rownames(wbr.m[[1]]),
  ##                dbrms[, xnams[2]][iimm])
    im.rs <- pmatch(rownames(wbr.m[[1]]),
                    dbrms[iimm, xnams[2]])
    str(im.rs)
    summary(im.rs)

    str(wbr.m)

    accFix <- function(x) {
        x[is.na(x)] <- 0
        cummax(x)         
    }
    
    wbr.m.f <- lapply(wbr.m, function(m) {
        m <- apply(m, 1, accFix)
        return(t(m))
    })
    str(wbr.m.f)

    wbr.rs <- lapply(wbr.m.f, function(m) {
        r <- aggregate(
            m,
            by=list(RS=dbrms[iimm[im.rs],rscod]),
            sum)
        rn <- r[,1]
        r <- as.matrix(r[,-1])
        rownames(r) <- rn
        r
    })
    
    wbr.r <- lapply(wbr.u, function(m) {
        r <- aggregate(
            m,
            by=list(Re=substr(rownames(m),1,1)),
            sum, na.rm=TRUE)
        r <- as.matrix(r[,-1])
        r <- rbind(r, colSums(r))
        rownames(r) <- c(r.pt, '')
        r
    })

    sapply(wbr.m, dim)
    sapply(wbr.rs, dim)
    sapply(wbr.u, dim)
    sapply(wbr.r, dim)

    sapply(wdl, dim)

    sapply(wbr.m, function(x) colSums(x[, -3:0+ncol(x)], na.rm=TRUE))
    sapply(wbr.rs, function(x) colSums(x[, -3:0+ncol(x)], na.rm=TRUE))
    sapply(wbr.u, function(x) colSums(x[, -3:0+ncol(x)], na.rm=TRUE))
    sapply(wbr.r, function(x) colSums(x[, -3:0+ncol(x)], na.rm=TRUE))  

    id.m <- pmatch(rownames(wbr.m[[1]]), dbrms[,xnams[2]])
    id.rs <- pmatch(rownames(wbr.rs[[1]]), dbrms[,rscod])
    id.uf <- pmatch(rownames(wbr.u[[1]]), dbrms[,scod])
    id.r <- pmatch(rownames(wbr.r[[1]]), dbrms[,rnam])

    summary(id.m)
    summary(id.rs)
    summary(id.uf)
    summary(id.r)

    tail(wdl[[1]][, 1:9],3)
    tail(dbrms,2)

    sapply(wdl, dim)
    
    for (k in 1:2) {
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(
                code=rownames(wbr.m[[k]]),
                City=as.character(dbrms[, mnam])[id.m],
                Province.State=as.character(dbrms[, snam])[id.m], 
                Country.Region='Brasil', Lat=NA, Long=NA,
                wbr.m[[k]]))
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(
                code='4104902', 
                City='Curitiba(SM)',
                Province.State='PR', 
                Country.Region='BR', Lat=NA, Long=NA,
                matrix(wcwb[[k]], 1,
                       dimnames=list(NULL, alldates))))
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(
                code=rownames(wbr.rs[[k]]),
                City=paste('Regional',
                           as.character(dbrms[, rsnam])[id.rs]),
                Province.State=as.character(dbrms[, snam])[id.rs], 
                Country.Region='Brasil', Lat=NA, Long=NA,
                wbr.rs[[k]]))
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(
                code='', 
                City='', 
                Province.State=as.character(dbrms[, snam])[id.uf], 
                Country.Region='Brasil', Lat=NA, Long=NA,
                wbr.u[[k]]))
        wdl[[k]] <- rbind(
            wdl[[k]],
            data.frame(
                code='', 
                City='', 
                Province.State=rownames(wbr.r[[k]]),
                Country.Region='Brasil', Lat=NA, Long=NA,
                wbr.r[[k]]))
    }
    
    
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


attr(wdl, 'Sys.time') <- Sys.time()

save('wdl', file='data/wdl.RData')
