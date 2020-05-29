
if (FALSE) { ## can manually skip

    options(width=70)
    setwd('..')

}

naFix <- function(x) {
    x[is.na(x)] <- 0L
    return(x) 
}

source('rcode/getdata.R')

wdl <- list(
    cases=data.frame(
        code='', City='', 
        read.csv('data/confirmed_global.csv')), 
    deaths=data.frame(
        code='', City='',
        read.csv('data/deaths_global.csv'))
)

for (k in 1:2) {
    dtmp <- as.Date(colnames(wdl[[k]])[7:ncol(wdl[[k]])],
                    'X%m.%d.%y')
    if (tail(dtmp, 1)==Sys.Date())
        wdl[[k]] <- wdl[[k]][, 1:(ncol(wdl[[k]])-1)]
}
    
Date <- as.Date(colnames(wdl[[1]])[7:ncol(wdl[[1]])],
                'X%m.%d.%y')
colnames(wdl[[1]])[7:ncol(wdl[[1]])] <-
    colnames(wdl[[2]])[7:ncol(wdl[[2]])] <-
    paste0('X', gsub('-', '', as.character(Date)))


### US data
us.d <- read.csv('data/states_daily_4pm_et.csv')

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
    wdl[[1]] <- rbind(
        wdl[[k]],
        data.frame(code='', City='',
                   Province.State=rownames(w.us[[k]]),
                   Country.Region='US',
                   Lat=as.numeric(NA),
                   Long=as.numeric(NA),
                   w.us[[k]]))
}


### Brasil data
dbr <- read.csv('data/caso.csv.gv')

i.uf <- which(dbr$place_type=='state')
i.mu <- which(dbr$place_type=='city')

### Identifica as colunas (y) e (x)
dbr$chdate <- gsub('-', '', dbr$date, fixed=TRUE)

jj.y  <- match(c('confirmed', 'deaths'),
               colnames(dbr))
jj.x <- match(c('city_ibge_code', 'chdate'),
              colnames(dbr))
jj.x.uf <- match(c('state', 'chdate'),
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

aux <- matrix(0L, nrow(wbr.mu[[1]]), ncol(w.us[[1]])-ncol(wbr.mu[[1]]))
colnames(aux) <- colnames(w.us[[1]])[1:ncol(aux)]

munam <- dbr$city[pmatch(rownames(wbr.mu[[1]]),
                         dbr$city_ibge_code)]

for (k in 1:2) {
    wdl[[k]] <- rbind(
        wdl[[k]],
        data.frame(code='', City='',
                   Province.State=rownames(wbr.uf[[k]]),
                   Country.Region='Brazil', Lat=NA, Long=NA,
                   aux[1:27, ], wbr.uf[[k]]))
    wdl[[k]] <- rbind(
        wdl[[k]],
        data.frame(code=rownames(wbr.mu[[k]]),
                   City=munam,
                   Province.State='', 
                   Country.Region='Brazil', Lat=NA, Long=NA,
                   aux, wbr.mu[[k]]))
    rownames(wdl[[k]]) <- 1:nrow(wdl[[k]])
    wdl[[k]] <- as.data.frame(wdl[[k]])
    for (j in 7:ncol(wdl[[k]]))
        wdl[[k]][, j] <- as.integer(wdl[[k]][,j])
}

save('wdl', file='data/wdl.RData')
